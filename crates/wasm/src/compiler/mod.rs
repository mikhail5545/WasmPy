/*
 * Copyright (c) 2026. Mikhail Kulik.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Static WASM code generator.
//!
//! Compiles a type-checked AST module into a valid WASM binary using `wasm-encoder`.
//! Assumes that the AST has already been validated by the type checker, so all
//! type annotations are present and correct.

pub mod op;

use ast::expr::Expr;
use ast::op::*;
use ast::stmt::Stmt;
use ast::types::TypeHint;
use ast::{Module, Param, Spanned};
use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, FunctionSection,
    Instruction, Module as WasmModule, TypeSection, ValType,
};

use crate::functions::FunctionContext;
use crate::types::WasmType;

/// Metadata for a compiled function (used for call resolution).
#[derive(Debug, Clone)]
struct FuncMeta {
    /// Index in the WASM function index space
    func_index: u32,
    /// Type index in the WASM type section (reserved for future use)
    #[allow(dead_code)]
    type_index: u32,
    /// Parameter types
    param_types: Vec<WasmType>,
    /// Return type
    return_type: WasmType,
}

/// The static WASM compiler. Compiles a typed AST into a WASM binary module.
///
/// ## Compilation strategy
///
/// - Top-level module code is compiled into an exported `_start` function.
/// - Each `def` statement produces a separate WASM function.
/// - All functions are exported by name.
/// - Python `int` → i64, `float` → f64, `bool` → i32.
pub struct Compiler {
    type_section: TypeSection,
    function_section: FunctionSection,
    export_section: ExportSection,
    code_section: CodeSection,

    /// Function name → metadata (for call resolution)
    func_table: HashMap<String, FuncMeta>,
    /// Next function index in the WASM module
    next_func_index: u32,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            type_section: TypeSection::new(),
            function_section: FunctionSection::new(),
            export_section: ExportSection::new(),
            code_section: CodeSection::new(),
            func_table: HashMap::new(),
            next_func_index: 0,
        }
    }

    /// Compile an entire module. This is the main entry point.
    ///
    /// Pass 1: Register all function signatures into the type section.
    /// Pass 2: Compile all function bodies.
    /// Pass 3: Compile top-level code into `_start`.
    pub fn compile_module(&mut self, module: &Module) {
        // ── Pass 1: Register function signatures ──
        for stmt in &module.body {
            if let Stmt::FuncDef {
                name,
                params,
                return_type_hint,
                ..
            } = &stmt.node
            {
                self.register_function(name, params, return_type_hint);
            }
        }

        // ── Pass 2: Compile function bodies ──
        for stmt in &module.body {
            if let Stmt::FuncDef {
                name,
                params,
                body,
                return_type_hint,
                ..
            } = &stmt.node
            {
                self.compile_func_body(name, params, body, return_type_hint);
            }
        }

        // ── Pass 3: Compile top-level code into _start ──
        self.compile_start(module);
    }

    /// Finalize and produce the WASM binary.
    pub fn finish(self) -> Vec<u8> {
        let mut wasm_module = WasmModule::new();
        wasm_module.section(&self.type_section);
        wasm_module.section(&self.function_section);
        wasm_module.section(&self.export_section);
        wasm_module.section(&self.code_section);
        wasm_module.finish()
    }

    // ─── Function registration ───────────────────────────────────────────

    fn register_function(
        &mut self,
        name: &str,
        params: &[Param],
        return_type_hint: &Option<Spanned<TypeHint>>,
    ) {
        let param_types: Vec<WasmType> = params
            .iter()
            .filter_map(|p| {
                p.type_hint
                    .as_ref()
                    .and_then(|h| WasmType::from_type_hint(&h.node))
            })
            .collect();

        let return_type = return_type_hint
            .as_ref()
            .and_then(|h| WasmType::from_type_hint(&h.node))
            .unwrap_or(WasmType::Void);

        let wasm_params: Vec<ValType> = param_types
            .iter()
            .filter_map(|t| t.to_val_type())
            .collect();

        let wasm_results: Vec<ValType> = return_type.to_val_type().into_iter().collect();

        let type_index = self.next_func_index; // type and func indices align 1:1
        self.type_section
            .ty()
            .function(wasm_params, wasm_results);
        self.function_section.function(type_index);
        self.export_section
            .export(name, ExportKind::Func, self.next_func_index);

        self.func_table.insert(
            name.to_string(),
            FuncMeta {
                func_index: self.next_func_index,
                type_index,
                param_types,
                return_type,
            },
        );

        self.next_func_index += 1;
    }

    // ─── Top-level _start ────────────────────────────────────────────────

    fn compile_start(&mut self, module: &Module) {
        // Check if there are any top-level statements (besides FuncDef)
        let has_top_level = module.body.iter().any(|s| !matches!(s.node, Stmt::FuncDef { .. }));
        if !has_top_level {
            return;
        }

        // Register _start type: () → ()
        let type_index = self.next_func_index;
        self.type_section.ty().function(vec![], vec![]);
        self.function_section.function(type_index);
        self.export_section
            .export("_start", ExportKind::Func, self.next_func_index);
        self.next_func_index += 1;

        let mut ctx = FunctionContext::new_start();

        for stmt in &module.body {
            if !matches!(stmt.node, Stmt::FuncDef { .. }) {
                self.compile_stmt(&stmt.node, &mut ctx);
            }
        }

        ctx.emit_end();
        let func = ctx.build();
        self.code_section.function(&func);
    }

    // ─── Function body compilation ───────────────────────────────────────

    fn compile_func_body(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Spanned<Stmt>],
        _return_type_hint: &Option<Spanned<TypeHint>>,
    ) {
        let meta = self.func_table.get(name).cloned().unwrap();

        let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();

        let mut ctx = FunctionContext::new(&param_names, &meta.param_types);

        for stmt in body {
            self.compile_stmt(&stmt.node, &mut ctx);
        }

        // If the function returns void (None), ensure we don't leave anything dangling
        if meta.return_type == WasmType::Void {
            // An implicit return at the end (no value)
        }

        ctx.emit_end();
        let func = ctx.build();
        self.code_section.function(&func);
    }

    // ─── Statement compilation ───────────────────────────────────────────

    fn compile_stmt(&self, stmt: &Stmt, ctx: &mut FunctionContext) {
        match stmt {
            Stmt::Expr(expr) => {
                let ty = self.compile_expr(&expr.node, ctx);
                // Drop the value from the stack if it's not void
                if ty != WasmType::Void {
                    ctx.emit(&Instruction::Drop);
                }
            }

            Stmt::Assign {
                targets,
                value,
                type_hint,
            } => {
                self.compile_assign(targets, value, type_hint, ctx);
            }

            Stmt::AugAssign {
                target, op, value, ..
            } => {
                self.compile_aug_assign(target, op, value, ctx);
            }

            Stmt::Return(Some(expr)) => {
                self.compile_expr(&expr.node, ctx);
                ctx.emit(&Instruction::Return);
            }

            Stmt::Return(None) => {
                ctx.emit(&Instruction::Return);
            }

            Stmt::If {
                test,
                body,
                elif_clauses,
                else_body,
            } => {
                self.compile_if(test, body, elif_clauses, else_body, ctx);
            }

            Stmt::While {
                test,
                body,
                else_body,
            } => {
                self.compile_while(test, body, else_body, ctx);
            }

            Stmt::Pass => {}

            Stmt::Break => {
                // In our while compilation pattern, the outer block is at depth 1
                ctx.emit(&Instruction::Br(1));
            }

            Stmt::Continue => {
                // In our while compilation pattern, the loop is at depth 0
                ctx.emit(&Instruction::Br(0));
            }

            Stmt::FuncDef { .. } => {
                // Already handled in pass 2
            }

            Stmt::Global(_) => {
                // No-op for WASM (all variables are locals within functions)
            }

            _ => {
                // Unsupported statement types should have been caught by the type checker
            }
        }
    }

    fn compile_assign(
        &self,
        targets: &[Spanned<Expr>],
        value: &Spanned<Expr>,
        type_hint: &Option<Spanned<TypeHint>>,
        ctx: &mut FunctionContext,
    ) {
        let val_type = self.compile_expr(&value.node, ctx);

        for target in targets {
            if let Expr::Name(name) = &target.node {
                let wasm_type = if let Some(hint) = type_hint {
                    WasmType::from_type_hint(&hint.node).unwrap_or(val_type)
                } else {
                    val_type
                };

                let idx = ctx.declare_local(name.clone(), wasm_type);

                // If the value type doesn't match the declared type, emit a promotion
                self.emit_promotion(ctx, val_type, wasm_type);

                ctx.emit(&Instruction::LocalSet(idx));
            }
        }
    }

    fn compile_aug_assign(
        &self,
        target: &Spanned<Expr>,
        op: &AugOp,
        value: &Spanned<Expr>,
        ctx: &mut FunctionContext,
    ) {
        if let Expr::Name(name) = &target.node {
            let idx = ctx.get_local(name).unwrap();
            let var_type = ctx.get_local_type(name).unwrap();

            // Load current value
            ctx.emit(&Instruction::LocalGet(idx));

            // Compile the RHS value
            let val_type = self.compile_expr(&value.node, ctx);

            // Promote RHS if needed
            self.emit_promotion(ctx, val_type, var_type);

            // Emit the operation
            match var_type {
                WasmType::I64 => match op {
                    AugOp::Add => ctx.emit(&Instruction::I64Add),
                    AugOp::Sub => ctx.emit(&Instruction::I64Sub),
                    AugOp::Mul => ctx.emit(&Instruction::I64Mul),
                    AugOp::Div => ctx.emit(&Instruction::I64DivS),
                    AugOp::FloorDiv => ctx.emit(&Instruction::I64DivS),
                    AugOp::Mod => ctx.emit(&Instruction::I64RemS),
                    AugOp::BitAnd => ctx.emit(&Instruction::I64And),
                    AugOp::BitOr => ctx.emit(&Instruction::I64Or),
                    AugOp::BitXor => ctx.emit(&Instruction::I64Xor),
                    AugOp::LShift => ctx.emit(&Instruction::I64Shl),
                    AugOp::RShift => ctx.emit(&Instruction::I64ShrS),
                    _ => {}
                },
                WasmType::F64 => match op {
                    AugOp::Add => ctx.emit(&Instruction::F64Add),
                    AugOp::Sub => ctx.emit(&Instruction::F64Sub),
                    AugOp::Mul => ctx.emit(&Instruction::F64Mul),
                    AugOp::Div => ctx.emit(&Instruction::F64Div),
                    _ => {}
                },
                _ => {}
            }

            ctx.emit(&Instruction::LocalSet(idx));
        }
    }

    // ─── Expression compilation ──────────────────────────────────────────

    /// Compile an expression, leaving its result on the WASM operand stack.
    /// Returns the WasmType of the result.
    fn compile_expr(&self, expr: &Expr, ctx: &mut FunctionContext) -> WasmType {
        match expr {
            Expr::Number(text) => {
                if text.contains('.') || text.contains('e') || text.contains('E') {
                    let val: f64 = text.parse().unwrap_or(0.0);
                    ctx.emit(&Instruction::F64Const(val.into()));
                    WasmType::F64
                } else {
                    let val: i64 = text.parse().unwrap_or(0);
                    ctx.emit(&Instruction::I64Const(val));
                    WasmType::I64
                }
            }

            Expr::Bool(b) => {
                ctx.emit(&Instruction::I32Const(if *b { 1 } else { 0 }));
                WasmType::I32
            }

            Expr::NoneLit => {
                // None doesn't push anything onto the stack
                WasmType::Void
            }

            Expr::Name(name) => {
                let idx = ctx.get_local(name).unwrap();
                let ty = ctx.get_local_type(name).unwrap();
                ctx.emit(&Instruction::LocalGet(idx));
                ty
            }

            Expr::BinOp { left, op, right } => {
                self.compile_bin_op(ctx, op, left, right)
            }

            Expr::UnaryOp { op, operand } => {
                self.compile_unary_op(ctx, op, operand)
            }

            Expr::Compare {
                left,
                ops,
                comparators,
            } => self.compile_compare(left, ops, comparators, ctx),

            Expr::IfExpr { test, body, orelse } => {
                self.compile_if_expr(test, body, orelse, ctx)
            }

            Expr::Call { func, args, .. } => {
                self.compile_call(func, args, ctx)
            }

            _ => WasmType::Void,
        }
    }

    fn compile_if_expr(
        &self,
        test: &Spanned<Expr>,
        body: &Spanned<Expr>,
        orelse: &Spanned<Expr>,
        ctx: &mut FunctionContext,
    ) -> WasmType {
        self.compile_expr(&test.node, ctx);

        // We need to know the result type to emit the correct BlockType.
        // Since the type checker has validated both branches have the same type,
        // we can peek at the body type. We'll use a simple heuristic based on the expression.
        let result_val_type = self.peek_expr_type(&body.node, ctx);
        let block_type = match result_val_type.to_val_type() {
            Some(vt) => BlockType::Result(vt),
            None => BlockType::Empty,
        };

        ctx.emit(&Instruction::If(block_type));
        self.compile_expr(&body.node, ctx);
        ctx.emit(&Instruction::Else);
        self.compile_expr(&orelse.node, ctx);
        ctx.emit(&Instruction::End);

        result_val_type
    }

    fn compile_call(
        &self,
        func: &Spanned<Expr>,
        args: &[Spanned<Expr>],
        ctx: &mut FunctionContext,
    ) -> WasmType {
        if let Expr::Name(name) = &func.node {
            let meta = self.func_table.get(name).cloned().unwrap();

            // Compile arguments, promoting types as needed
            for (i, arg) in args.iter().enumerate() {
                let arg_type = self.compile_expr(&arg.node, ctx);
                let expected = meta.param_types[i];
                self.emit_promotion(ctx, arg_type, expected);
            }

            ctx.emit(&Instruction::Call(meta.func_index));
            meta.return_type
        } else {
            WasmType::Void
        }
    }

    // ─── Control flow ────────────────────────────────────────────────────

    fn compile_if(
        &self,
        test: &Spanned<Expr>,
        body: &[Spanned<Stmt>],
        elif_clauses: &[(Spanned<Expr>, Vec<Spanned<Stmt>>)],
        else_body: &Option<Vec<Spanned<Stmt>>>,
        ctx: &mut FunctionContext,
    ) {
        self.compile_expr(&test.node, ctx);
        ctx.emit(&Instruction::If(BlockType::Empty));

        for stmt in body {
            self.compile_stmt(&stmt.node, ctx);
        }

        if !elif_clauses.is_empty() || else_body.is_some() {
            ctx.emit(&Instruction::Else);

            // Elif chains become nested if/else
            let mut remaining_elifs = elif_clauses.iter().peekable();
            if let Some((elif_test, elif_body)) = remaining_elifs.next() {
                self.compile_if(
                    elif_test,
                    elif_body,
                    &elif_clauses[1..].to_vec(),
                    else_body,
                    ctx,
                );
            } else if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    self.compile_stmt(&stmt.node, ctx);
                }
            }
        }

        ctx.emit(&Instruction::End);
    }

    fn compile_while(
        &self,
        test: &Spanned<Expr>,
        body: &[Spanned<Stmt>],
        _else_body: &Option<Vec<Spanned<Stmt>>>,
        ctx: &mut FunctionContext,
    ) {
        // WASM pattern for while loops:
        // block $exit
        //   loop $loop
        //     <test>
        //     i32.eqz
        //     br_if $exit    ;; if test is false, break out
        //     <body>
        //     br $loop       ;; continue loop
        //   end
        // end
        ctx.emit(&Instruction::Block(BlockType::Empty));
        ctx.emit(&Instruction::Loop(BlockType::Empty));

        // Compile test
        self.compile_expr(&test.node, ctx);
        ctx.emit(&Instruction::I32Eqz);
        ctx.emit(&Instruction::BrIf(1)); // break to outer block if test is false

        // Compile body
        for stmt in body {
            self.compile_stmt(&stmt.node, ctx);
        }

        ctx.emit(&Instruction::Br(0)); // jump back to loop start
        ctx.emit(&Instruction::End); // end loop
        ctx.emit(&Instruction::End); // end block
    }

    // ─── Helpers ─────────────────────────────────────────────────────────

    /// Emit promotion instructions to convert `actual` type to `expected` type.
    /// The value to promote is assumed to be on top of the WASM stack.
    fn emit_promotion(&self, ctx: &mut FunctionContext, actual: WasmType, expected: WasmType) {
        if actual == expected {
            return;
        }
        match (actual, expected) {
            (WasmType::I32, WasmType::I64) => {
                ctx.emit(&Instruction::I64ExtendI32S);
            }
            (WasmType::I32, WasmType::F64) => {
                ctx.emit(&Instruction::F64ConvertI32S);
            }
            (WasmType::I64, WasmType::F64) => {
                ctx.emit(&Instruction::F64ConvertI64S);
            }
            _ => {
                // No conversion needed or unsupported
            }
        }
    }

    /// Peek at what type an expression would produce (without emitting code).
    /// Used for BlockType decisions in if-expressions.
    fn peek_expr_type(&self, expr: &Expr, ctx: &FunctionContext) -> WasmType {
        match expr {
            Expr::Number(text) => {
                if text.contains('.') || text.contains('e') || text.contains('E') {
                    WasmType::F64
                } else {
                    WasmType::I64
                }
            }
            Expr::Bool(_) => WasmType::I32,
            Expr::NoneLit => WasmType::Void,
            Expr::Name(name) => ctx.get_local_type(name).unwrap_or(WasmType::Void),
            Expr::BinOp { .. } => {
                // Approximate: could be more precise
                WasmType::I64
            }
            Expr::Compare { .. } => WasmType::I32,
            Expr::Call { func, .. } => {
                if let Expr::Name(name) = &func.node {
                    self.func_table
                        .get(name)
                        .map(|m| m.return_type)
                        .unwrap_or(WasmType::Void)
                } else {
                    WasmType::Void
                }
            }
            _ => WasmType::Void,
        }
    }
}


