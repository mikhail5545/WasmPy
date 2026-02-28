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
pub mod op_dispatch;
pub mod helpers;
mod control_flow;
pub mod literal;
pub mod data_access;

use ast::expr::Expr;
use ast::op::*;
use ast::stmt::Stmt;
use ast::types::TypeHint;
use ast::{Module, Param, Spanned};
use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, FunctionSection,
    Instruction, Module as WasmModule, TypeSection, ValType, MemorySection,
    MemoryType, GlobalSection, GlobalType, ConstExpr, ArrayType, StructType,
    FieldType, StorageType, RefType, HeapType, DataSection,
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
    /// Function section of a module defines a list of function signatures (type indices) that correspond 1:1 with the code section's function bodies. They have their own local section
    /// https://webassembly.github.io/spec/core/syntax/modules.html#function-section
    ///
    /// Note: the function section only lists type indices, while the actual function bodies (with their own local declarations) are defined in the code section.
    /// This separation allows for more flexible function definitions and better code organization.
    function_section: FunctionSection,
    /// Export section of a module defines a set of exports that become accessible to the host environment when the module is instantiated.
    /// https://webassembly.github.io/spec/core/syntax/modules.html#exports
    export_section: ExportSection,
    /// Code section decodes the list of code entries that are pairs of lists of locals and expressions. They represent the bodies of functions defined by a module.
    /// https://webassembly.github.io/spec/core/syntax/modules.html#code-section
    code_section: CodeSection,
    data_section: DataSection,
    /// Function name → metadata (for call resolution)
    func_table: HashMap<String, FuncMeta>,
    /// Next function index in the WASM module
    next_func_index: u32,
    /// Pre-registered type indices for common types, used for calls to initialize objects in type section (e.g. `struct.new $index`)
    string_type_index: u32,
    i64_array_type_index: u32,
    ref_array_type_index: u32,
    i64_tuple_type_index: u32,
    next_data_index: u32,
    current_context: Option<FunctionContext>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut type_section = TypeSection::new();
        type_section.ty().array(&StorageType::I8, true);
        type_section.ty().array(&StorageType::Val(ValType::I64), true);
        type_section.ty().array(&StorageType::Val(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::ANY,
        })), true);
        type_section.ty().array(&StorageType::Val(ValType::I64), true);
        let string_type_index = 0; // First type is our string struct
        let i64_array_type_index = 1; // Second type is our i64 array
        let ref_array_type_index = 2;
        let i64_tuple_type_index = 3; // Third type is our i64 tuple (for future use)
        
        Self {
            type_section,
            function_section: FunctionSection::new(),
            export_section: ExportSection::new(),
            code_section: CodeSection::new(),
            func_table: HashMap::new(),
            data_section: DataSection::new(),
            string_type_index,
            i64_array_type_index,
            ref_array_type_index,
            i64_tuple_type_index,
            next_data_index: 0,
            next_func_index: 0,
            current_context: None,
        }
    }

    fn ctx(&mut self) -> &mut FunctionContext{
        self.current_context.as_mut().expect("No active function context")
    }

    /// Compile an entire module. This is the main entry point.
    ///
    /// Pass 1: Register all function signatures into the type section.
    /// Pass 2: Compile all function bodies.
    /// Pass 3: Compile top-level code into `_start`.
    ///
    /// `module ::= module type* import* tag* global* mem* table* func* data* elem* start? export*`
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

    fn register_function(&mut self, name: &str, params: &[Param], return_type_hint: &Option<Spanned<TypeHint>>) {
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
            .filter_map(|t| t.to_val_type(self.string_type_index, self.i64_array_type_index))
            .collect();

        let wasm_results: Vec<ValType> = return_type.to_val_type(self.string_type_index, self.i64_array_type_index).into_iter().collect();


        self.type_section.ty().function(wasm_params, wasm_results);
        let type_index = self.type_section.len() - 1; // Get the index of the type we just added, since we have custom GC types and indices are not guaranteed to be sequential
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

    /// Defines index of the `_start` function, which is automatically invoked when the module is instantiated after tables and memories are set up.
    /// see [`WASM spec on start function`] for details.
    ///
    /// [`WASM spec on start function`]: https://webassembly.github.io/spec/core/syntax/modules.html#start-function
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

        self.current_context = Some(FunctionContext::new_start(self.string_type_index, self.i64_array_type_index));

        for stmt in &module.body {
            if !matches!(stmt.node, Stmt::FuncDef { .. }) {
                self.compile_stmt(&stmt.node);
            }
        }

        self.ctx().emit_end();
        let func = self.ctx().build();
        self.code_section.function(&func);
    }

    fn compile_func_body(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Spanned<Stmt>],
        _return_type_hint: &Option<Spanned<TypeHint>>,
    ) {
        let meta = self.func_table.get(name).cloned().unwrap();

        let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();

        self.current_context = Some(FunctionContext::new(&param_names, &meta.param_types, self.string_type_index, self.i64_array_type_index));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        // If the function returns void (None), ensure we don't leave anything dangling
        if meta.return_type == WasmType::Void {
            // An implicit return at the end (no value)
        }

        self.ctx().emit_end();
        let func = self.ctx().build();
        self.code_section.function(&func);
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                let ty = self.compile_expr(&expr.node);
                // Drop the value from the stack if it's not void
                if ty != WasmType::Void {
                    self.ctx().emit(&Instruction::Drop);
                }
            }

            Stmt::Assign {
                targets,
                value,
                type_hint,
            } => {
                self.compile_assign(targets, value, type_hint);
            }

            Stmt::AugAssign {
                target, op, value, ..
            } => {
                self.compile_aug_assign(target, op, value);
            }

            Stmt::Return(Some(expr)) => {
                self.compile_expr(&expr.node);
                self.ctx().emit(&Instruction::Return);
            }

            Stmt::Return(None) => {
                self.ctx().emit(&Instruction::Return);
            }

            Stmt::If {
                test,
                body,
                elif_clauses,
                else_body,
            } => {
                self.compile_if(test, body, elif_clauses, else_body);
            }

            Stmt::While {
                test,
                body,
                else_body,
            } => {
                self.compile_while(test, body, else_body);
            }

            Stmt::Pass => {}

            Stmt::Break => {
                // In our while compilation pattern, the outer block is at depth 1
                self.ctx().emit(&Instruction::Br(1));
            }

            Stmt::Continue => {
                // In our while compilation pattern, the loop is at depth 0
                self.ctx().emit(&Instruction::Br(0));
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

    fn compile_assign(&mut self, targets: &[Spanned<Expr>], value: &Spanned<Expr>, type_hint: &Option<Spanned<TypeHint>>) {
        let val_type = self.compile_expr(&value.node);

        for target in targets {
            if let Expr::Name(name) = &target.node {
                let wasm_type = if let Some(hint) = type_hint {
                    WasmType::from_type_hint(&hint.node).unwrap_or(val_type)
                } else {
                    val_type
                };

                let idx = self.ctx().declare_local(name.clone(), wasm_type);

                // If the value type doesn't match the declared type, emit a promotion
                self.emit_promotion(val_type, wasm_type);

                self.ctx().emit(&Instruction::LocalSet(idx));
            }
        }
    }

    fn compile_aug_assign(&mut self, target: &Spanned<Expr>, op: &AugOp, value: &Spanned<Expr>) {
        if let Expr::Name(name) = &target.node {
            let idx = self.ctx().get_local(name).unwrap();
            let var_type = self.ctx().get_local_type(name).unwrap();

            // Load current value
            self.ctx().emit(&Instruction::LocalGet(idx));

            // Compile the RHS value
            let val_type = self.compile_expr(&value.node);

            // Promote RHS if needed
            self.emit_promotion(val_type, var_type);

            // Emit the operation
            self.emit_aug_assign_dispatch(val_type, op);

            self.ctx().emit(&Instruction::LocalSet(idx));
        }
    }

    /// Compile an expression, leaving its result on the WASM operand stack.
    /// Returns the WasmType of the result.
    fn compile_expr(&mut self, expr: &Expr) -> WasmType {
        match expr {
            Expr::Number(text) => {
                if Self::is_floating_point(text) {
                    let val: f64 = text.parse().unwrap_or(0.0);
                    self.ctx().emit(&Instruction::F64Const(val.into()));
                    WasmType::F64
                } else {
                    let val: i64 = text.parse().unwrap_or(0);
                    self.ctx().emit(&Instruction::I64Const(val));
                    WasmType::I64
                }
            }

            Expr::Bool(b) => {
                self.ctx().emit(&Instruction::I32Const(if *b { 1 } else { 0 }));
                WasmType::I32
            }

            Expr::NoneLit => {
                // None doesn't push anything onto the stack
                WasmType::Void
            }

            Expr::Name(name) => {
                let idx = self.ctx().get_local(name).unwrap();
                let ty = self.ctx().get_local_type(name).unwrap();
                self.ctx().emit(&Instruction::LocalGet(idx));
                ty
            }

            Expr::BinOp { left, op, right } => {
                self.compile_bin_op(op, left, right)
            }

            Expr::UnaryOp { op, operand } => {
                self.compile_unary_op(op, operand)
            }

            Expr::Compare {
                left,
                ops,
                comparators,
            } => self.compile_compare(left, ops, comparators),

            Expr::IfExpr { test, body, orelse } => {
                self.compile_if_expr(test, body, orelse)
            }

            Expr::Call { func, args, .. } => {
                self.compile_call(func, args)
            }

            Expr::List(args) => {
                self.compile_list_literal(args)
            }

            Expr::StringLit(s) => {
                self.compile_string_literal(s)
            }

            _ => WasmType::Void,
        }
    }

    fn compile_if_expr(&mut self, test: &Spanned<Expr>, body: &Spanned<Expr>, orelse: &Spanned<Expr>) -> WasmType {
        self.compile_expr(&test.node);

        // We need to know the result type to emit the correct BlockType.
        // Since the type checker has validated both branches have the same type,
        // we can peek at the body type. We'll use a simple heuristic based on the expression.
        let result_val_type = self.peek_expr_type(&body.node);
        let block_type = match result_val_type.to_val_type(self.string_type_index, self.i64_array_type_index) {
            Some(vt) => BlockType::Result(vt),
            None => BlockType::Empty,
        };

        self.ctx().emit(&Instruction::If(block_type));
        self.compile_expr(&body.node);
        self.ctx().emit(&Instruction::Else);
        self.compile_expr(&orelse.node);
        self.ctx().emit(&Instruction::End);

        result_val_type
    }

    fn compile_call(&mut self, func: &Spanned<Expr>, args: &[Spanned<Expr>]) -> WasmType {
        if let Expr::Name(name) = &func.node {
            let meta = self.func_table.get(name).cloned().unwrap();

            // Compile arguments, promoting types as needed
            for (i, arg) in args.iter().enumerate() {
                let arg_type = self.compile_expr(&arg.node);
                let expected = meta.param_types[i];
                self.emit_promotion(arg_type, expected);
            }

            self.ctx().emit(&Instruction::Call(meta.func_index));
            meta.return_type
        } else {
            WasmType::Void
        }
    }
}


