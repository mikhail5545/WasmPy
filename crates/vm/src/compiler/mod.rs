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
mod translate;
mod utils;

use ast::{Module, Spanned, Param};
use ast::expr::Expr;
use ast::stmt::Stmt;
use ast::op::*;
use crate::instruction::*;
use std::collections::HashSet;

/// Represents a standalone chunk of bytecode, such as a function body or the main module body. Contains the instructions and metadata needed to execute it.
#[derive(Debug, Clone)]
pub struct CodeObject {
    pub instructions: Vec<Instruction>,
    pub register_count: u8,
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeType {
    Module,
    Function,
}

/// Compilation context of a specific scope (e.g. function body, module body). Contains the instructions generated so far and the next available register index.
/// This is used to compile statements and expressions within that scope, and will eventually be turned into a CodeObject when compilation is finished.
struct Context {
    instructions: Vec<Instruction>,
    next_reg: u8,
    /// Tracks variable names that are local to this scope (e.g. function arguments)
    locals: HashSet<String>,
    /// Names explicitly declared as global (via `global` keyword)
    explicit_globals: HashSet<String>,
    /// Explicit differentiation between module-level and function-level contexts,
    /// which may have different rules for variable resolution and assignment.
    scope_type: ScopeType,
}

impl Context {
    pub fn new(scope_type: ScopeType) -> Self{
        Self{
            instructions: Vec::new(),
            next_reg: 0,
            locals: HashSet::new(),
            explicit_globals: HashSet::new(),
            scope_type,
        }
    }

    fn alloc_reg(&mut self) -> u8 {
        let r = self.next_reg;
        self.next_reg += 1;
        assert!(self.next_reg < 255, "Too many registers allocated in this context");
        r
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

// TODO: implement registers per-scope, call frame stack with saved ip, register windows, and local scopes to allow for function calls and recursion. For now, we will just use a single global register space and no call frames.
pub struct Compiler {
    current_context: Context,
    functions: Vec<CodeObject>, // function table for storing compiled functions, indexed by Function(usize) in Value
}

/// The main entry point for the compiler. Takes a parsed AST module and compiles it into a CompilationResult containing the main code object and any function code objects.
pub struct CompilationResult {
    /// The top-level code (body)
    pub main: CodeObject,
    /// All defined functions, indexed by their Function(usize) value in the constant pool
    pub functions: Vec<CodeObject>,
}

impl Compiler {
    pub fn new() -> Self{
        Self{
            current_context: Context::new(ScopeType::Module),
            functions: Vec::new(),
        }
    }

    /// Proxies emission to the current context
    fn emit(&mut self, instruction: Instruction) {
        self.current_context.emit(instruction);
    }

    /// Proxies register allocation to the current context
    fn alloc_reg(&mut self) -> u8 {
        self.current_context.alloc_reg()
    }

    /// Finalizes the compilation process by emitting a `Halt` instruction and constructing the final `CompilationResult`
    pub fn finish(mut self) -> CompilationResult {
        self.emit(Instruction::Halt);
        CompilationResult{
            main: CodeObject{
                instructions: self.current_context.instructions,
                register_count: self.current_context.next_reg,
            },
            functions: self.functions,
        }
    }

    pub fn compile_module(&mut self, module: &Module) {
        for stmt in &module.body {
            self.compile_stmt(&stmt.node);
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt{
            Stmt::Expr(expr) => {
                self.compile_expr(&expr.node);
            }
            Stmt::Assign {targets, value, ..} => {
                let val_reg = self.compile_expr(&value.node);
                for target in targets {
                    if let Expr::Name(name) = &target.node {
                        self.handle_assignment(name, val_reg);
                    }
                }
            }
            Stmt::Global(names) => {
                for name in names{
                    self.current_context.explicit_globals.insert(name.clone());
                }
            }
            Stmt::Return(Some(expr)) => {
                let reg = self.compile_expr(&expr.node);
                self.emit(Instruction::Return(reg));
            }
            Stmt::Return(None) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::None));
                self.emit(Instruction::Return(reg));
            }
            Stmt::If {test, body, elif_clauses, else_body} => {
                self.compile_if(test, body, elif_clauses, else_body);
            }
            Stmt::While {test, body, else_body} => {
                self.compile_while(test, body, else_body);
            }
            Stmt::FuncDef {name, params, body, ..} => {
                self.compile_func_def(name, params, body);
            }
            Stmt::Pass | Stmt::Break | Stmt::Continue => {
                // TODO: implement control flow for break and continue, and just ignore pass for now
            }
            _ => unimplemented!("Statement type {:?} not implemented yet", stmt),
        }
    }

    fn handle_assignment(&mut self, name: &str, val_reg: u8) {
        if self.current_context.scope_type == ScopeType::Module{
            // In module scope, all assignments are global
            self.emit(Instruction::StoreGlobal(name.to_string(), val_reg));
        } else {
            if self.current_context.explicit_globals.contains(name) {
                self.emit(Instruction::StoreGlobal(name.to_string(), val_reg));
            } else {
                self.current_context.locals.insert(name.to_string());
                self.emit(Instruction::StoreLocal(name.to_string(), val_reg));
            }
        }
    }

    fn compile_func_def(&mut self, name: &str, params: &[Param], body: &[Spanned<Stmt>]) {
        // Swap the current context with a new one for a fresh function context
        let mut func_context = Context::new(ScopeType::Function);

        for (i, param) in params.iter().enumerate() {
            func_context.locals.insert(param.name.to_string());
            func_context.emit(Instruction::StoreLocal(param.name.clone(), i as u8));

            if func_context.next_reg <= i as u8 {
                func_context.next_reg = i as u8 + 1;
            }
        }

        // Scan the function body for assignments to determine local variables and explicit globals before compiling the body,
        // so we know how to handle variable accesses and assignments correctly.
        Self::scan_scope(body, &mut func_context.locals, &mut func_context.explicit_globals);

        let parent_context = std::mem::replace(&mut self.current_context, func_context);

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        let none_reg = self.alloc_reg();
        self.emit(Instruction::LoadConst(none_reg, Value::None));
        self.emit(Instruction::Return(none_reg));

        let func_code = CodeObject {
            instructions: self.current_context.instructions.clone(),
            register_count: self.current_context.next_reg,
        };

        let func_id = self.functions.len();
        self.functions.push(func_code);

        self.current_context = parent_context;

        let reg = self.alloc_reg();
        self.emit(Instruction::LoadConst(reg, Value::Function(func_id)));
        self.emit(Instruction::StoreGlobal(name.to_string(), reg));
    }

    fn compile_expr(&mut self, expr: &Expr) -> u8 {
        match expr{
            Expr::Number(text) => {
                let reg = self.alloc_reg();
                if let Ok(n) = text.parse::<i64>() {
                    self.emit(Instruction::LoadConst(reg, Value::Int(n)));
                } else if let Ok(f) = text.parse::<f64>() {
                    self.emit(Instruction::LoadConst(reg, Value::Float(f)));
                }
                reg
            }
            Expr::StringLit(literal) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::Str(literal.clone())));
                reg
            }
            Expr::Bool(b) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::Bool(*b)));
                reg
            }
            Expr::NoneLit => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::None));
                reg
            }
            Expr::Name(name) => {
                let reg = self.alloc_reg();
                if self.current_context.locals.contains(name) {
                    self.emit(Instruction::LoadLocal(reg, name.clone()));
                } else {
                    self.emit(Instruction::LoadGlobal(reg, name.clone()));
                }
                reg
            }
            Expr::BinOp { left, op, right } => {
                let left_reg = self.compile_expr(&left.node);
                let right_reg = self.compile_expr(&right.node);
                let dest_reg = self.alloc_reg();

                let kind = Self::translate_binop(op);
                self.emit(Instruction::BinOp(dest_reg, kind, left_reg, right_reg));
                dest_reg
            }
            Expr::UnaryOp { op, operand } => {
                let operand_reg = self.compile_expr(&operand.node);
                let dest_reg = self.alloc_reg();

                let kind = Self::translate_unary_op(op);
                self.emit(Instruction::UnaryOp(dest_reg, kind, operand_reg));
                dest_reg
            }
            Expr::Compare {left, ops, comparators} => {
                self.compile_compare(left, ops, comparators)
            }
            Expr::IfExpr { test, body, orelse } => {
                self.compile_if_expr(test, body, orelse)
            }
            Expr::Call { func, args, kwargs: _ } => {
                self.compile_call_expr(func, args)
            }
            Expr::Attribute { value, attr } => {
                let src_reg = self.compile_expr(&value.node);
                let dest_reg = self.alloc_reg();
                self.emit(Instruction::LoadAttr(dest_reg, src_reg, attr.clone()));
                dest_reg
            }
            _ => unimplemented!("Expression type {:?} not implemented yet", expr),
        }
    }

    fn compile_compare(&mut self, left: &Spanned<Expr>, ops: &[CmpOp], comparators: &[Spanned<Expr>]) -> u8 {
        // For a single comparison: <left> <OP> <right>
        // For chained comparisons: <left> <OP1> <mid1> <OP2> <mid2> ... <OPn> <right>
        let mut left_reg = self.compile_expr(&left.node);
        let dest_reg = self.alloc_reg();
        self.emit(Instruction::LoadConst(dest_reg, Value::Bool(true))); // write true to dest_reg, then override dest_reg if needed

        for (op, comp) in ops.iter().zip(comparators.iter()) {
            let right_reg = self.compile_expr(&comp.node);
            let cmp_reg = self.alloc_reg();
            self.emit(Instruction::Compare(cmp_reg, Self::translate_cmp_op(op), left_reg, right_reg));
            // dest_reg = dest_reg AND cmp_reg
            let and_reg = self.alloc_reg();
            self.emit(Instruction::BinOp(and_reg, BinOpKind::And, dest_reg, cmp_reg));
            self.emit(Instruction::Move(dest_reg, and_reg)); // dest_reg override
            left_reg = right_reg; // for next comparison in chain
        }
        dest_reg
    }

    fn compile_call_expr(&mut self, func: &Spanned<Expr>, args: &Vec<Spanned<Expr>>) -> u8 {
        let func_reg = self.compile_expr(&func.node);

        let arg_start = self.current_context.next_reg;
        for arg in args {
            let r = self.compile_expr(&arg.node);
            // ensure args are in consecutive registers
            if r != self.current_context.next_reg - 1{
                let target = self.alloc_reg();
                self.emit(Instruction::Move(target, r));
            }
        }
        let dest_reg = self.alloc_reg();
        self.emit(Instruction::Call(dest_reg, func_reg, arg_start, args.len() as u8));
        dest_reg
    }

    fn compile_if_expr(&mut self, test: &Spanned<Expr>, body: &Spanned<Expr>, orelse: &Spanned<Expr>) -> u8 {
        let test_reg = self.compile_expr(&test.node);
        let else_placeholder = self.current_context.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, 0)); // placeholder, will be patched later

        let body_reg = self.compile_expr(&body.node);
        let dest_reg = self.alloc_reg();
        self.emit(Instruction::Move(dest_reg, body_reg));
        let end_placeholder = self.current_context.instructions.len();
        self.emit(Instruction::Jump(0)); // placeholder, will be patched later

        let else_target = self.current_context.instructions.len();
        self.current_context.instructions[else_placeholder] = Instruction::JumpIfFalse(test_reg, else_target);

        let orelse_reg = self.compile_expr(&orelse.node);
        self.emit(Instruction::Move(dest_reg, orelse_reg));

        let end_target = self.current_context.instructions.len();
        self.current_context.instructions[end_placeholder] = Instruction::Jump(end_target);
        dest_reg
    }

    fn compile_if(
        &mut self,
        test: &Spanned<Expr>,
        body: &[Spanned<Stmt>],
        elif_clauses: &[(Spanned<Expr>, Vec<Spanned<Stmt>>)],
        else_body: &Option<Vec<Spanned<Stmt>>>,
    ) {
        let test_reg = self.compile_expr(&test.node); // From here, we will jump to the appropriate body based on the test result
        let jump_else = self.current_context.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, 0));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        let mut end_jumps = vec![];
        end_jumps.push(self.current_context.instructions.len());
        self.emit(Instruction::Jump(0));

        // Patch the if/else jump
        let here = self.current_context.instructions.len(); // We will jump here if the test was false
        self.current_context.instructions[jump_else] = Instruction::JumpIfFalse(test_reg, here);

        for(elif_test, elif_body) in elif_clauses {
            let elif_test_reg = self.compile_expr(&elif_test.node);
            let jump_next_elif = self.current_context.instructions.len();
            self.emit(Instruction::JumpIfFalse(elif_test_reg, 0));

            for stmt in elif_body{
                self.compile_stmt(&stmt.node);
            }
            end_jumps.push(self.current_context.instructions.len());
            self.emit(Instruction::Jump(0));

            let here = self.current_context.instructions.len();
            self.current_context.instructions[jump_next_elif] = Instruction::JumpIfFalse(elif_test_reg, here);
        }

        if let Some(else_body) = else_body {
            for stmt in else_body {
                self.compile_stmt(&stmt.node);
            }
        }

        // Patch all the jumps to the end of the if/elif/else chain
        let end = self.current_context.instructions.len();
        for jump in end_jumps {
            self.current_context.instructions[jump] = Instruction::Jump(end);
        }
    }

    fn compile_while(&mut self, test: &Spanned<Expr>, body: &[Spanned<Stmt>], _else_body: &Option<Vec<Spanned<Stmt>>>) {
        let loop_start = self.current_context.instructions.len();
        let test_reg = self.compile_expr(&test.node);
        let exit_jump = self.current_context.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, exit_jump));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }
        self.emit(Instruction::Jump(loop_start));

        let end = self.current_context.instructions.len();
        self.current_context.instructions[exit_jump] = Instruction::JumpIfFalse(test_reg, end);
    }
}