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
use parser::ast::{Module, Spanned, Stmt};
use crate::instruction::*;

// TODO: implement registers per-scope, call frame stack with saved ip, register windows, and local scopes to allow for function calls and recursion. For now, we will just use a single global register space and no call frames.
pub struct Compiler {
    instructions: Vec<Instruction>,
    next_reg: u8,
}

impl Compiler {
    pub fn new() -> Self{
        Self{
            instructions: Vec::new(),
            next_reg: 0,
        }
    }

    fn alloc_reg(&mut self) -> u8 {
        let r = self.next_reg;
        self.next_reg += 1;
        assert!(self.next_reg < 255, "Too many registers allocated");
        r
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn finish(mut self) -> Vec<Instruction> {
        self.emit(Instruction::Halt);
        self.instructions
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
            Stmt::Assign {targets, value} => {
                let val_reg = self.compile_expr(&value.node);
                for target in targets {
                    if let parser::ast::Expr::Name(name) = &target.node {
                        self.emit(Instruction::StoreName(name.clone(), val_reg));
                    }
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
            Stmt::Pass | Stmt::Break | Stmt::Continue => {
                // TODO: implement control flow for break and continue, and just ignore pass for now
            }
            _ => unimplemented!("Statement type {:?} not implemented yet", stmt),
        }
    }

    fn compile_expr(&mut self, expr: &parser::ast::Expr) -> u8 {
        match expr{
            parser::ast::Expr::Number(text) => {
                let reg = self.alloc_reg();
                if let Ok(n) = text.parse::<i64>() {
                    self.emit(Instruction::LoadConst(reg, Value::Int(n)));
                } else if let Ok(f) = text.parse::<f64>() {
                    self.emit(Instruction::LoadConst(reg, Value::Float(f)));
                }
                reg
            }
            parser::ast::Expr::StringLit(literal) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::Str(literal.clone())));
                reg
            }
            parser::ast::Expr::Bool(b) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::Bool(*b)));
                reg
            }
            parser::ast::Expr::NoneLit => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(reg, Value::None));
                reg
            }
            parser::ast::Expr::Name(name) => {
                let reg = self.alloc_reg();
                self.emit(Instruction::LoadName(reg, name.clone()));
                reg
            }
            parser::ast::Expr::BinOp { left, op, right } => {
                let left_reg = self.compile_expr(&left.node);
                let right_reg = self.compile_expr(&right.node);
                let dest_reg = self.alloc_reg();

                let kind = Self::translate_binop(op);
                self.emit(Instruction::BinOp(dest_reg, kind, left_reg, right_reg));
                dest_reg
            }
            parser::ast::Expr::UnaryOp { op, operand } => {
                let operand_reg = self.compile_expr(&operand.node);
                let dest_reg = self.alloc_reg();

                let kind = Self::translate_unary_op(op);
                self.emit(Instruction::UnaryOp(dest_reg, kind, operand_reg));
                dest_reg
            }
            parser::ast::Expr::Compare {left, ops, comparators} => {
                self.compile_compare(left, ops, comparators)
            }
            parser::ast::Expr::IfExpr { test, body, orelse } => {
                self.compile_if_expr(test, body, orelse)
            }
            parser::ast::Expr::Call { func, args, kwargs: _ } => {
                self.compile_call_expr(func, args)
            }
            _ => unimplemented!("Expression type {:?} not implemented yet", expr),
        }
    }

    fn compile_compare(
        &mut self,
        left: &parser::ast::Spanned<parser::ast::Expr>,
        ops: &[parser::ast::CmpOp],
        comparators: &[parser::ast::Spanned<parser::ast::Expr>],
    ) -> u8 {
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

    fn compile_call_expr(
        &mut self,
        func: &Spanned<parser::ast::Expr>,
        args: &Vec<Spanned<parser::ast::Expr>>,
    ) -> u8 {
        let func_reg = self.compile_expr(&func.node);

        if let parser::ast::Expr::Name(name) = &func.node {
            if name == "print" && !args.is_empty() {
                let arg_reg = self.compile_expr(&args[0].node);
                self.emit(Instruction::Print(arg_reg));
                let dest_reg = self.alloc_reg();
                self.emit(Instruction::LoadConst(dest_reg, Value::None)); // print() returns None
                return dest_reg;
            }
        }
        let arg_start = self.next_reg;
        for arg in args {
            let r = self.compile_expr(&arg.node);
            // ensure args are in consecutive registers
            if r != self.next_reg - 1{
                let target = self.alloc_reg();
                self.emit(Instruction::Move(target, r));
            }
        }
        let dest_reg = self.alloc_reg();
        self.emit(Instruction::Call(dest_reg, func_reg, arg_start, args.len() as u8));
        dest_reg
    }

    fn compile_if_expr(
        &mut self,
        test: &parser::ast::Spanned<parser::ast::Expr>,
        body: &parser::ast::Spanned<parser::ast::Expr>,
        orelse: &parser::ast::Spanned<parser::ast::Expr>,
    ) -> u8 {
        let test_reg = self.compile_expr(&test.node);
        let else_placeholder = self.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, 0)); // placeholder, will be patched later

        let body_reg = self.compile_expr(&body.node);
        let dest_reg = self.alloc_reg();
        self.emit(Instruction::Move(dest_reg, body_reg));
        let end_placeholder = self.instructions.len();
        self.emit(Instruction::Jump(0)); // placeholder, will be patched later

        let else_target = self.instructions.len();
        self.instructions[else_placeholder] = Instruction::JumpIfFalse(test_reg, else_target);

        let orelse_reg = self.compile_expr(&orelse.node);
        self.emit(Instruction::Move(dest_reg, orelse_reg));

        let end_target = self.instructions.len();
        self.instructions[end_placeholder] = Instruction::Jump(end_target);
        dest_reg
    }

    fn compile_if(
        &mut self,
        test: &parser::ast::Spanned<parser::ast::Expr>,
        body: &[parser::ast::Spanned<parser::ast::Stmt>],
        elif_clauses: &[(parser::ast::Spanned<parser::ast::Expr>, Vec<parser::ast::Spanned<parser::ast::Stmt>>)],
        else_body: &Option<Vec<parser::ast::Spanned<parser::ast::Stmt>>>,
    ) {
        let test_reg = self.compile_expr(&test.node); // From here, we will jump to the appropriate body based on the test result
        let jump_else = self.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, 0));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        let mut end_jumps = vec![];
        end_jumps.push(self.instructions.len());
        self.emit(Instruction::Jump(0));

        // Patch the if/else jump
        let here = self.instructions.len(); // We will jump here if the test was false
        self.instructions[jump_else] = Instruction::JumpIfFalse(test_reg, here);

        for(elif_test, elif_body) in elif_clauses {
            let elif_test_reg = self.compile_expr(&elif_test.node);
            let jump_next_elif = self.instructions.len();
            self.emit(Instruction::JumpIfFalse(elif_test_reg, 0));

            for stmt in elif_body{
                self.compile_stmt(&stmt.node);
            }
            end_jumps.push(self.instructions.len());
            self.emit(Instruction::Jump(0));

            let here = self.instructions.len();
            self.instructions[jump_next_elif] = Instruction::JumpIfFalse(elif_test_reg, here);
        }

        if let Some(else_body) = else_body {
            for stmt in else_body {
                self.compile_stmt(&stmt.node);
            }
        }

        // Patch all the jumps to the end of the if/elif/else chain
        let end = self.instructions.len();
        for jump in end_jumps {
            self.instructions[jump] = Instruction::Jump(end);
        }
    }

    fn compile_while(
        &mut self,
        test: &parser::ast::Spanned<parser::ast::Expr>,
        body: &[parser::ast::Spanned<parser::ast::Stmt>],
        _else_body: &Option<Vec<parser::ast::Spanned<parser::ast::Stmt>>>,
    ) {
        let loop_start = self.instructions.len();
        let test_reg = self.compile_expr(&test.node);
        let exit_jump = self.instructions.len();
        self.emit(Instruction::JumpIfFalse(test_reg, exit_jump));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }
        self.emit(Instruction::Jump(loop_start));

        let end = self.instructions.len();
        self.instructions[exit_jump] = Instruction::JumpIfFalse(test_reg, end);
    }

    fn translate_binop(op: &parser::ast::BinOp) -> BinOpKind {
        match op{
            parser::ast::BinOp::Add => BinOpKind::Add,
            parser::ast::BinOp::Sub => BinOpKind::Sub,
            parser::ast::BinOp::Mul => BinOpKind::Mul,
            parser::ast::BinOp::Div => BinOpKind::Div,
            parser::ast::BinOp::FloorDiv => BinOpKind::FloorDiv,
            parser::ast::BinOp::Mod => BinOpKind::Mod,
            parser::ast::BinOp::Pow => BinOpKind::Pow,
            parser::ast::BinOp::BitAnd => BinOpKind::BitAnd,
            parser::ast::BinOp::BitOr => BinOpKind::BitOr,
            parser::ast::BinOp::BitXor => BinOpKind::BitXor,
            parser::ast::BinOp::LShift => BinOpKind::LShift,
            parser::ast::BinOp::RShift => BinOpKind::RShift,
            parser::ast::BinOp::And => BinOpKind::And,
            parser::ast::BinOp::Or => BinOpKind::Or,
            _ => BinOpKind::Add, // fallback
        }
    }

    fn translate_unary_op(op: &parser::ast::UnaryOp) -> UnaryOpKind {
        match op{
            parser::ast::UnaryOp::Neg => UnaryOpKind::Neg,
            parser::ast::UnaryOp::Not => UnaryOpKind::Not,
            parser::ast::UnaryOp::Invert => UnaryOpKind::Invert,
            parser::ast::UnaryOp::Pos => UnaryOpKind::Pos,
        }
    }

    fn translate_cmp_op(op: &parser::ast::CmpOp) -> CmpOpKind {
        match op{
            parser::ast::CmpOp::Eq => CmpOpKind::Eq,
            parser::ast::CmpOp::NotEq => CmpOpKind::NotEq,
            parser::ast::CmpOp::Lt => CmpOpKind::Lt,
            parser::ast::CmpOp::LtE => CmpOpKind::LtE,
            parser::ast::CmpOp::Gt => CmpOpKind::Gt,
            parser::ast::CmpOp::GtE => CmpOpKind::GtE,
            parser::ast::CmpOp::Is => CmpOpKind::Is,
            parser::ast::CmpOp::IsNot => CmpOpKind::IsNot,
            parser::ast::CmpOp::In => CmpOpKind::In,
            parser::ast::CmpOp::NotIn => CmpOpKind::NotIn,
        }
    }
}