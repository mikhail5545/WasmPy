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

use super::*;

impl Compiler{
    pub(super) fn compile_if(&mut self, test: &Spanned<Expr>, body: &[Spanned<Stmt>], elif_clauses: &[(Spanned<Expr>, Vec<Spanned<Stmt>>)], else_body: &Option<Vec<Spanned<Stmt>>>) {
        self.compile_expr(&test.node);
        self.ctx().emit(&Instruction::If(BlockType::Empty));

        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        if !elif_clauses.is_empty() || else_body.is_some() {
            self.ctx().emit(&Instruction::Else);

            // Elif chains become nested if/else
            let mut remaining_elifs = elif_clauses.iter().peekable();
            if let Some((elif_test, elif_body)) = remaining_elifs.next() {
                self.compile_if(
                    elif_test,
                    elif_body,
                    &elif_clauses[1..].to_vec(),
                    else_body,
                );
            } else if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    self.compile_stmt(&stmt.node);
                }
            }
        }

        self.ctx().emit(&Instruction::End);
    }

    pub(super) fn compile_while(&mut self, test: &Spanned<Expr>, body: &[Spanned<Stmt>], _else_body: &Option<Vec<Spanned<Stmt>>>) {
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
        self.ctx().emit(&Instruction::Block(BlockType::Empty));
        self.ctx().emit(&Instruction::Loop(BlockType::Empty));

        // Compile test
        self.compile_expr(&test.node);
        self.ctx().emit(&Instruction::I32Eqz);
        self.ctx().emit(&Instruction::BrIf(1)); // break to outer block if test is false

        // Compile body
        for stmt in body {
            self.compile_stmt(&stmt.node);
        }

        self.ctx().emit(&Instruction::Br(0)); // jump back to loop start
        self.ctx().emit(&Instruction::End); // end loop
        self.ctx().emit(&Instruction::End); // end block
    }
}