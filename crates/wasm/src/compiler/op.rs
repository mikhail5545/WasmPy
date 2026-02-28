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

impl Compiler {
    pub(super) fn compile_unary_op(&mut self, op: &UnaryOp, operand: &Spanned<Expr>) -> WasmType {
        let op_type = self.compile_expr(&operand.node);
        
        match op {
            UnaryOp::Neg => match op_type {
                WasmType::I64 => {
                    // -x == x * -1 (operand is already on the stack)
                    self.ctx().emit(&Instruction::I64Const(-1));
                    self.ctx().emit(&Instruction::I64Mul);
                    WasmType::I64
                }
                WasmType::F64 => {
                    self.ctx().emit(&Instruction::F64Neg);
                    WasmType::F64
                }
                WasmType::I32 => {
                    // Promote bool to i64, then negate
                    self.ctx().emit(&Instruction::I64ExtendI32S);
                    self.ctx().emit(&Instruction::I64Const(-1));
                    self.ctx().emit(&Instruction::I64Mul);
                    WasmType::I64
                }
                _ => op_type,
            },
            UnaryOp::Pos => {
                // +x is a no-op for numeric types, but bool promotes to int
                if op_type == WasmType::I32 {
                    self.ctx().emit(&Instruction::I64ExtendI32S);
                    WasmType::I64
                } else {
                    op_type
                }
            }
            UnaryOp::Not => {
                self.ctx().emit(&Instruction::I32Eqz);
                WasmType::I32
            }
            UnaryOp::Invert => {
                // ~x = x ^ -1
                if op_type == WasmType::I32 {
                    self.ctx().emit(&Instruction::I64ExtendI32S);
                }
                self.ctx().emit(&Instruction::I64Const(-1));
                self.ctx().emit(&Instruction::I64Xor);
                WasmType::I64
            }
        }
    }
    
    pub(super) fn compile_bin_op(&mut self, op: &BinOp, left: &Spanned<Expr>, right: &Spanned<Expr> ) -> WasmType {
        // Logical AND/OR need short-circuit evaluation
        if matches!(op, BinOp::And | BinOp::Or) {
            return self.compile_logical(left, op, right);
        }
        let lt = self.compile_expr(&left.node);
        let rt = self.compile_expr(&right.node);

        // Division always produces float
        if matches!(op, BinOp::Div) {
            // Promote both operands to f64 if they aren't already
            // But we need to promote in-place on the stack. The issue is the values
            // are already on the stack. We need to handle this differently.
            // For now: compile_expr pushes left, then right.
            // We need to convert them if needed.
            //
            // Strategy: recompile with promotion. Since the type checker already
            // validated this, we know it's safe.
            //
            // Actually, we can't reorder stack values easily. Instead, we handle
            // promotion by checking types and emitting conversion.

            // If the right needs promotion, we can convert it now (it's on top).
            // Then we need to convert left (which is below right).
            // This requires a temp local. Let's handle this properly.
            self.emit_stack_promotion_for_binop(lt, rt, WasmType::F64);
            self.ctx().emit(&Instruction::F64Div);
            return WasmType::F64;
        }

        let result_type = lt.numeric_promote(rt).unwrap_or(lt);

        // Promote operands if needed
        self.emit_stack_promotion_for_binop(lt, rt, result_type);

        self.emit_binop_dispatch(result_type, op);

        result_type
    }

    pub(super) fn compile_logical(
        &mut self,
        left: &Spanned<Expr>,
        op: &BinOp,
        right: &Spanned<Expr>,
    ) -> WasmType {
        // Short-circuit evaluation using WASM if/else
        let _lt = self.compile_expr(&left.node);

        match op {
            BinOp::And => {
                // If left is false, result is false (don't evaluate right)
                self.ctx().emit(&Instruction::If(BlockType::Result(ValType::I32)));
                self.compile_expr(&right.node);
                self.ctx().emit(&Instruction::Else);
                self.ctx().emit(&Instruction::I32Const(0));
                self.ctx().emit(&Instruction::End);
            }
            BinOp::Or => {
                // If left is true, result is true (don't evaluate right)
                self.ctx().emit(&Instruction::If(BlockType::Result(ValType::I32)));
                self.ctx().emit(&Instruction::I32Const(1));
                self.ctx().emit(&Instruction::Else);
                self.compile_expr(&right.node);
                self.ctx().emit(&Instruction::End);
            }
            _ => unreachable!(),
        }

        WasmType::I32
    }

    pub(super) fn compile_compare(
        &mut self,
        left: &Spanned<Expr>,
        ops: &[CmpOp],
        comparators: &[Spanned<Expr>],
    ) -> WasmType {
        if ops.len() == 1 {
            // Simple comparison: left OP right
            let lt = self.compile_expr(&left.node);
            let rt = self.compile_expr(&comparators[0].node);

            let cmp_type = lt.numeric_promote(rt).unwrap_or(lt);
            self.emit_stack_promotion_for_binop(lt, rt, cmp_type);
            self.emit_cmp_instruction(&ops[0], cmp_type);

            return WasmType::I32;
        }

        // Chained comparison: a < b < c → (a < b) and (b < c)
        // We need to evaluate `b` only once.
        let mut left_type = self.compile_expr(&left.node);

        // We'll accumulate the boolean result
        // Start with true
        self.ctx().emit(&Instruction::I32Const(1));

        for (op, comp) in ops.iter().zip(comparators.iter()) {
            // We have: [left_val, accumulator] on stack
            // But actually this gets complicated with WASM's stack machine.
            // For simplicity, use temp locals for chained comparisons.

            // Store the accumulator
            let acc_idx = self.ctx().declare_local("__chain_acc".to_string(), WasmType::I32);
            self.ctx().emit(&Instruction::LocalSet(acc_idx));

            // Store left value for reuse
            let left_tmp = self.ctx().declare_local(
                format!("__chain_left_{}", 0),
                left_type,
            );
            self.ctx().emit(&Instruction::LocalSet(left_tmp));

            // Compile comparator
            let right_type = self.compile_expr(&comp.node);

            // Store right for potential reuse as next left
            let right_tmp = self.ctx().declare_local(
                format!("__chain_right_{}", 0),
                right_type,
            );
            self.ctx().emit(&Instruction::LocalSet(right_tmp));

            // Load left and right for comparison
            self.ctx().emit(&Instruction::LocalGet(left_tmp));
            self.ctx().emit(&Instruction::LocalGet(right_tmp));

            let cmp_type = left_type.numeric_promote(right_type).unwrap_or(left_type);
            self.emit_stack_promotion_for_binop(left_type, right_type, cmp_type);
            self.emit_cmp_instruction(op, cmp_type);

            // AND with accumulator
            self.ctx().emit(&Instruction::LocalGet(acc_idx));
            self.ctx().emit(&Instruction::I32And);

            // The right value becomes the new left for the next comparison
            left_type = right_type;
            // Push right value back for next iteration
            // We need to restructure... actually for chained, we need right as the next left.
            // Let's just reload it at the start of the next iteration.
        }

        WasmType::I32
    }

    pub(super) fn emit_cmp_instruction(&mut self, op: &CmpOp, cmp_type: WasmType) {
        match cmp_type {
            WasmType::I64 => match op {
                CmpOp::Eq => self.ctx().emit(&Instruction::I64Eq),
                CmpOp::NotEq => self.ctx().emit(&Instruction::I64Ne),
                CmpOp::Lt => self.ctx().emit(&Instruction::I64LtS),
                CmpOp::LtE => self.ctx().emit(&Instruction::I64LeS),
                CmpOp::Gt => self.ctx().emit(&Instruction::I64GtS),
                CmpOp::GtE => self.ctx().emit(&Instruction::I64GeS),
                _ => {}
            },
            WasmType::F64 => match op {
                CmpOp::Eq => self.ctx().emit(&Instruction::F64Eq),
                CmpOp::NotEq => self.ctx().emit(&Instruction::F64Ne),
                CmpOp::Lt => self.ctx().emit(&Instruction::F64Lt),
                CmpOp::LtE => self.ctx().emit(&Instruction::F64Le),
                CmpOp::Gt => self.ctx().emit(&Instruction::F64Gt),
                CmpOp::GtE => self.ctx().emit(&Instruction::F64Ge),
                _ => {}
            },
            WasmType::I32 => match op {
                CmpOp::Eq => self.ctx().emit(&Instruction::I32Eq),
                CmpOp::NotEq => self.ctx().emit(&Instruction::I32Ne),
                CmpOp::Lt => self.ctx().emit(&Instruction::I32LtS),
                CmpOp::LtE => self.ctx().emit(&Instruction::I32LeS),
                CmpOp::Gt => self.ctx().emit(&Instruction::I32GtS),
                CmpOp::GtE => self.ctx().emit(&Instruction::I32GeS),
                _ => {}
            },
            _ => {}
        }
    }
    
    /// For binary operations where both operands are already on the stack,
    /// promote them to the target type.
    ///
    /// Stack state: [..., left, right]
    /// We can only convert the top value (right) directly.
    /// If left also needs promotion, we use a temp local.
    pub(super) fn emit_stack_promotion_for_binop(&mut self, left_type: WasmType, right_type: WasmType, target_type: WasmType) {
        let left_needs = left_type != target_type;
        let right_needs = right_type != target_type;
        
        if !left_needs && !right_needs {
            return;
        }
        
        if !left_needs && right_needs {
            // If only right operand needs promotion, we can do it directly on the stack (it's on top).
            self.emit_promotion(right_type, target_type);
        } else if left_needs && !right_needs {
            // It only left needs promotion, we need to save the right operand, promote left, then restore right.
            let tmp_name = "__promote_tmp".to_string();
            let tmp_idx = self.ctx().declare_local(tmp_name, right_type);
            self.ctx().emit(&Instruction::LocalSet(tmp_idx));
            self.emit_promotion(left_type, target_type);
            self.ctx().emit(&Instruction::LocalGet(tmp_idx));
        } else {
            let tmp_name = "__promote_tmp".to_string();
            let tmp_idx = self.ctx().declare_local(tmp_name, right_type);
            self.ctx().emit(&Instruction::LocalSet(tmp_idx)); // save right
            self.emit_promotion(left_type, target_type); // promote left
            self.ctx().emit(&Instruction::LocalGet(tmp_idx)); // restore right
            self.emit_promotion(right_type, target_type); // promote right
        }
    }
}