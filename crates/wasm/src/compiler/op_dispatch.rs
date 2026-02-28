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
    pub(super) fn emit_aug_assign_dispatch(&mut self, operand_type: WasmType, op: &AugOp) {
        match operand_type {
            WasmType::I64 => {
                self.emit_i64_aug_assign(op)
            }
            WasmType::F64 => {
                self.emit_f64_aug_assign(op)
            }
            _ => panic!("Unsupported type for augmented assignment: {:?}", operand_type),
        }
    }

    fn emit_i64_aug_assign(&mut self, op: &AugOp) {
        match op {
            AugOp::Add => self.ctx().emit(&Instruction::I64Add),
            AugOp::Sub => self.ctx().emit(&Instruction::I64Sub),
            AugOp::Mul => self.ctx().emit(&Instruction::I64Mul),
            AugOp::Div => self.ctx().emit(&Instruction::I64DivS),
            AugOp::FloorDiv => self.ctx().emit(&Instruction::I64DivS),
            AugOp::Mod => self.ctx().emit(&Instruction::I64RemS),
            AugOp::BitAnd => self.ctx().emit(&Instruction::I64And),
            AugOp::BitOr => self.ctx().emit(&Instruction::I64Or),
            AugOp::BitXor => self.ctx().emit(&Instruction::I64Xor),
            AugOp::LShift => self.ctx().emit(&Instruction::I64Shl),
            AugOp::RShift => self.ctx().emit(&Instruction::I64ShrS),
            _ => unimplemented!("Unsupported augmented assignment operator for i64: {:?}", op),
        }
    }

    fn emit_f64_aug_assign(&mut self, op: &AugOp) {
        match op {
            AugOp::Add => self.ctx().emit(&Instruction::F64Add),
            AugOp::Sub => self.ctx().emit(&Instruction::F64Sub),
            AugOp::Mul => self.ctx().emit(&Instruction::F64Mul),
            AugOp::Div => self.ctx().emit(&Instruction::F64Div),
            _ => unimplemented!("Unsupported augmented assignment operator for f64: {:?}", op),
        }
    }

    pub(super) fn emit_binop_dispatch(&mut self, ty: WasmType, op: &BinOp) {
        match ty {
            WasmType::I64 => self.emit_i64_binop(op),
            WasmType::F64 => self.emit_f64_binop(op),
            WasmType::I32 => self.emit_i32_binop(op),
            WasmType::ListRef => self.emit_list_ref_binop(op),
            WasmType::StringRef => self.emit_str_ref_binop(op),
            _ => panic!("Unsupported type for binary operation: {:?}", ty),
        }
    }

    fn emit_i64_binop(&mut self, op: &BinOp) {
        match op{
            BinOp::Add => self.ctx().emit(&Instruction::I64Add),
            BinOp::Sub => self.ctx().emit(&Instruction::I64Sub),
            BinOp::Mul => self.ctx().emit(&Instruction::I64Mul),
            BinOp::FloorDiv => self.ctx().emit(&Instruction::I64DivS),
            BinOp::Mod => self.ctx().emit(&Instruction::I64RemS),
            BinOp::Pow => {
                // WASM doesn't have i64 pow — we'd need a helper.
                // For now, convert to f64, pow, convert back.
                // But the values are already on stack as i64...
                // This is a simplification; a real impl would need a runtime helper.
                // TODO: implement integer pow as a helper function
                self.ctx().emit(&Instruction::I64Mul); // placeholder
            }
            BinOp::BitAnd => self.ctx().emit(&Instruction::I64And),
            BinOp::BitOr => self.ctx().emit(&Instruction::I64Or),
            BinOp::BitXor => self.ctx().emit(&Instruction::I64Xor),
            BinOp::LShift => self.ctx().emit(&Instruction::I64Shl),
            BinOp::RShift => self.ctx().emit(&Instruction::I64ShrS),
            _ => unimplemented!("Unsupported binary operator for i64: {:?}", op),
        }
    }

    fn emit_f64_binop(&mut self, op: &BinOp) {
        match op {
            BinOp::Add => self.ctx().emit(&Instruction::F64Add),
            BinOp::Sub => self.ctx().emit(&Instruction::F64Sub),
            BinOp::Mul => self.ctx().emit(&Instruction::F64Mul),
            BinOp::Div => self.ctx().emit(&Instruction::F64Div),
            BinOp::Mod => {
                // WASM doesn't have f64.rem — would need a runtime import
                // TODO: implement float modulo via import
            }
            BinOp::Pow => {
                // TODO: implement via imported math.pow
            }
            _ => unimplemented!("Unsupported binary operator for f64: {:?}", op),
        }
    }

    fn emit_i32_binop(&mut self, op: &BinOp) {
        match op{
            BinOp::Add => self.ctx().emit(&Instruction::I32Add),
            BinOp::Sub => self.ctx().emit(&Instruction::I32Sub),
            BinOp::Mul => self.ctx().emit(&Instruction::I32Mul),
            BinOp::FloorDiv => self.ctx().emit(&Instruction::I32DivS),
            BinOp::Mod => self.ctx().emit(&Instruction::I32RemS),
            BinOp::BitAnd => self.ctx().emit(&Instruction::I32And),
            BinOp::BitOr => self.ctx().emit(&Instruction::I32Or),
            BinOp::BitXor => self.ctx().emit(&Instruction::I32Xor),
            BinOp::LShift => self.ctx().emit(&Instruction::I32Shl),
            BinOp::RShift => self.ctx().emit(&Instruction::I32ShrS),
            _ => unimplemented!("Unsupported binary operator for i32: {:?}", op),
        }
    }

    fn emit_list_ref_binop(&mut self, op: &BinOp) {
        match op{
            BinOp::Add => {
                // For list concatenation, we would need to call a helper function.
                // This is a placeholder; a real implementation would involve more complex logic.
                self.ctx().emit(&Instruction::Call(0)); // placeholder function index
            }
            _ => unimplemented!("Unsupported binary operator for list references: {:?}", op),
        }
    }

    fn emit_str_ref_binop(&mut self, op: &BinOp) {
        match op{
            BinOp::Add => {
                // For string concatenation, we would need to call a helper function.
                // This is a placeholder; a real implementation would involve more complex logic.
                self.ctx().emit(&Instruction::Call(0)); // placeholder function index
            }
            _ => unimplemented!("Unsupported binary operator for string references: {:?}", op),
        }
    }
}