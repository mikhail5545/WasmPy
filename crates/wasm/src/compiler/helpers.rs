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
    /// Peek at what type an expression would produce (without emitting code).
    /// Used for BlockType decisions in if-expressions.
    pub(super) fn peek_expr_type(&mut self, expr: &Expr) -> WasmType {
        match expr {
            Expr::Number(text) => {
                if Self::is_floating_point(text) {
                    WasmType::F64
                } else {
                    WasmType::I64
                }
            }
            Expr::Bool(_) => WasmType::I32,
            Expr::NoneLit => WasmType::Void,
            Expr::Name(name) => self.ctx().get_local_type(name).unwrap_or(WasmType::Void),
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

    /// Emit promotion instructions to convert `actual` type to `expected` type.
    /// The value to promote is assumed to be on top of the WASM stack.
    pub(super) fn emit_promotion(&mut self, actual: WasmType, expected: WasmType) {
        if actual == expected {
            return;
        }
        match (actual, expected) {
            (WasmType::I32, WasmType::I64) => {
                self.ctx().emit(&Instruction::I64ExtendI32S);
            }
            (WasmType::I32, WasmType::F64) => {
                self.ctx().emit(&Instruction::F64ConvertI32S);
            }
            (WasmType::I64, WasmType::F64) => {
                self.ctx().emit(&Instruction::F64ConvertI64S);
            }
            _ => {
                // No conversion needed or unsupported
            }
        }
    }
    
    /// src.contains '.' or 'e'/'E' to indicate it's a floating-point literal
    pub(super) fn is_floating_point(src: &String) -> bool {
        src.contains('.') || src.contains('e') || src.contains('E')
    }
}