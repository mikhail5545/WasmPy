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
    pub(super) fn translate_binop(op: &BinOp) -> BinOpKind {
        match op{
            BinOp::Add => BinOpKind::Add,
            BinOp::Sub => BinOpKind::Sub,
            BinOp::Mul => BinOpKind::Mul,
            BinOp::Div => BinOpKind::Div,
            BinOp::FloorDiv => BinOpKind::FloorDiv,
            BinOp::Mod => BinOpKind::Mod,
            BinOp::Pow => BinOpKind::Pow,
            BinOp::BitAnd => BinOpKind::BitAnd,
            BinOp::BitOr => BinOpKind::BitOr,
            BinOp::BitXor => BinOpKind::BitXor,
            BinOp::LShift => BinOpKind::LShift,
            BinOp::RShift => BinOpKind::RShift,
            BinOp::And => BinOpKind::And,
            BinOp::Or => BinOpKind::Or,
            _ => BinOpKind::Add, // fallback
        }
    }

    pub(super) fn translate_unary_op(op: &UnaryOp) -> UnaryOpKind {
        match op{
            UnaryOp::Neg => UnaryOpKind::Neg,
            UnaryOp::Not => UnaryOpKind::Not,
            UnaryOp::Invert => UnaryOpKind::Invert,
            UnaryOp::Pos => UnaryOpKind::Pos,
        }
    }

    pub(super) fn translate_cmp_op(op: &CmpOp) -> CmpOpKind {
        match op{
            CmpOp::Eq => CmpOpKind::Eq,
            CmpOp::NotEq => CmpOpKind::NotEq,
            CmpOp::Lt => CmpOpKind::Lt,
            CmpOp::LtE => CmpOpKind::LtE,
            CmpOp::Gt => CmpOpKind::Gt,
            CmpOp::GtE => CmpOpKind::GtE,
            CmpOp::Is => CmpOpKind::Is,
            CmpOp::IsNot => CmpOpKind::IsNot,
            CmpOp::In => CmpOpKind::In,
            CmpOp::NotIn => CmpOpKind::NotIn,
        }
    }
}