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

#[derive(Debug, Clone)]
pub enum Instruction{
    /// Load a constant into a register: reg = value
    LoadConst(u8, Value),
    /// Load a variable into a register: reg = var_name
    LoadName(u8, String),
    /// Store a register's value into a variable: var_name = reg
    StoreName(String, u8),
    /// reg_dest = reg_left OP reg_right
    BinOp(u8, BinOpKind, u8, u8),
    /// reg_dest = OP reg_src (unary)
    UnaryOp(u8, UnaryOpKind, u8),
    /// reg_dest = reg_left OP reg_right (comparison)
    Compare(u8, CmpOpKind, u8, u8),
    /// If reg is false, jump to instruction index
    JumpIfFalse(u8, usize),
    /// Unconditional jump to instruction index
    Jump(usize),
    /// Call function in reg_func with arguments in consecutive registers `[arg_start..arg_start+arg_count]`, store result in reg_dest -> reg_dest, func_reg, arg_start, arg_count
    Call(u8, u8, u8, u8),
    /// Return value in register
    Return(u8),
    /// Print register value (for debugging or print() builtin)
    Print(u8),
    /// Move reg_dst = reg_src (used for variable assignment or copying values)
    Move(u8, u8),
    /// Halt the VM execution
    Halt,
}

#[derive(Debug, Clone)]
pub enum Value{
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Function(usize), // Index into function table
    None,
}

#[derive(Debug, Clone)]
pub enum BinOpKind{
    Add, Sub, Mul, Div, FloorDiv, Mod, Pow,
    BitAnd, BitOr, BitXor, LShift, RShift,
    And, Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpKind{
    Neg, Pos, Not, Invert,
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOpKind{
    Eq, NotEq, Lt, LtE, Gt, GtE, Is, IsNot, In, NotIn,
}