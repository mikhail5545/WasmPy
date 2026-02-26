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

use crate::error::RuntimeError;

#[derive(Debug, Clone)]
pub enum Instruction{
    /// Load a constant into a register: reg = value
    LoadConst(u8, Value),
    /// Load a variable into a register: reg = var_name
    LoadGlobal(u8, String),
    LoadLocal(u8, String), // for function scopes: reg = var_name
    /// Store a register's value into a variable: var_name = reg
    StoreGlobal(String, u8),
    /// Store a register's value into a local variable (for function scopes): var_name = reg
    StoreLocal(String, u8),
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
    /// Move reg_dst = reg_src (used for variable assignment or copying values)
    Move(u8, u8),
    /// Build a tuple from consecutive registers `[reg_start..reg_start+count]`, store in reg_dest -> reg_dest, reg_start, count
    BuildTuple(u8, u8, u8),
    /// Build a list from consecutive registers `[reg_start..reg_start+count]`, store in reg_dest -> reg_dest, reg_start, count
    BuildList(u8, u8, u8),
    /// Load an attribute 'name' from an object in reg_obj, store in reg_dest -> reg_dest, reg_obj, attr_name
    LoadAttr(u8, u8, String),
    /// Halt the VM execution
    Halt,
}

pub struct NativeFn{
    pub func: Box<dyn Fn(&[Value]) -> Result<Value, RuntimeError>>,
}

impl std::fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFn")
            .finish()
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, _other: &Self) -> bool {
        // Native functions are not comparable, so we return false
        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value{
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Tuple(Vec<Value>),
    List(std::rc::Rc<std::cell::RefCell<Vec<Value>>>),
    Function(usize), // Index into function table
    /// Unified representation for both built-in global functions (print) and attribute functions (obj.method)
    NativeFunc(std::rc::Rc<NativeFn>),
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