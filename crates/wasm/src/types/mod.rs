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

use ast::types::TypeHint;
use wasm_encoder::ValType;
use std::fmt;

/// Represents a WASM-level type that a Python value maps to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WasmType {
    /// Python `int` → WASM `i64`
    I64,
    /// Python `float` → WASM `f64`
    F64,
    /// Python `bool` → WASM `i32` (0 or 1)
    I32,
    /// Python `None` / void return → no WASM value
    Void,
}

impl WasmType {
    /// Convert a `TypeHint` from the AST into a `WasmType`.
    /// Returns `None` for types not yet supported in the static backend (str, list, dict, etc.).
    pub fn from_type_hint(hint: &TypeHint) -> Option<WasmType> {
        match hint {
            TypeHint::Int => Some(WasmType::I64),
            TypeHint::Float => Some(WasmType::F64),
            TypeHint::Bool => Some(WasmType::I32),
            TypeHint::None => Some(WasmType::Void),
            // Compound and string types are not supported in the static backend yet
            TypeHint::Str
            | TypeHint::List(_)
            | TypeHint::Tuple(_)
            | TypeHint::Dict(_, _)
            | TypeHint::Set(_)
            | TypeHint::Optional(_)
            | TypeHint::Custom(_) => None,
        }
    }

    /// Convert to `wasm_encoder::ValType`. Returns `None` for `Void`.
    pub fn to_val_type(self) -> Option<ValType> {
        match self {
            WasmType::I64 => Some(ValType::I64),
            WasmType::F64 => Some(ValType::F64),
            WasmType::I32 => Some(ValType::I32),
            WasmType::Void => None,
        }
    }

    /// Returns `true` if this is a numeric type (I64 or F64).
    pub fn is_numeric(self) -> bool {
        matches!(self, WasmType::I64 | WasmType::F64)
    }

    /// Returns `true` if this is an integer type (I64 or I32).
    pub fn is_integer(self) -> bool {
        matches!(self, WasmType::I64 | WasmType::I32)
    }

    /// Numeric promotion: determines the result type of a binary arithmetic operation.
    /// int op int → int, float op float → float, int op float → float, bool promoted to int.
    pub fn numeric_promote(self, other: WasmType) -> Option<WasmType> {
        match (self, other) {
            (WasmType::I64, WasmType::I64) => Some(WasmType::I64),
            (WasmType::F64, WasmType::F64) => Some(WasmType::F64),
            (WasmType::I64, WasmType::F64) | (WasmType::F64, WasmType::I64) => {
                Some(WasmType::F64)
            }
            // Bool can participate in arithmetic as int
            (WasmType::I32, WasmType::I32) => Some(WasmType::I64),
            (WasmType::I32, WasmType::I64) | (WasmType::I64, WasmType::I32) => {
                Some(WasmType::I64)
            }
            (WasmType::I32, WasmType::F64) | (WasmType::F64, WasmType::I32) => {
                Some(WasmType::F64)
            }
            _ => None,
        }
    }
}

impl fmt::Display for WasmType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmType::I64 => write!(f, "int"),
            WasmType::F64 => write!(f, "float"),
            WasmType::I32 => write!(f, "bool"),
            WasmType::Void => write!(f, "None"),
        }
    }
}

/// Display a `TypeHint` in a human-friendly way for error messages.
pub fn type_hint_display(hint: &TypeHint) -> String {
    match hint {
        TypeHint::None => "None".to_string(),
        TypeHint::Int => "int".to_string(),
        TypeHint::Float => "float".to_string(),
        TypeHint::Str => "str".to_string(),
        TypeHint::Bool => "bool".to_string(),
        TypeHint::List(inner) => format!("list[{}]", type_hint_display(inner)),
        TypeHint::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| type_hint_display(e)).collect();
            format!("tuple[{}]", parts.join(", "))
        }
        TypeHint::Dict(k, v) => {
            format!("dict[{}, {}]", type_hint_display(k), type_hint_display(v))
        }
        TypeHint::Set(inner) => format!("set[{}]", type_hint_display(inner)),
        TypeHint::Optional(inner) => format!("Optional[{}]", type_hint_display(inner)),
        TypeHint::Custom(name) => name.clone(),
    }
}
