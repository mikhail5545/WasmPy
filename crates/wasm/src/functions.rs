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
use wasm_encoder::{Encode, Function, Instruction, ValType};
use std::collections::HashMap;
use crate::types::WasmType;


/// Context for compiling a single WASM function body.
///
/// Because `wasm-encoder::Function` demands all local declarations at construction
/// time, this context buffers instructions and local declarations separately.
/// Call [`FunctionContext::build`] after compilation to produce the final `Function`.
pub struct FunctionContext {
    /// Mapping of local variable names to their WASM local indices
    locals: HashMap<String, u32>,
    /// Mapping of local variable names to their WASM types
    local_types: HashMap<String, WasmType>,
    /// Ordered list of local types (excluding parameters). Index 0 here = param_count in WASM.
    extra_local_types: Vec<ValType>,
    /// Number of parameters (params occupy indices 0..param_count)
    #[allow(dead_code)]
    param_count: u32,
    /// The next available local index (starts after parameters)
    next_local_idx: u32,
    /// Buffered raw instruction bytes
    instruction_bytes: Vec<u8>,
}

impl FunctionContext {
    /// Create a new function context. `param_names` and `param_types` are parallel arrays.
    /// Parameters are automatically registered as locals at indices 0..N.
    pub fn new(param_names: &[String], param_wasm_types: &[WasmType]) -> Self {
        let mut locals = HashMap::new();
        let mut local_types = HashMap::new();

        for (i, (name, wasm_ty)) in param_names.iter().zip(param_wasm_types.iter()).enumerate() {
            locals.insert(name.clone(), i as u32);
            local_types.insert(name.clone(), *wasm_ty);
        }

        let param_count = param_names.len() as u32;

        Self {
            locals,
            local_types,
            extra_local_types: Vec::new(),
            param_count,
            next_local_idx: param_count,
            instruction_bytes: Vec::new(),
        }
    }

    /// Create a minimal function context for module-level code (no params).
    pub fn new_start() -> Self {
        Self {
            locals: HashMap::new(),
            local_types: HashMap::new(),
            extra_local_types: Vec::new(),
            param_count: 0,
            next_local_idx: 0,
            instruction_bytes: Vec::new(),
        }
    }

    /// Declare a new local variable and return its WASM local index.
    /// If the variable is already declared, returns its existing index.
    pub fn declare_local(&mut self, name: String, wasm_type: WasmType) -> u32 {
        if let Some(&existing_idx) = self.locals.get(&name) {
            return existing_idx;
        }
        let val_type = wasm_type
            .to_val_type()
            .expect("Cannot declare a local with Void type");
        let idx = self.next_local_idx;
        self.extra_local_types.push(val_type);
        self.locals.insert(name.clone(), idx);
        self.local_types.insert(name, wasm_type);
        self.next_local_idx += 1;
        idx
    }

    /// Get the WASM local index for a given variable name.
    pub fn get_local(&self, name: &str) -> Option<u32> {
        self.locals.get(name).copied()
    }

    /// Get the WASM type of a local variable.
    pub fn get_local_type(&self, name: &str) -> Option<WasmType> {
        self.local_types.get(name).copied()
    }

    /// Emit an instruction to the function body (buffered).
    pub fn emit(&mut self, instr: &Instruction) {
        instr.encode(&mut self.instruction_bytes);
    }

    /// Emit the `End` instruction to close the function body.
    pub fn emit_end(&mut self) {
        Instruction::End.encode(&mut self.instruction_bytes);
    }

    /// Build the final `wasm_encoder::Function` from the collected locals and instructions.
    pub fn build(self) -> Function {
        // Group consecutive locals of the same type for compact encoding
        let mut grouped: Vec<(u32, ValType)> = Vec::new();
        for vt in &self.extra_local_types {
            if let Some((count, last_type)) = grouped.last_mut() {
                if *last_type == *vt {
                    *count += 1;
                    continue;
                }
            }
            grouped.push((1, *vt));
        }

        let mut func = Function::new(grouped);
        func.raw(self.instruction_bytes);
        func
    }
}

