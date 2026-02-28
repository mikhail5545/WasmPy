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
    pub(super) fn compile_string_literal(&mut self, s: &str)  -> WasmType {
        let bytes = s.as_bytes().to_vec();
        
        let string_type_idx = self.string_type_index;
        self.data_section.passive(bytes.clone());
        let data_index = self.next_data_index;
        self.next_data_index += 1;
        
        self.ctx().emit(&Instruction::ArrayNewData {
            array_type_index: string_type_idx,
            array_data_index: data_index,
        });
        
        WasmType::StringRef
    }
    
    pub(super) fn compile_list_literal(&mut self, elements: &[Spanned<Expr>]) -> WasmType{
        for el in elements {
            let ty = self.compile_expr(&el.node);
            if ty != WasmType::I64 {
                panic!("Only i64 literals are supported in list literals for now, found {:?}", ty);
            }
        }
        
        // Creates array of length N using top N elements on the stack as initial values
        let i64_array_type_idx = self.i64_array_type_index;
        self.ctx().emit(&Instruction::ArrayNewFixed{
            array_type_index: i64_array_type_idx,
            array_size: elements.len() as u32,
        });
        
        WasmType::ListRef
    }
}