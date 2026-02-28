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
    pub(super) fn compile_list_get(&mut self, list_expr: &Expr, index: u32) {
        self.compile_expr(list_expr);
        self.ctx().emit(&Instruction::I32Const(index as i32));
        let i64_array_type_index = self.i64_array_type_index;
        self.ctx().emit(&Instruction::ArrayGet(i64_array_type_index));
    }
}