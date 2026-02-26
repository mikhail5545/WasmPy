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
use super::*;

// This module contains the implementations of the instruction handlers for the RegisterVM.
// Each handler corresponds to an instruction defined in the `Instruction` enum and manipulates the VM's state

impl RegisterVM{
    pub(super) fn op_load_const(&mut self, reg: u8, value: Value) -> bool{
        let frame =  self.call_stack.last_mut().unwrap();
        frame.registers[reg as usize] = value;
        false
    }

    pub(super) fn op_load_global(&mut self, reg: u8, name: String) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let value = self.globals.get(&name).cloned().unwrap_or(Value::None);
        frame.registers[reg as usize] = value;
        false
    }

    pub(super) fn op_load_local(&mut self, reg: u8, name: String) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let value = frame.locals.get(&name).cloned().unwrap_or(Value::None);
        frame.registers[reg as usize] = value;
        false
    }

    pub(super) fn op_store_global(&mut self, name: String, reg: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        self.globals.insert(name.clone(), frame.registers[reg as usize].clone());
        false
    }

    pub(super) fn op_store_local(&mut self, name: String, reg: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        frame.locals.insert(name.clone(), frame.registers[reg as usize].clone());
        false
    }

    pub(super) fn op_bin_op(&mut self, dest: u8, op: BinOpKind, left: u8, right: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let res = Self::eval_binop(op, &frame.registers[left as usize], &frame.registers[right as usize]);
        frame.registers[dest as usize] = res;
        false
    }

    pub(super) fn op_unary_op(&mut self, dest: u8, op: UnaryOpKind, src: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let res = Self::eval_unary_op(op, &frame.registers[src as usize]);
        frame.registers[dest as usize] = res;
        false
    }

    pub(super) fn op_cmp_op(&mut self, dest: u8, op: CmpOpKind, left: u8, right: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let res = Self::eval_compare(op, &frame.registers[left as usize], &frame.registers[right as usize]);
        frame.registers[dest as usize] = res;
        false
    }
    
    pub(super) fn op_jump_if_false(&mut self, cond_src: u8, target: usize) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let cond = &frame.registers[cond_src as usize];
        if !Self::is_truthy(cond) {
            frame.pc = target;
        }
        false
    }
    
    pub(super) fn op_jump(&mut self, target: usize) -> bool {
        let frame = self.call_stack.last_mut().unwrap();
        frame.pc = target;
        false
    }
    
    pub(super) fn op_move(&mut self, src: u8, dest: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        frame.registers[dest as usize] = frame.registers[src as usize].clone();
        false
    }
    
    pub(super) fn op_build_list(&mut self, dest: u8, start: u8, count: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let elements = (start..start+count).map(|i| frame.registers[i as usize].clone()).collect();
        frame.registers[dest as usize] = Value::List(std::rc::Rc::new(std::cell::RefCell::new(elements)));
        false
    }
    
    pub(super) fn op_build_tuple(&mut self, dest: u8, start: u8, count: u8) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let elements = (start..start+count).map(|i| frame.registers[i as usize].clone()).collect();
        frame.registers[dest as usize] = Value::Tuple(elements);
        false
    }
    
    pub(super) fn op_call(&mut self, dest_reg: u8, func_reg: u8, arg_start: u8, arg_count: u8) -> bool{
        let caller = self.call_stack.last().unwrap();
        let func_value = caller.registers[func_reg as usize].clone();
        
        let mut args = Vec::with_capacity(arg_count as usize);
        for i in 0..arg_count {
            args.push(caller.registers[(arg_start + i) as usize].clone());
        }
        
        match func_value {
            Value::Function(func_id) => {
                let func_instructions = self.functions.get(&func_id).expect("Function ID not found in function table").clone();

                // Copy arguments from caller into new frame
                let mut new_frame = CallFrame::new(func_instructions, Some(dest_reg));
                for i in 0..arg_count{
                    new_frame.registers[i as usize] = caller.registers[(arg_start + i) as usize].clone();
                }
                self.call_stack.push(new_frame);
                false
            }
            Value::NativeFunc(native_fn) => {
                match (native_fn.func)(&args) {
                    Ok(result) => {
                        self.call_stack.last_mut().unwrap().registers[func_reg as usize] = result.clone();
                        false
                    }
                    Err(e) => {
                        self.handle_runtime_error(e)
                    }
                }
            }
            _ => {
                let err = RuntimeError::TypeError(format!("Attempted to call a non-function value: {:?}", RegisterVM::display_value(&func_value)));
                self.handle_runtime_error(err)
            }
        }
    }
    
    pub(super) fn op_return(&mut self, src_reg: u8) -> bool{
        let frame = self.call_stack.last().unwrap();
        let return_value = frame.registers[src_reg as usize].clone();
        let return_reg = frame.return_reg;
        self.call_stack.pop();

        match(return_reg, self.call_stack.last_mut()) {
            (Some(dest), Some(caller)) => {
                caller.registers[dest as usize] = return_value;
                false
            }
            _ => true, // Returning from top-level frame or no caller, halt the VM
        }
    }
    
    pub(super) fn op_load_attr(&mut self, dest: u8, src: u8, name: String) -> bool{
        let frame = self.call_stack.last_mut().unwrap();
        let target = &frame.registers[dest as usize];
        
        if let Some(method) = Self::get_attribute(target, &name) {
            frame.registers[dest as usize] = method;
        } else {
            panic!("Attribute '{}' not found on value: {:?}", name, target);
        }
        false
    }
}