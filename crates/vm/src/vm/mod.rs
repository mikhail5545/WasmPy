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

mod instruction_handlers;

use std::collections::HashMap;
use crate::instruction::*;

struct CallFrame{
    /// The function's instructions to execute. The frame's program counter (pc) tracks which instruction is currently being executed.
    pub instructions: Vec<Instruction>,
    /// The program counter (pc) is an index into the instructions vector, indicating the next instruction to execute.
    pub pc: usize,
    /// The frame's own register file, which holds the function's local variables and intermediate values. Each register is indexed by u8 (0-255).
    pub registers: Box<[Value; 256]>,
    /// Where to store the return value in the CALLER's register file.
    /// This is None for the top-level frame, and Some(reg) for function calls, indicating which register in the caller's frame should receive the return value when this frame returns.
    pub return_reg: Option<u8>,
    /// A mapping of local variable names to their values, used for variable storage and lookup within the function.
    /// This allows for named variables in addition to register-based storage.
    pub locals: HashMap<String, Value>,
}

impl CallFrame {
    pub fn new(instructions: Vec<Instruction>, return_reg: Option<u8>) -> Self{
        Self{
            instructions,
            pc: 0,
            registers: Box::new(std::array::from_fn(|_| Value::None)),
            return_reg,
            locals: HashMap::new(),
        }
    }
}

/// A simple register-based virtual machine that executes a sequence of instructions.
pub struct RegisterVM{
    /// A mapping of global variable names to their values, used for variable storage and lookup during execution.
    globals: HashMap<String, Value>,
    /// A mapping of function names to their instruction sequences, allowing the VM to execute function calls.
    /// Each function is associated with a vector of instructions that define its behavior.
    functions: HashMap<usize, Vec<Instruction>>,
    /// A call stack to manage function calls and returns, where each frame contains the function's instructions,
    /// its own register file, and the return register information.
    call_stack: Vec<CallFrame>,
}

impl RegisterVM{
    pub fn new() -> Self{
        let mut globals = HashMap::new();

        crate::builtin::globals::register_globals(&mut globals);

        Self{
            globals,
            call_stack: Vec::new(),
            functions: HashMap::new(),
        }
    }

    pub fn load(&mut self, result: crate::compiler::CompilationResult) {
        for (i, func_code) in result.functions.into_iter().enumerate() {
            self.functions.insert(i, func_code.instructions);
        }

        let main_frame = CallFrame::new(result.main.instructions, None);

        self.call_stack.clear();
        self.call_stack.push(main_frame);
    }

    pub fn run(&mut self) {
        loop {
            // Get the current frame (the top of the call stack)
            let frame = match self.call_stack.last_mut() {
                Some(frame) => frame,
                None => break, // No more frames to execute, halt the VM
            };

            if frame.pc >= frame.instructions.len() {
                // Implicit return None at the end of the function if no explicit return is encountered
                let return_reg = frame.return_reg;
                self.call_stack.pop(); // Pop the current frame
                if let (Some(dest), Some(caller)) = (return_reg, self.call_stack.last_mut()) {
                    caller.registers[dest as usize] = Value::None; // Store None in caller's return register
                }
                continue;
            }

            let instruction = frame.instructions[frame.pc].clone();
            frame.pc += 1; // Increment program counter before executing instruction

            if self.match_instruction(instruction) {
                break; // If the instruction signals to halt the VM, break the loop
            }
        }
    }

    fn get_attribute(target: &Value, name: &str) -> Option<Value> {
        crate::builtin::get_attribute(target, name)
    }

    fn handle_runtime_error(&mut self, error: crate::error::RuntimeError) -> bool{
        println!("Runtime error: {:?}", error);


        // TODO: In the future, loop through self.call_stack here.
        // If a "try/catch" block is found (stored in CallFrame metadata),
        // move pc to the 'catch' block and push the error to a register.

        println!("Traceback (most recent call last):");
        for frame in &self.call_stack {
            println!("  at instruction index {} in function with instructions: {:?}", frame.pc, frame.instructions);
        }
        println!("{:?}", error);
        true // Halt the VM after an unhandled runtime error
    }

    /// Dispatches the given instruction to the appropriate handler method based on its type.
    /// Each instruction variant is matched and the corresponding operation is executed.
    fn match_instruction(&mut self, instruction: Instruction) -> bool {
        match instruction {
            Instruction::LoadConst(register, value) => self.op_load_const(register, value),
            Instruction::LoadGlobal(register, name) => self.op_load_global(register, name),
            Instruction::LoadLocal(register, name) => self.op_load_local(register, name),
            Instruction::StoreGlobal(name, register) => self.op_store_global(name, register),
            Instruction::StoreLocal(name, register) => self.op_store_local(name, register),
            Instruction::BinOp(dest, op, left, right) => self.op_bin_op(dest, op, left, right),
            Instruction::UnaryOp(dest, op, src) => self.op_unary_op(dest, op, src),
            Instruction::Compare(dest, op, left, right) => self.op_cmp_op(dest, op, left, right),
            Instruction::JumpIfFalse(cond_reg, target) => self.op_jump_if_false(cond_reg, target),
            Instruction::Jump(target) => self.op_jump(target),
            Instruction::Move(dest_reg, src_reg) => self.op_move(dest_reg, src_reg),
            Instruction::BuildList(dest_reg, start_reg, count) => self.op_build_list(dest_reg, start_reg, count),
            Instruction::BuildTuple(dest_reg, start_reg, count) => self.op_build_tuple(dest_reg, start_reg, count),
            Instruction::Call(dest_reg, func_reg, arg_start, arg_count) => self.op_call(dest_reg, func_reg, arg_start, arg_count),
            Instruction::Return(reg) => self.op_return(reg),
            Instruction::Halt => true, // Explicit halt instruction, stop the VM
            _ => false,
        }
    }

    /// Evaluates a binary operation on two values, returning the result as a new Value. 
    /// This function handles type checking and operator semantics for supported operations.
    fn eval_binop(op: BinOpKind, left: &Value, right: &Value) -> Value{
        match (op, left, right) {
            // --- Value::Int ---
            (BinOpKind::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (BinOpKind::Sub, Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (BinOpKind::Mul, Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            // Note: this is integer division, which truncates value towards zero (default '/' rust behaviour). For example, -3 // 2 == -1, while in Python -3 // 2 == -2.
            (BinOpKind::Div, Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (BinOpKind::Pow, Value::Int(a), Value::Int(b)) => Value::Int(a.pow(*b as u32)),
            (BinOpKind::Mod, Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            // TODO: Add floor division for integers and floating point numbers
            (BinOpKind::FloorDiv, Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            // --- Value::Float ---
            (BinOpKind::Add, Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (BinOpKind::Sub, Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (BinOpKind::Mul, Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            // Note: this is floating point division, which does not truncate value. For example, -3.0 / 2.0 == -1.5, while in Python -3.0 // 2.0 == -2.0.
            (BinOpKind::Div, Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            // --- String concatenation ---
            (BinOpKind::Add, Value::Str(a), Value::Str(b)) => Value::Str(format!("{}{}", a, b)),
            // --- Boolean operations ---
            (BinOpKind::And, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && *b),
            (BinOpKind::Or, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || *b),
            _ => Value::None, // For unsupported operations, return None
        }
    }
    
    /// Evaluates a unary operation on a value, returning the result as a new Value.
    fn eval_unary_op(op: UnaryOpKind, operand: &Value) -> Value{
        match (op, operand) {
            (UnaryOpKind::Neg, Value::Int(a)) => Value::Int(-a),
            (UnaryOpKind::Neg, Value::Float(a)) => Value::Float(-a),
            (UnaryOpKind::Not, v) => Value::Bool(!Self::is_truthy(v)),
            (UnaryOpKind::Invert, Value::Int(a)) => Value::Int(!a),
            _ => Value::None, // For unsupported operations, return None
        }
    }
    
    /// Evaluates a comparison operation between two values, returning the result as a new Value::Bool. 
    /// This function handles type checking and operator semantics for supported comparisons.
    fn eval_compare(op: CmpOpKind, left: &Value, right: &Value) -> Value{
        match(op, left, right) {
            // --- Value::Int ---
            (CmpOpKind::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
            (CmpOpKind::NotEq, Value::Int(a), Value::Int(b)) => Value::Bool(a != b),
            (CmpOpKind::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
            (CmpOpKind::Gt, Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
            (CmpOpKind::LtE, Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
            (CmpOpKind::GtE, Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
            // --- Value::Float ---
            (CmpOpKind::Eq, Value::Float(a), Value::Float(b)) => Value::Bool(a == b),
            (CmpOpKind::NotEq, Value::Float(a), Value::Float(b)) => Value::Bool(a != b),
            (CmpOpKind::Lt, Value::Float(a), Value::Float(b)) => Value::Bool(a < b),
            (CmpOpKind::Gt, Value::Float(a), Value::Float(b)) => Value::Bool(a > b),
            (CmpOpKind::LtE, Value::Float(a), Value::Float(b)) => Value::Bool(a <= b),
            (CmpOpKind::GtE, Value::Float(a), Value::Float(b)) => Value::Bool(a >= b),
            // --- Value::Str ---
            (CmpOpKind::Eq, Value::Str(a), Value::Str(b)) => Value::Bool(a == b),
            (CmpOpKind::NotEq, Value::Str(a), Value::Str(b)) => Value::Bool(a != b),
            (CmpOpKind::Lt, Value::Str(a), Value::Str(b)) => Value::Bool(a < b),
            (CmpOpKind::Gt, Value::Str(a), Value::Str(b)) => Value::Bool(a > b),
            (CmpOpKind::LtE, Value::Str(a), Value::Str(b)) => Value::Bool(a <= b),
            (CmpOpKind::GtE, Value::Str(a), Value::Str(b)) => Value::Bool(a >= b),
            // --- Value::Bool ---
            (CmpOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (CmpOpKind::NotEq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a != b),
            (CmpOpKind::Lt, Value::Bool(a), Value::Bool(b)) => Value::Bool(!a && *b), // False < True
            (CmpOpKind::Gt, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && !b), // True > False
            (CmpOpKind::LtE, Value::Bool(a), Value::Bool(b)) => Value::Bool(!a || *b), // False <= True, True <= True, False <= False
            (CmpOpKind::GtE, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || !b), // True >= False, True >= True, False
            // --- None comparisons ---
            (CmpOpKind::Eq, Value::None, Value::None) => Value::Bool(true),
            (CmpOpKind::NotEq, Value::None, Value::None) => Value::Bool(false),
            (CmpOpKind::Eq, Value::None, _) => Value::Bool(false),
            (CmpOpKind::Eq, _, Value::None) => Value::Bool(false),
            (CmpOpKind::NotEq, Value::None, _) => Value::Bool(true),
            (CmpOpKind::NotEq, _, Value::None) => Value::Bool(true),
            _ => Value::None, // For unsupported comparisons, return None
        }
    }

    /// Helper function to determine the "truthiness" of a value, used for boolean contexts like if conditions and while loops.
    fn is_truthy(value: &Value) -> bool{
        match value {
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Str(s) => !s.is_empty(),
            Value::Bool(b) => *b,
            Value::Function(_) => true, // Functions are always truthy
            Value::NativeFunc(_) => true,
            Value::None => false,
            _ => false,
        }
    }

    /// Helper function to convert a Value to a string for debugging or printing purposes.
    fn display_value(value: &Value) -> String{
        match value{
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Str(s) => s.clone(),
            Value::Bool(b) => (if *b { "True" } else { "False" }).to_string(),
            Value::Function(id) => format!("<function {}>", id),
            Value::NativeFunc(func) => format!("<native function {:p}>", func),
            Value::None => "None".to_string(),
            _ => "<unknown value>".to_string(),
        }
    }

    /// Register a new function in the VM's function table, assigning it a unique ID and storing its instruction sequence.
    pub fn add_function(&mut self, instructions:Vec<Instruction>) -> usize {
        let id = self.functions.len();
        self.functions.insert(id, instructions);
        id
    }
}

