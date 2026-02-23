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
}

impl CallFrame {
    pub fn new(instructions: Vec<Instruction>, return_reg: Option<u8>) -> Self{
        Self{
            instructions,
            pc: 0,
            registers: Box::new(std::array::from_fn(|_| Value::None)),
            return_reg,
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
    pub fn new(instructions: Vec<Instruction>) -> Self{
        Self{
            globals: HashMap::new(),
            call_stack: Vec::new(),
            functions: HashMap::new(),
        }
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

            if let Some(should_halt) = self.match_instruction(instruction) {
                if should_halt {
                    break; // Halt the VM execution
                }
            }
        }
    }

    fn match_instruction(&mut self, instruction: Instruction) -> Option<bool> {
        match instruction {
            Instruction::LoadConst(register, value) => {
                let frame = self.call_stack.last_mut().unwrap();
                frame.registers[register as usize] = value.clone();
                Some(false)
            }
            Instruction::LoadName(register, name) => {
                let value = self.globals.get(&name).cloned().unwrap_or(Value::None);
                self.call_stack.last_mut().unwrap().registers[register as usize] = value;
                Some(false)
            }
            Instruction::StoreName(name, register) => {
                let value = self.call_stack.last_mut().unwrap().registers[register as usize].clone();
                self.globals.insert(name.clone(), value);
                Some(false)
            }
            Instruction::BinOp(dest, op, left, right) => {
                let frame = self.call_stack.last_mut().unwrap();
                let result = Self::eval_binop(op, &frame.registers[left as usize], &frame.registers[right as usize]);
                frame.registers[dest as usize] = result;
                Some(false)
            }
            Instruction::UnaryOp(dest, op, src) => {
                let frame = self.call_stack.last_mut().unwrap();
                let result = Self::eval_unary_op(op, &frame.registers[src as usize]);
                frame.registers[dest as usize] = result;
                Some(false)
            }
            Instruction::Compare(dest, op, left, right) => {
                let frame = self.call_stack.last_mut().unwrap();
                let result = Self::eval_compare(op, &frame.registers[left as usize], &frame.registers[right as usize]);
                frame.registers[dest as usize] = result;
                Some(false)
            }
            Instruction::JumpIfFalse(cond_reg, target) => {
                let frame = self.call_stack.last_mut().unwrap();
                let cond_value = &frame.registers[cond_reg as usize];
                if !Self::is_truthy(cond_value) {
                    frame.pc = target; // Jump to target instruction index
                }
                Some(false)
            }
            Instruction::Jump(target) => {
                let frame = self.call_stack.last_mut().unwrap();
                frame.pc = target; // Unconditional jump to target instruction index
                Some(false)
            }
            Instruction::Move(dest_reg, src_reg) => {
                let frame = self.call_stack.last_mut().unwrap();
                frame.registers[dest_reg as usize] = frame.registers[src_reg as usize].clone();
                Some(false)
            }
            Instruction::Print(reg) => {
                let frame = self.call_stack.last_mut().unwrap();
                let value = &frame.registers[reg as usize];
                println!("{}", Self::display_value(value));
                Some(false)
            }
            Instruction::Call(dest_reg, func_reg, arg_start, arg_count) => {
                let caller = self.call_stack.last().unwrap();
                let func_value = caller.registers[func_reg as usize].clone();

                if let Value::Function(func_id) = func_value {
                    let func_instructions = self.functions.get(&func_id).expect("Function ID not found in function table").clone();

                    // Copy arguments from caller into new frame
                    let mut new_frame = CallFrame::new(func_instructions, Some(dest_reg));
                    for i in 0..arg_count{
                        new_frame.registers[i as usize] = caller.registers[(arg_start + i) as usize].clone();
                    }
                    self.call_stack.push(new_frame);
                } else {
                    panic!("Attempted to call a non-function value: {:?}", func_value);
                }
                Some(false)
            }
            Instruction::Return(reg) => {
                let return_value = self.call_stack.last().unwrap().registers[reg as usize].clone();
                let return_reg = self.call_stack.last().unwrap().return_reg;
                self.call_stack.pop();

                match(return_reg, self.call_stack.last_mut()) {
                    (Some(dest), Some(caller)) => {
                        caller.registers[dest as usize] = return_value;
                        Some(false)
                    }
                    _ => Some(true), // Returning from top-level frame or no caller, halt the VM
                }
            }
            Instruction::Halt => Some(true), // Explicit halt instruction, stop the VM
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
            Value::None => false,
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
            Value::None => "None".to_string(),
        }
    }

    /// Register a new function in the VM's function table, assigning it a unique ID and storing its instruction sequence.
    pub fn add_function(&mut self, instructions:Vec<Instruction>) -> usize {
        let id = self.functions.len();
        self.functions.insert(id, instructions);
        id
    }
}

