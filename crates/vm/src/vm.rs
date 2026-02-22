pub enum Instruction{
    Load(usize, i64),
    Add(usize, usize, usize),
    Print(usize),
    Halt,
}

pub struct RegisterVM{
    registers: [i64; 256],
    instructions: Vec<Instruction>,
    ip: usize,
}

