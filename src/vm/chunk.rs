use super::instructions::{Inst, Value};
use crate::scanner::token::Position;

pub struct Chunk {
    code: Vec<Inst>,
    constants: Vec<Value>,
    positions: Vec<Position>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            positions: vec![],
        }
    }

    pub fn write(&mut self, inst: Inst, pos: Position) {
        self.code.push(inst);
        self.positions.push(pos);
    }

    pub fn write_constant(&mut self, val: Value, pos: Position) {
        let idx = self.add_const(val);
        self.write(Inst::Const(idx), pos);
    }

    fn add_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }

    pub fn read_const(&self, idx: &usize) -> Value {
        self.constants[*idx].clone()
    }

    pub fn disassemble(&self, name: &str) {
        println!("----- {} -----", name);

        let mut byte_offset = 0;

        for (offset, inst) in self.code.iter().enumerate() {
            println!(
                "{:04x} {}",
                byte_offset,
                self.disassemble_inst(offset, inst)
            );
            byte_offset += inst.len();
        }
    }

    pub fn inst_at(&self, idx: usize) -> &Inst {
        &self.code[idx]
    }

    pub fn pos_at(&self, idx: usize) -> Position {
        self.positions[idx]
    }

    pub fn disassemble_inst(&self, offset: usize, inst: &Inst) -> String {
        use Inst::*;
        let d = match inst {
            Ret => format!("ret"),
            Const(idx) => format!("const {} {}", idx, self.constants[*idx]),
            Neg => format!("neg"),
            Add => format!("add"),
            Sub => format!("sub"),
            Mult => format!("mult"),
            Div => format!("div"),
            Mod => format!("mod"),
            Not => format!("not"),
            True => format!("true"),
            False => format!("false"),
            Nil => format!("nil"),
            Equ => format!("equ"),
            Neq => format!("neq"),
            Gtr => format!("gtr"),
            Lss => format!("lss"),
            Gtq => format!("gtq"),
            Leq => format!("leq"),
            Print => format!("print"),
            Pop => format!("pop"),
            Global(_) => format!("global"),
            GetGlobal(_) => format!("get global"),
            SetGlobal(_) => format!("set global"),
        };

        if offset > 0 && self.positions[offset - 1].line == self.positions[offset].line {
            format!("  | {}", d)
        } else {
            format!("{} {}", self.positions[offset], d)
        }
    }
}
