use super::instructions::{Inst, Value};
use crate::scanner::token::Position;

pub struct Chunk {
    instructions: Vec<Inst>,
    constants: Vec<Value>,
    positions: Vec<Position>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: vec![],
            constants: vec![],
            positions: vec![],
        }
    }

    pub fn write(&mut self, inst: Inst, pos: Position) {
        self.instructions.push(inst);
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
        println!("----- begin {} -----", name);

        for (offset, inst) in self.instructions.iter().enumerate() {
            println!("{}", self.disassemble_inst(offset, inst));
        }

        println!("----- end {} -----", name);
    }

    pub fn inst_count(&self) -> usize {
        self.instructions.len()
    }

    // updates a jump instruction once the offset can be computed
    pub fn patch_jmp(&mut self, start_idx: usize) {
        let offset = self.inst_count() - start_idx;

        self.instructions[start_idx] = match self.inst_at(start_idx) {
            Inst::Jmp(_) => Inst::Jmp(offset),
            Inst::JmpIfTrue(_) => Inst::JmpIfTrue(offset),
            Inst::JmpIfFalse(_) => Inst::JmpIfFalse(offset),
            _ => panic!(
                "Chunk.patch_jmp expected a Jmp instruction, got: {:?}",
                self.inst_at(start_idx)
            ),
        };
    }

    pub fn inst_at(&self, idx: usize) -> &Inst {
        &self.instructions[idx]
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
            PopN(n) => format!("pop {}", n),
            DefGlobal(id) => format!("def global {}", id),
            GetGlobal(id) => format!("get global {}", id),
            SetGlobal(id) => format!("set global {}", id),
            GetLocal(idx) => format!("get local {}", idx),
            SetLocal(idx) => format!("set local {}", idx),
            Jmp(offset) => format!("jmp {}", offset),
            JmpIfTrue(offset) => format!("jmp if true {}", offset),
            JmpIfFalse(offset) => format!("jmp if false {}", offset),
            Loop(offset) => format!("jmp -{}", offset),
        };

        if offset > 0 && self.positions[offset - 1].line == self.positions[offset].line {
            format!("{:04x}  | {}", offset, d)
        } else {
            format!("{:04x} line {}: {}", offset, self.positions[offset].line, d)
        }
    }

    pub fn const_(&self, idx: usize) -> Option<&Value> {
        self.constants.get(idx)
    }

    pub fn instructions(&self) -> &[Inst] {
        &self.instructions
    }
 }
