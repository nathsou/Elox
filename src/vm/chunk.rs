use super::instructions::{Inst, Value};

pub struct Chunk {
    code: Vec<Inst>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, inst: Inst, line: usize) {
        self.code.push(inst);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, val: Value, line: usize) {
        let idx = self.add_const(val);
        self.write(Inst::Const(idx), line);
    }

    fn add_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
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

    pub fn disassemble_inst(&self, offset: usize, inst: &Inst) -> String {
        use Inst::*;
        let d = match inst {
            Return => format!("return"),
            Const(idx) => format!("const {} {}", idx, self.constants[*idx]),
        };

        if offset > 0 && self.lines[offset - 1] == self.lines[offset] {
            format!("  | {}", d)
        } else {
            format!("{} {}", self.lines[offset], d)
        }
    }
}
