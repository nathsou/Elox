pub mod chunk;
pub mod instructions;

use chunk::Chunk;
use instructions::Inst;

pub fn test() {
    let mut ch = Chunk::new();
    let idx = ch.write_constant(1.2f64, 123);
    ch.write(Inst::Return, 123);
    ch.disassemble("test");
}
