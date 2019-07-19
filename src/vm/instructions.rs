pub type Value = f64;

#[derive(Clone)]
pub enum Inst {
    Return,
    Const(usize),
}

impl Inst {
    pub fn len(&self) -> u8 {
        use Inst::*;
        match self {
            Return => 1,
            Const(_) => 2,
        }
    }
}
