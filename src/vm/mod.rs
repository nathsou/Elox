pub mod chunk;
mod compiler;
pub mod instructions;

use crate::interpreter::eval_result::EvalError;
use crate::interpreter::host::Host;
use crate::parser::expressions::{BinaryOperator, UnaryOperator};
use crate::runner::{EloxError, EloxResult, EloxRunner};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use chunk::Chunk;
use instructions::{Inst, Value};
use std::rc::Rc;

macro_rules! binary_op {
    (==, $self: tt, $bin_op: expr, $ValType: ident) => ({
        let b = $self.pop();
        let a = $self.pop();
        $self.push(Value::Boolean(a == b));
    });

    (!=, $self: tt, $bin_op: expr, $ValType: ident) => ({
        let b = $self.pop();
        let a = $self.pop();
        $self.push(Value::Boolean(a != b));
    });

    (+, $self: tt) => ({
        let b = $self.pop();
        let a = $self.pop();
        match (&a, &b) {
            (&Value::Number(a), &Value::Number(b)) => $self.push(Value::Number(a + b)),
            _ => $self.push(Value::new_str(&format!("{}{}", a, b))),
        };
    });

    ($op: tt, $self: tt, $bin_op: expr, $ValType: ident) => ({
        let b = $self.pop();
        let a = $self.pop();
        match (&a, &b) {
            (&Value::Number(a), &Value::Number(b)) => $self.push(Value::$ValType(a $op b)),
            _ => return Err(EloxError::Eval(EvalError::UnexpectedBinaryOperatorOperands($self.pos(), $bin_op, a.type_(), b.type_()))),
        };
    });
}

pub struct EloxVM {
    host: Rc<Host>,
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl EloxVM {
    pub fn new() -> EloxVM {
        EloxVM {
            host: Rc::new(Host::default()),
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(256),
        }
    }

    pub fn launch(&mut self) -> EloxResult {
        loop {
            self.trace();
            match self.chunk.inst_at(self.ip) {
                Inst::Ret => {
                    let val = self.pop();
                    println!("{}", val);
                    return Ok(());
                }
                Inst::Const(idx) => {
                    let val = self.chunk.read_const(idx);
                    self.push(val);
                }
                Inst::Neg => {
                    let val = self.pop();
                    if let Value::Number(nb) = val {
                        self.push(Value::Number(-nb));
                    } else {
                        return Err(EloxError::Eval(EvalError::UnexpectedUnaryOperatorOperand(
                            self.pos(),
                            UnaryOperator::Minus,
                            val.type_(),
                        )));
                    }
                }
                Inst::Not => {
                    let val = self.pop();
                    self.push(Value::Boolean(!val.is_truthy()));
                }
                Inst::Add => binary_op!(+, self),
                Inst::Sub => binary_op!(-, self, BinaryOperator::Minus, Number),
                Inst::Mult => binary_op!(*, self, BinaryOperator::Star, Number),
                Inst::Div => binary_op!(/, self, BinaryOperator::Slash, Number),
                Inst::Mod => binary_op!(%, self, BinaryOperator::Percent, Number),
                Inst::True => self.push(Value::Boolean(true)),
                Inst::False => self.push(Value::Boolean(false)),
                Inst::Nil => self.push(Value::Nil),
                Inst::Equ => binary_op!(==, self, BinaryOperator::EqualEqual, Boolean),
                Inst::Neq => binary_op!(!=, self, BinaryOperator::BangEqual, Boolean),
                Inst::Gtr => binary_op!(>, self, BinaryOperator::Greater, Boolean),
                Inst::Lss => binary_op!(<, self, BinaryOperator::Less, Boolean),
                Inst::Gtq => binary_op!(>=, self, BinaryOperator::GreaterEqual, Boolean),
                Inst::Leq => binary_op!(<=, self, BinaryOperator::GreaterEqual, Boolean),
            }
            self.ip += 1;
        }
    }

    fn pos(&self) -> Position {
        self.chunk.pos_at(self.ip)
    }

    #[inline(always)]
    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline(always)]
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("tried to pop an empty stack")
    }

    #[allow(dead_code)]
    fn trace(&self) {
        println!(
            "[{}]",
            self.stack
                .iter()
                .map(|val| format!("{}", val))
                .collect::<Vec<_>>()
                .join(", ")
        );
        println!(
            "{}",
            self.chunk
                .disassemble_inst(self.ip, self.chunk.inst_at(self.ip))
        );
    }
}

impl EloxRunner for EloxVM {
    fn run(&mut self, source: &str) -> EloxResult {
        self.compile(source)?;
        self.launch()?;
        Ok(())
    }

    fn throw_error(&mut self, err: impl ErrorPosition) {
        let pos = err.position();
        (self.host.error)(format!("{}", err), pos.line, pos.col);
    }
}
