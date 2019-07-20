pub mod chunk;
mod compiler;
pub mod instructions;

use crate::interpreter::eval_result::EvalError;
use crate::interpreter::host::Host;
use crate::parser::expressions::{BinaryOperator, UnaryOperator};
use crate::parser::{IdentifierHandle, IdentifierHandlesGenerator};
use crate::runner::{EloxError, EloxResult, EloxRunner};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use chunk::Chunk;
use fnv::FnvHashMap;
use instructions::{Inst, Obj, Value};
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;

macro_rules! binary_op {
    (==, $self: tt) => ({
        let b = $self.pop();
        let a = $self.pop();
        $self.push(Value::Boolean(a == b));
    });

    (!=, $self: tt) => ({
        let b = $self.pop();
        let a = $self.pop();
        $self.push(Value::Boolean(a != b));
    });

    (+, $self: tt) => ({
        let b = $self.pop();
        let a = $self.pop();
        match (&a, &b) {
            (&Value::Number(a), &Value::Number(b)) => $self.push(Value::Number(a + b)),
            _ => {
                let obj = Value::new_str(&format!("{}{}", a, b), &mut $self.strings);
                $self.push(obj);
            },
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
    stack: EloxVMStack,
    identifiers: IdentifierHandlesGenerator,
    strings: FnvHashMap<String, Rc<Obj>>,
    globals: FnvHashMap<IdentifierHandle, Value>,
}

impl EloxVM {
    pub fn new() -> EloxVM {
        EloxVM {
            host: Rc::new(Host::default()),
            chunk: Chunk::new(),
            ip: 0,
            stack: EloxVMStack::with_capacity(256),
            identifiers: IdentifierHandlesGenerator::new(),
            strings: FnvHashMap::default(),
            globals: FnvHashMap::default(),
        }
    }

    pub fn launch(&mut self) -> EloxResult {
        loop {
            self.trace();
            let inst = self.chunk.inst_at(self.ip);
            match inst {
                Inst::Ret => {
                    return Ok(());
                }
                Inst::Pop => {
                    self.pop();
                }
                Inst::Global(id_handle) => {
                    // we peek instead of popping to ensure that the VM still has
                    // access to the value while inserting it to the globals HashMap
                    // in the event of a gc while inserting
                    let val = self.stack.peek().clone();
                    self.globals.insert(*id_handle, val);
                    self.pop();
                }
                Inst::GetGlobal(id_handle) => {
                    let var = self.globals.get(&id_handle);
                    if let Some(var) = var {
                        let var = var.clone();
                        self.push(var);
                    } else {
                        return Err(EloxError::Eval(EvalError::UndefinedVariable(
                            self.pos(),
                            self.identifiers.name(*id_handle),
                        )));
                    }
                }
                Inst::SetGlobal(id_handle) => {
                    if !self.globals.contains_key(&id_handle) {
                        return Err(EloxError::Eval(EvalError::UndefinedVariable(
                            self.pos(),
                            self.identifiers.name(*id_handle),
                        )));
                    }

                    let val = self.stack.peek().clone();
                    self.globals.insert(*id_handle, val);
                }
                Inst::Print => {
                    println!("{}", self.pop());
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
                Inst::Equ => binary_op!(==, self),
                Inst::Neq => binary_op!(!=, self),
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
        self.stack.pop()
    }

    #[allow(dead_code)]
    fn trace(&self) {
        println!("{}", self.stack);
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

// peekable stack https://docs.rs/itertools/0.8.0/src/itertools/multipeek_impl.rs.html#20-28
pub struct EloxVMStack {
    stack: Vec<Value>,
    peek_idx: usize,
    peek_buf: VecDeque<Value>,
}

impl EloxVMStack {
    pub fn new() -> EloxVMStack {
        EloxVMStack::with_capacity(0)
    }

    pub fn with_capacity(cap: usize) -> EloxVMStack {
        EloxVMStack {
            stack: Vec::with_capacity(cap),
            peek_idx: 0,
            peek_buf: VecDeque::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
        self.reset_peek();
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.peek_idx = 0;
        if self.peek_buf.is_empty() {
            self.stack.pop().expect("tried to pop an empty stack")
        } else {
            self.peek_buf
                .pop_front()
                .expect("tried to pop an empty stack")
        }
    }

    #[inline]
    pub fn reset_peek(&mut self) {
        if !self.peek_buf.is_empty() {
            self.peek_buf.clear();
            self.peek_idx = 0;
        }
    }

    pub fn peek(&mut self) -> &Value {
        if self.peek_idx >= self.peek_buf.len() {
            let val = self.stack.pop().expect("tried to peek an empty stack");
            self.peek_buf.push_back(val);
        }

        let ret = &self.peek_buf[self.peek_idx];

        self.peek_idx += 1;

        ret
    }
}

impl fmt::Display for EloxVMStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.stack
                .iter()
                .map(|val| format!("{}", val))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
