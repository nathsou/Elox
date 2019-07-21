pub mod chunk;
mod compiler;
pub mod instructions;

use crate::interpreter::eval_result::EvalError;
use crate::interpreter::host::Host;
use crate::parser::expressions::{BinaryOperator, UnaryOperator};
use crate::parser::{IdentifierHandle, IdentifierHandlesGenerator, Parser};
use crate::runner::{EloxError, EloxResult, EloxRunner};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use crate::scanner::Scanner;
use chunk::Chunk;
use compiler::Compiler;
use fnv::FnvHashMap;
use instructions::{Inst, Obj, Value};
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
            // self.trace();
            match self.chunk.inst_at(self.ip) {
                Inst::Ret => {
                    break;
                }
                Inst::Pop => {
                    self.pop();
                }
                Inst::PopN(n) => {
                    self.stack.pop_n(*n);
                }
                Inst::Jmp(offset) => {
                    self.ip += offset;
                    continue; // don't increment the ip at the end
                }
                Inst::Loop(offset) => {
                    self.ip -= offset;
                    continue;
                }
                Inst::JmpIfTrue(offset) => {
                    if self.stack.peek(0).is_truthy() {
                        self.ip += offset;
                        continue;
                    }
                }
                Inst::JmpIfFalse(offset) => {
                    if !self.stack.peek(0).is_truthy() {
                        self.ip += offset;
                        continue;
                    }
                }
                Inst::DefGlobal(id_handle) => {
                    // we peek instead of popping to ensure that the VM still has
                    // access to the value while inserting it to the globals HashMap
                    // in the event of a gc while inserting
                    let val = self.stack.peek(0);
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

                    let val = self.stack.peek(0);
                    self.globals.insert(*id_handle, val);
                }
                Inst::GetLocal(idx) => {
                    let val = self.stack.get(*idx);
                    self.push(val);
                }
                Inst::SetLocal(idx) => {
                    let val = self.stack.peek(0);
                    self.stack.set(*idx, val);
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
        debug_assert!(
            self.stack.size() == 0,
            "the stack must be empty at the end of execution",
        );

        Ok(())
    }

    #[inline]
    fn pos(&self) -> Position {
        self.chunk.pos_at(self.ip)
    }

    #[inline]
    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack.pop()
    }

    #[inline]
    #[allow(dead_code)]
    fn trace(&self) {
        println!("{}", self.stack);
        println!(
            "{}",
            self.chunk
                .disassemble_inst(self.ip, self.chunk.inst_at(self.ip))
        );
    }

    pub fn clear(&mut self) {
        self.identifiers.clear();
        self.strings.clear();
    }
}

impl EloxRunner for EloxVM {
    fn run(&mut self, source: &str) -> EloxResult {
        let scanner = Scanner::new(source.chars().peekable());
        let ast = Parser::new(scanner.peekable(), &mut self.identifiers).parse();

        match ast {
            Ok(ast) => {
                let mut compiler =
                    Compiler::new(&mut self.chunk, &mut self.identifiers, &mut self.strings);

                compiler.compile(&ast)?;
            }
            Err(err) => return Err(EloxError::Parser(err)),
        };

        // self.chunk.disassemble("test");

        self.launch()?;
        Ok(())
    }

    fn throw_error(&mut self, err: impl ErrorPosition) {
        let pos = err.position();
        (self.host.error)(format!("{}", err), pos.line, pos.col);
    }
}

// peekable stack
struct EloxVMStack {
    stack: Vec<Value>,
}

impl EloxVMStack {
    pub fn with_capacity(cap: usize) -> EloxVMStack {
        EloxVMStack {
            stack: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn get(&self, idx: usize) -> Value {
        assert!(self.stack.len() > idx);
        self.stack[idx].clone()
    }

    #[inline]
    pub fn set(&mut self, idx: usize, val: Value) {
        assert!(self.stack.len() > idx);
        self.stack[idx] = val;
    }

    #[inline]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("tried to pop an empty stack")
    }

    #[inline]
    pub fn pop_n(&mut self, n: usize) {
        self.stack.drain((self.stack.len() - n)..);
    }

    #[inline]
    pub fn peek(&mut self, offset: usize) -> Value {
        assert!(offset < self.stack.len());
        self.stack[self.stack.len() - 1 - offset].clone()
        // unsafe {
        //     let end = self.stack.as_ptr().add(self.stack.len() - 1 - offset);
        //     std::ptr::read(end)
        // }
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.stack.len()
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn stack() {
        let mut stack = EloxVMStack::with_capacity(2);
        let a = Value::Number(3f64);
        let b = Value::Number(7f64);
        let c = Value::Object(Rc::new(Obj::Str(String::from("test"))));
        stack.push(a.clone());
        stack.push(b.clone());
        assert_eq!(stack.pop(), b);
        assert_eq!(stack.peek(0), a);
        stack.push(c.clone());
        assert_eq!(stack.peek(1), a);
        {
            let p = stack.peek(0);
            assert_eq!(p, c);
        }
        let p2 = stack.pop();
        assert_eq!(p2, c);
        assert_eq!(stack.pop(), a);
    }
}
