pub mod chunk;
mod compiler;
pub mod instructions;
pub mod target;
pub mod wasm_module;

use crate::interpreter::eval_result::EvalError;
use crate::interpreter::host::Host;
use crate::parser::expressions::{BinaryOperator, UnaryOperator};
use crate::parser::{IdentifierHandle, IdentifierHandlesGenerator, Parser};
use crate::runner::{EloxError, EloxResult, EloxRunner};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use crate::scanner::Scanner;
use chunk::Chunk;
use compiler::{Compiler, FuncType, Local};
use fnv::FnvHashMap;
use instructions::{FuncObj, Inst, NativeFunc, Obj, Value};
use std::fmt;
use std::rc::Rc;

macro_rules! binary_op {
    (==, $self: tt) => ({
        let b = $self.stack.pop();
        let a = $self.stack.pop();
        $self.stack.push(Value::Boolean(a == b));
    });

    (!=, $self: tt) => ({
        let b = $self.stack.pop();
        let a = $self.stack.pop();
        $self.stack.push(Value::Boolean(a != b));
    });

    (+, $self: tt) => ({
        let b = $self.stack.pop();
        let a = $self.stack.pop();
        match (&a, &b) {
            (&Value::Number(a), &Value::Number(b)) => $self.stack.push(Value::Number(a + b)),
            _ => {
                let obj = Value::new_str(&format!("{}{}", a, b), &mut $self.strings);
                $self.stack.push(obj);
            },
        };
    });

    ($op: tt, $self: tt, $bin_op: expr, $ValType: ident) => ({
        let b = $self.stack.pop();
        let a = $self.stack.pop();
        match (&a, &b) {
            (&Value::Number(a), &Value::Number(b)) => $self.stack.push(Value::$ValType(a $op b)),
            _ => return Err(EloxError::Eval(EvalError::UnexpectedBinaryOperatorOperands($self.pos(), $bin_op, a.type_(), b.type_()))),
        };
    });
}

#[derive(Clone)]
struct CallFrame {
    pub func: Rc<FuncObj>,
    pub stack_top: usize,
    pub ip: usize,
}

impl CallFrame {
    pub fn new(func: &Rc<FuncObj>, stack_top: usize, ip: usize) -> CallFrame {
        CallFrame {
            func: Rc::clone(func),
            stack_top,
            ip,
        }
    }
}

const MAX_FRAMES: usize = 256;

pub struct EloxVM {
    host: Rc<Host>,
    stack: EloxVMStack,
    call_frames: Vec<CallFrame>,
    frames_count: usize,
    identifiers: IdentifierHandlesGenerator,
    strings: FnvHashMap<String, Rc<Obj>>,
    globals: FnvHashMap<IdentifierHandle, Value>,
}

impl EloxVM {
    pub fn new() -> EloxVM {
        let mut vm = EloxVM {
            host: Rc::new(Host::default()),
            stack: EloxVMStack::with_capacity(256),
            // TODO: Use MaybeUninit
            call_frames: Vec::with_capacity(64),
            frames_count: 0,
            identifiers: IdentifierHandlesGenerator::new(),
            strings: FnvHashMap::default(),
            globals: FnvHashMap::default(),
        };

        vm.define_natives();

        vm
    }

    fn define_natives(&mut self) {
        let host_clock = Rc::clone(&self.host.clock);
        let clock = NativeFunc {
            name: self.identifiers.by_name("clock"),
            arity: 0,
            func: Box::new(move |pos, _| Ok(Value::Number((host_clock)(pos)?))),
            // func: Box::new(|pos, _| {
            //     if let Ok(now) = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            //         Ok(Value::Number(now.as_millis() as f64 / 1000f64))
            //     } else {
            //         Err(EloxError::Eval(EvalError::CouldNotGetTime(pos)))
            //     }
            // }),
        };

        self.define_native(clock);
    }

    fn define_native(&mut self, func: NativeFunc) {
        self.globals.insert(
            func.name,
            Value::Object(Rc::new(Obj::Native(Rc::new(func)))),
        );
    }

    pub fn launch(&mut self) -> EloxResult {
        loop {
            let ip = self.call_frames[self.frames_count - 1].ip;
            let inst = &self.call_frames[self.frames_count - 1]
                .func
                .chunk
                .inst_at(ip);
            // self.trace();
            match inst {
                Inst::Ret => {
                    let ret_val = self.stack.pop();
                    let frame = self.call_frames.pop().unwrap();
                    self.frames_count -= 1;

                    if self.frames_count == 0 {
                        break;
                    }

                    let start_count = frame.stack_top;
                    self.stack.pop_n(self.stack.size() - start_count);
                    self.stack.push(ret_val);
                }
                Inst::Pop => {
                    self.stack.pop();
                }
                Inst::PopN(n) => {
                    self.stack.pop_n(*n);
                }
                Inst::Jmp(offset) => {
                    self.call_frames[self.frames_count - 1].ip = ip + offset;
                    continue; // don't increment the ip at the end
                }
                Inst::Loop(offset) => {
                    self.call_frames[self.frames_count - 1].ip =
                        (ip as isize - *offset as isize) as usize;
                    continue;
                }
                Inst::JmpIfTrue(offset) => {
                    if self.stack.peek(0).is_truthy() {
                        self.call_frames[self.frames_count - 1].ip = ip + offset;
                        continue;
                    }
                }
                Inst::JmpIfFalse(offset) => {
                    if !self.stack.peek(0).is_truthy() {
                        self.call_frames[self.frames_count - 1].ip = ip + offset;
                        continue;
                    }
                }
                Inst::DefGlobal(id_handle) => {
                    // we peek instead of popping to ensure that the VM still has
                    // access to the value while inserting it to the globals HashMap
                    // in the event of a gc while inserting
                    let val = self.stack.peek(0);
                    self.globals.insert(*id_handle, val);
                    self.stack.pop();
                }
                Inst::GetGlobal(id_handle) => {
                    let var = self.globals.get(&id_handle);
                    if let Some(var) = var {
                        let var = var.clone();
                        self.stack.push(var);
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
                    let offset = self.call_frames[self.frames_count - 1].stack_top;
                    let val = self.stack.get(*idx + offset);
                    self.stack.push(val);
                }
                Inst::SetLocal(idx) => {
                    let offset = self.call_frames[self.frames_count - 1].stack_top;
                    let val = self.stack.peek(0);
                    self.stack.set(*idx + offset, val);
                }
                Inst::Print => {
                    println!("{}", self.stack.pop());
                }
                Inst::Const(idx) => {
                    let val = self.call_frames[self.frames_count - 1]
                        .func
                        .chunk
                        .read_const(*idx);
                    self.stack.push(val);
                }
                Inst::Call(args_count) => {
                    let args_count = *args_count;
                    let val = self.stack.peek(args_count);
                    if self.call_value(val, args_count)? {
                        continue; // don't increment the ip
                    }
                }
                Inst::Neg => {
                    let val = self.stack.pop();
                    if let Value::Number(nb) = val {
                        self.stack.push(Value::Number(-nb));
                    } else {
                        return Err(EloxError::Eval(EvalError::UnexpectedUnaryOperatorOperand(
                            self.pos(),
                            UnaryOperator::Minus,
                            val.type_(),
                        )));
                    }
                }
                Inst::Not => {
                    let val = self.stack.pop();
                    self.stack.push(Value::Boolean(!val.is_truthy()));
                }
                Inst::Add => binary_op!(+, self),
                Inst::Sub => binary_op!(-, self, BinaryOperator::Minus, Number),
                Inst::Mult => binary_op!(*, self, BinaryOperator::Star, Number),
                Inst::Div => binary_op!(/, self, BinaryOperator::Slash, Number),
                Inst::Mod => binary_op!(%, self, BinaryOperator::Percent, Number),
                Inst::True => self.stack.push(Value::Boolean(true)),
                Inst::False => self.stack.push(Value::Boolean(false)),
                Inst::Nil => self.stack.push(Value::Nil),
                Inst::Equ => binary_op!(==, self),
                Inst::Neq => binary_op!(!=, self),
                Inst::Gtr => binary_op!(>, self, BinaryOperator::Greater, Boolean),
                Inst::Lss => binary_op!(<, self, BinaryOperator::Less, Boolean),
                Inst::Gtq => binary_op!(>=, self, BinaryOperator::GreaterEqual, Boolean),
                Inst::Leq => binary_op!(<=, self, BinaryOperator::LessEqual, Boolean),
            }
            self.call_frames[self.frames_count - 1].ip += 1;
        }
        // debug_assert!(
        //     self.stack.size() == 1, // main func object
        //     "the stack must be empty at the end of execution",
        // );

        Ok(())
    }

    fn call_value(&mut self, val: Value, args_count: usize) -> Result<bool, EloxError> {
        if let Value::Object(obj) = &val {
            match &**obj {
                &Obj::Func(ref func) => {
                    self.call(func, args_count)?;
                    return Ok(true); // don't increment the ip
                }
                &Obj::Native(ref native) => {
                    let res = (native.func)(self.pos(), self.stack.pop_n(native.arity))?;
                    self.stack.pop(); // pop the native
                    self.stack.push(res);
                }
                _ => {
                    return Err(EloxError::Eval(EvalError::ValueNotCallable(
                        self.pos(),
                        val.type_(),
                    )))
                }
            }
        }

        Ok(false)
    }

    fn call(&mut self, func: &Rc<FuncObj>, args_count: usize) -> EloxResult {
        if args_count != func.arity {
            return Err(EloxError::Eval(EvalError::WrongNumberOfArgs(
                self.pos(),
                func.arity,
                args_count,
                String::from("fn"),
            )));
        }

        let frame = CallFrame::new(func, (self.stack.size() - 1) - args_count, 0);
        self.call_frames.push(frame);
        self.frames_count += 1;

        if self.frames_count > MAX_FRAMES {
            return Err(EloxError::Eval(EvalError::StackOverflow(
                self.pos(),
                MAX_FRAMES,
            )));
        }

        Ok(())
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        &self.call_frames[self.frames_count - 1]
    }

    #[inline]
    fn pos(&self) -> Position {
        let frame = self.current_frame();
        frame.func.chunk.pos_at(frame.ip)
    }

    #[inline]
    #[allow(dead_code)]
    fn trace(&self) {
        let frame = self.current_frame();
        println!("{}", self.stack);
        println!(
            "{}",
            frame.func.chunk.disassemble_inst(
                self.call_frames[self.frames_count - 1].ip,
                self.call_frames[self.frames_count - 1]
                    .func
                    .chunk
                    .inst_at(self.call_frames[self.frames_count - 1].ip)
            )
        );
    }

    pub fn clear(&mut self) {
        self.identifiers.clear();
        self.strings.clear();
    }

    pub fn compile(&mut self, source: &str) -> EloxResult {
        let scanner = Scanner::new(source.chars().peekable());
        let ast = Parser::new(scanner.peekable(), &mut self.identifiers).parse();

        match ast {
            Ok(ast) => {
                let mut func = FuncObj::new(Some(FuncObj::main_func_name()), 0); // main func
                let mut locals: Vec<Local> = vec![];

                let mut compiler = Compiler::new(
                    &mut locals,
                    0,
                    &mut func,
                    FuncType::SCRIPT,
                    &mut self.identifiers,
                    &mut self.strings,
                );

                compiler.compile(&ast)?;
                func.chunk.disassemble("main");
                let func_obj = Rc::new(func);
                let func = Rc::new(Obj::Func(Rc::clone(&func_obj)));
                let val = Value::Object(func);
                self.stack.push(val.clone());
                self.call_value(val, 0)?;
            }
            Err(err) => return Err(EloxError::Parser(err)),
        };

        Ok(())
    }

    pub fn chunk(&self) -> &Chunk {
        &self.call_frames[self.frames_count - 1].func.chunk
    }
}

impl EloxRunner for EloxVM {
    fn run(&mut self, source: &str) -> EloxResult {
        self.compile(source)?;
        self.launch()?;
        Ok(())
    }

    fn throw_error(&mut self, err: impl ErrorPosition) -> EloxResult {
        let pos = *err.position();
        (self.host.error)(pos, format!("{}", err), pos.line, pos.col)?;
        Ok(())
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
    pub fn pop_n(&mut self, n: usize) -> Vec<Value> {
        self.stack.drain((self.stack.len() - n)..).collect()
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
