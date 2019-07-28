use crate::parser::IdentifierHandle;
use super::{EloxError, Position, Chunk};
use fnv::FnvHashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Inst {
    Ret,
    Const(usize), // index in the constants table
    Neg,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Not,
    True,
    False,
    Nil,
    Equ,
    Neq,
    Gtr,
    Lss,
    Gtq,
    Leq,
    Print,
    Pop,
    PopN(usize), // n
    DefGlobal(IdentifierHandle),
    GetGlobal(IdentifierHandle),
    SetGlobal(IdentifierHandle),
    GetLocal(usize),   // stack offset
    SetLocal(usize),   // stack offset
    Jmp(usize),        // addr
    JmpIfFalse(usize), // addr
    JmpIfTrue(usize),  // addr
    Loop(usize),       // Jumps backwards to addr
    Call(usize),       // args count
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(Rc<Obj>),
}

#[derive(Debug, Clone)]
pub struct FuncObj {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<IdentifierHandle>,
}

impl FuncObj {
    pub fn new(name: Option<IdentifierHandle>, arity: usize) -> FuncObj {
        FuncObj {
            name,
            arity,
            chunk: Chunk::new(),
        }
    }

    pub fn main_func_name() -> IdentifierHandle {
        usize::max_value()
    }
}

pub type NativeFn = (Fn(Position, Vec<Value>) -> Result<Value, EloxError>);

pub struct NativeFunc {
    pub name: IdentifierHandle,
    pub arity: usize,
    pub func: Box<NativeFn>,
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native function>")
    }
}

#[derive(Debug)]
pub enum Obj {
    Str(String),
    Func(Rc<FuncObj>),
    Native(Rc<NativeFunc>),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Obj::Str(string) => write!(f, "{}", string),
            Obj::Func(func) => write!(
                f,
                "<function {}>",
                if let Some(name) = func.name {
                    format!("{}", name)
                } else {
                    "anonymous".into()
                }
            ),
            Obj::Native(_) => write!(f, "<native function>"),
        }
    }
}

impl Obj {
    pub fn type_(&self) -> String {
        match self {
            Obj::Str(_) => "string",
            Obj::Func(_) => "function",
            Obj::Native(_) => "native function",
        }
        .into()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", *n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(obj) => obj.fmt(f),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn type_(&self) -> String {
        match &self {
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Nil => "nil",
            Value::Object(obj) => return obj.type_(),
        }
        .into()
    }

    pub fn new_str(s: &str, strings: &mut FnvHashMap<String, Rc<Obj>>) -> Value {
        let s = String::from(s);

        if let Some(string) = strings.get(&s) {
            return Value::Object(Rc::clone(string));
        }
        let val = Rc::new(Obj::Str(s.clone()));

        strings.insert(s, Rc::clone(&val));

        Value::Object(Rc::clone(&val))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }
        match (self, other) {
            (&Value::Number(a), &Value::Number(b)) => a == b,
            (&Value::Boolean(a), &Value::Boolean(b)) => a == b,
            (&Value::Object(ref a), &Value::Object(ref b)) => Rc::ptr_eq(a, b),
            (&Value::Nil, &Value::Nil) => true,
            _ => false,
        }
    }
}
