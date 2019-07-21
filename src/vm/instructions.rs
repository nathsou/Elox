use crate::parser::IdentifierHandle;
use fnv::FnvHashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Inst {
    Ret,
    Const(usize),
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
    PopN(usize),
    DefGlobal(IdentifierHandle),
    GetGlobal(IdentifierHandle),
    SetGlobal(IdentifierHandle),
    GetLocal(usize),
    SetLocal(usize),
    Jmp(usize),
    JmpIfFalse(usize),
    JmpIfTrue(usize),
    Loop(usize), // Jumps backwards
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(Rc<Obj>),
}

#[derive(Debug)]
pub enum Obj {
    Str(String),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Obj::Str(string) => write!(f, "{}", string),
        }
    }
}

impl Obj {
    pub fn type_(&self) -> String {
        match self {
            Obj::Str(_) => "string",
        }
        .into()
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Str(ref a), Obj::Str(ref b)) => a == b,
        }
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
