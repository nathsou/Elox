use super::lox_callable::LoxCallable;
use super::lox_instance::LoxInstance;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Nil,
    Boolean(bool),
    Callable(Rc<LoxCallable>),
    Instance(LoxInstance)
}

impl Value {
    #[inline]
    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::Boolean(b) => b,
            _ => true,
        }
    }

    #[inline]
    pub fn into_callable(self) -> Option<Rc<LoxCallable>> {
        match self {
            Value::Callable(c) => Some(c),
            _ => None,
        }
    }

    #[inline]
    pub fn into_instance(self) -> Option<LoxInstance> {
        if let Value::Instance(inst) = self {
            return Some(inst);
        }

        None
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::String(ref a), &Value::String(ref b)) => a == b,
            (&Value::Number(a), &Value::Number(b)) => a == b,
            (&Value::Boolean(a), &Value::Boolean(b)) => a == b,
            (&Value::Nil, &Value::Nil) => true,
            (&Value::Callable(ref a), &Value::Callable(ref b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(nb) => write!(f, "{}", nb),
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(c) => write!(f, "{:?}", c),
            Value::Instance(i) => write!(f, "<instance {:?}>", i)
        }
    }
}
