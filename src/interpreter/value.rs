use super::eval_result::{EvalError, EvalResult};
use super::lox_callable::LoxCallable;
use super::lox_class::LoxClass;
use super::lox_function::LoxFunction;
use super::lox_instance::LoxInstance;
use super::Interpreter;
use crate::parser::Identifier;
use crate::scanner::token::Position;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum CallableValue {
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Native(Rc<LoxCallable>),
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Nil,
    Boolean(bool),
    Callable(CallableValue),
    Instance(LoxInstance),
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
    pub fn into_callable_value(self) -> Option<CallableValue> {
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

    pub fn into_number(self) -> Option<f64> {
        if let Value::Number(nb) = self {
            return Some(nb);
        }

        None
    }

    pub fn type_(&self) -> String {
        match &self {
            Value::Boolean(_) => "boolean".into(),
            Value::Number(_) => "number".into(),
            Value::Nil => "nil".into(),
            Value::String(_) => "string".into(),
            Value::Callable(callable) => match callable {
                CallableValue::Class(_) => "class".into(),
                CallableValue::Function(_) => "function".into(),
                CallableValue::Native(_) => "native".into(),
            },
            Value::Instance(_) => "instance".into(),
        }
    }
}

impl CallableValue {
    pub fn into_callable(self) -> Rc<LoxCallable> {
        match self {
            CallableValue::Class(c) => c,
            CallableValue::Function(f) => f,
            CallableValue::Native(n) => n,
        }
    }

    pub fn into_class(self) -> Option<Rc<LoxClass>> {
        if let CallableValue::Class(c) = self {
            return Some(Rc::clone(&c));
        }

        None
    }
}

impl PartialEq for CallableValue {
    fn eq(&self, other: &CallableValue) -> bool {
        match (self, other) {
            (&CallableValue::Function(ref a), &CallableValue::Function(ref b)) => Rc::ptr_eq(a, b),
            (&CallableValue::Class(ref a), &CallableValue::Class(ref b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::String(ref a), &Value::String(ref b)) => a == b,
            (&Value::Number(a), &Value::Number(b)) => a == b,
            (&Value::Boolean(a), &Value::Boolean(b)) => a == b,
            (&Value::Nil, &Value::Nil) => true,
            (&Value::Callable(ref a), &Value::Callable(ref b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub fn to_str(
        &self,
        interpreter: &Interpreter,
        call_pos: Position,
    ) -> EvalResult<std::string::String> {
        match self {
            Value::Nil => Ok(format!("nil")),
            Value::Boolean(b) => Ok(format!("{}", b)),
            Value::Number(nb) => Ok(format!("{}", nb)),
            Value::String(s) => Ok(format!("{}", s)),
            Value::Instance(inst) => {
                if let Some(res) = inst.call(Identifier::str_(), interpreter, vec![], call_pos) {
                    match res {
                        Ok(Value::String(s)) => Ok(s),
                        Ok(val) => Err(EvalError::ToStringMethodMustReturnAString(
                            inst.method_pos(Identifier::str_()).unwrap(), // we already know it exists
                            interpreter.name(inst.class_name()),
                            val.type_(),
                        )),
                        Err(err) => Err(err),
                    }
                } else {
                    Ok(format!(
                        "<instance {}>",
                        interpreter.name(inst.class_name())
                    ))
                }
            }
            Value::Callable(callable) => {
                let name = callable.clone().into_callable().name(&interpreter.names());

                use CallableValue::*;
                Ok(match callable {
                    Function(_) | Native(_) => format!("<function {}>", name),
                    Class(_) => format!("<class {}>", name),
                })
            }
        }
    }
}
