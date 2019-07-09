use super::lox_callable::{LoxCallable};
use super::value::Value;
use super::Environment;
use super::Interpreter;
use super::{eval_result::EvalError, EvalResult};
use std::cell::RefCell;

#[derive(Debug)]
pub enum NativeValue {
    Vector(RefCell<Vec<Value>>),
} 

impl NativeValue {
    pub fn into_vec(&self) -> Option<&RefCell<Vec<Value>>> {
        if let NativeValue::Vector(vec) = &self {
            return Some(vec);
        }

        None
    }
}

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(
        &self,
        interpreter: &Interpreter,
        _env: &Environment,
        _args: Vec<Value>,
    ) -> EvalResult<Value> {
        if let Some(now) = (interpreter.host.clock)() {
            return Ok(Value::Number(now));
        }

        Err(EvalError::CouldNotGetTime())
    }

    fn arity(&self) -> usize {
        0
    }

}