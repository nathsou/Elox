use super::lox_callable::{LoxCallable};
use super::value::Value;
use super::Environment;
use super::Interpreter;
use super::{eval_result::EvalError, EvalResult};
use crate::parser::{Identifier, IdentifierNames};
use std::cell::RefCell;
use std::rc::Rc;
use super::lox_function::LoxFunctionParams;

#[derive(Debug)]
pub enum NativeValue {
    Vector(RefCell<Vec<Value>>),
}

#[allow(irrefutable_let_patterns)]
impl NativeValue {
    pub fn into_vec(&self) -> &RefCell<Vec<Value>> {
        if let NativeValue::Vector(vec) = &self {
            return vec;
        }

        panic!(
            "could not convert native value '{:?}' into a NativeValue::Vector",
            self
        );
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

    fn params(&self) -> LoxFunctionParams {
        None
    }

    fn name(&self, names: &Rc<IdentifierNames>) -> String {
        names[Identifier::clock()].clone()
    }
}
