use super::lox_callable::LoxCallable;
use super::lox_function::LoxFunctionParams;
use super::value::Value;
use super::Environment;
use super::Interpreter;
use super::{eval_result::EvalError, EvalResult};
use crate::parser::{Identifier, IdentifierNames};
use crate::scanner::token::Position;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum NativeValue {
    Vector(Rc<RefCell<Vec<Value>>>),
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
        call_pos: Position,
    ) -> EvalResult<Value> {
        if let Ok(now) = (interpreter.host.clock)(call_pos) {
            return Ok(Value::Number(now));
        }

        Err(EvalError::CouldNotGetTime(call_pos))
    }

    fn params(&self) -> LoxFunctionParams {
        None
    }

    fn name(&self, names: &Rc<IdentifierNames>) -> String {
        names[Identifier::clock()].clone()
    }

    fn has_rest_param(&self) -> bool {
        false
    }
}
