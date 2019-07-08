use super::lox_callable::{LoxCallable, LoxCallableType};
use super::value::Value;
use super::Environment;
use super::Interpreter;
use super::{eval_result::EvalError, EvalResult};
use std::time::SystemTime;

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _env: &Environment,
        _args: Vec<Value>,
    ) -> EvalResult<Value> {
        if let Ok(now) = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            return Ok(Value::Number((now.as_millis() as f64) / 1000f64));
        }

        Err(EvalError::CouldNotGetTime())
    }

    fn arity(&self) -> usize {
        0
    }

    fn type_(&self) -> LoxCallableType {
        LoxCallableType::Function
    }
}
