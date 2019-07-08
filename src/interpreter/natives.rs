use super::lox_callable::{LoxCallable, LoxCallableType};
use super::value::Value;
use super::Environment;
use super::Interpreter;
use super::{eval_result::EvalError, EvalResult};

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

    fn type_(&self) -> LoxCallableType {
        LoxCallableType::Function
    }
}
