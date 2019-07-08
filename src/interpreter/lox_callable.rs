use super::value::Value;
use super::Environment;
use super::EvalResult;
use super::Interpreter;

pub enum LoxCallableType {
    Function,
    Class,
}

pub trait LoxCallable: std::fmt::Debug {
    fn call(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value>;

    fn arity(&self) -> usize;

    fn type_(&self) -> LoxCallableType; 
}
