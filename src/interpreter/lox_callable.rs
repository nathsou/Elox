use super::lox_function::LoxFunctionParams;
use super::value::Value;
use super::Environment;
use super::EvalResult;
use super::Interpreter;
use crate::parser::IdentifierNames;
use std::rc::Rc;

pub trait LoxCallable: std::fmt::Debug {
    fn call(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value>;

    fn params(&self) -> LoxFunctionParams;

    fn name(&self, names: &Rc<IdentifierNames>) -> String;
}
