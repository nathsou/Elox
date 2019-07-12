use super::lox_function::LoxFunctionParams;
use super::value::Value;
use super::Environment;
use super::EvalResult;
use super::Interpreter;
use crate::parser::IdentifierNames;
use crate::scanner::token::Position;
use std::rc::Rc;

pub trait LoxCallable: std::fmt::Debug {
    fn call(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
        args: Vec<Value>,
        call_pos: Position,
    ) -> EvalResult<Value>;

    fn params(&self) -> LoxFunctionParams;

    fn name(&self, names: &Rc<IdentifierNames>) -> String;

    fn has_rest_param(&self) -> bool;
}
