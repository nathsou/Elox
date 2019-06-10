pub mod environment;
mod eval;
mod eval_result;
mod execute;
mod lexical_scope;
mod lox_callable;
mod lox_function;
mod natives;
mod value;

use crate::parser::statements::Stmt;
use environment::Environment;
use eval_result::EvalResult;
use execute::Exec;
use value::Value;

pub struct Interpreter {
    global: Environment,
}

impl Interpreter {
    pub fn new(env: Environment) -> Interpreter {
        Interpreter { global: env }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> EvalResult<()> {
        for stmt in stmts {
            self.exec(&self.global, stmt)?;
        }

        Ok(())
    }
}
