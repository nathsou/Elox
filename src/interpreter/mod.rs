pub mod environment;

mod eval;
mod eval_result;
mod execute;
pub mod lexical_scope;
mod lox_callable;
mod lox_class;
mod lox_function;
mod lox_instance;
mod natives;
pub mod value;

use crate::parser::{statements::Stmt, IdentifierUse, IdentifierUseHandle};
use environment::Environment;
use eval_result::EvalResult;
use execute::Exec;

use fnv::FnvHashMap;
use value::Value;
pub struct Interpreter {
    global: Environment,
    depths: FnvHashMap<IdentifierUseHandle, usize>,
}

impl Interpreter {
    pub fn new(env: Environment) -> Interpreter {
        Interpreter {
            global: env,
            depths: fnv::FnvHashMap::default(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> EvalResult<()> {
        for stmt in stmts {
            self.exec(&self.global, stmt)?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, identifier: IdentifierUseHandle, depth: usize) {
        self.depths.insert(identifier, depth);
    }

    pub fn lookup_variable(&self, env: &Environment, identifier: &IdentifierUse) -> Option<Value> {
        if let Some(&depth) = self.depths.get(&identifier.use_handle) {
            env.get(depth, identifier.name)
        } else {
            self.global.get(0, identifier.name)
        }
    }

    pub fn assign_variable(
        &self,
        env: &Environment,
        identifier: &IdentifierUse,
        value: Value,
    ) -> bool {
        if let Some(&depth) = self.depths.get(&identifier.use_handle) {
            env.assign(depth, identifier.name, value)
        } else {
            self.global.assign(0, identifier.name, value)
        }
    }
}
