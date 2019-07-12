pub mod environment;

pub mod eval;
pub mod eval_result;
mod execute;
pub mod host;
pub mod lexical_scope;
pub mod lox_array;
pub mod lox_callable;
mod lox_class;
pub mod lox_function;
mod lox_instance;
mod natives;
pub mod value;

use crate::parser::{
    statements::Stmt, IdentifierHandle, IdentifierNames, IdentifierUse, IdentifierUseHandle,
};
use environment::Environment;
use eval_result::EvalResult;
use execute::Exec;
use fnv::FnvHashMap;
use host::Host;
use std::rc::Rc;
use value::Value;

pub struct Interpreter {
    global: Environment,
    depths: FnvHashMap<IdentifierUseHandle, usize>,
    host: Rc<Host>,
    names: Rc<IdentifierNames>,
}

impl Interpreter {
    pub fn new(env: Environment, host: &Rc<Host>, names: &Rc<IdentifierNames>) -> Interpreter {
        Interpreter {
            global: env,
            depths: fnv::FnvHashMap::default(),
            host: Rc::clone(host),
            names: Rc::clone(&names),
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

    pub fn name(&self, handle: IdentifierHandle) -> String {
        self.names[handle].clone()
    }

    pub fn names(&self) -> Rc<IdentifierNames> {
        Rc::clone(&self.names)
    }

    pub fn lookup_variable(&self, env: &Environment, identifier: &IdentifierUse) -> Option<Value> {
        let res = if let Some(&depth) = self.depths.get(&identifier.use_handle) {
            env.get(depth, identifier.name)
        } else {
            self.global.get(0, identifier.name)
        };

        res
    }

    pub fn lookup_global(&self, name: IdentifierHandle) -> Option<Value> {
        self.global.get(0, name)
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
