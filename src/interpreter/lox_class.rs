use super::eval_result::EvalResult;
use super::lox_callable::LoxCallable;

use super::lox_function::LoxFunction;
use super::lox_instance::LoxInstance;
use super::Environment;
use super::Interpreter;
use super::Value;
use crate::parser::{Identifier, IdentifierHandle};
use fnv::FnvHashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct _LoxClass {
    pub identifier: IdentifierHandle,
    pub methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>,
}

#[derive(Debug)]
pub struct LoxClass {
    pub mold: Rc<_LoxClass>,
}

impl LoxClass {
    pub fn new(
        identifier: IdentifierHandle,
        methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>,
    ) -> LoxClass {
        LoxClass {
            mold: Rc::new(_LoxClass {
                identifier,
                methods,
            }),
        }
    }

    pub fn find_method(&self, name: IdentifierHandle) -> Option<Rc<LoxFunction>> {
        let methods = &self.mold.methods;

        if let Some(func) = methods.get(&name) {
            return Some(Rc::clone(func));
        }

        None
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value> {
        let instance = LoxInstance::new(Rc::clone(&self.mold));
        if let Some(initializer) = self.find_method(Identifier::init()) {
            initializer.bind(&instance).call(interpreter, env, args)?;
        }

        Ok(Value::Instance(instance))
    }

    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method(Identifier::init()) {
            return initializer.arity();
        }

        0usize
    }
}

impl fmt::Debug for _LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.identifier)
    }
}