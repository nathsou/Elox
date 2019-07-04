use super::eval_result::EvalResult;
use super::lox_callable::LoxCallable;
use super::lox_instance::LoxInstance;
use super::lox_function::LoxFunction;
use super::Environment;
use super::Interpreter;
use super::Value;
use crate::parser::IdentifierHandle;
use fnv::FnvHashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

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
    pub fn new(identifier: IdentifierHandle, methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>) -> LoxClass {
        LoxClass {
            mold: Rc::new(_LoxClass { identifier, methods }),
        }
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &Interpreter,
        _env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value> {
        let instance = LoxInstance::new(Rc::clone(&self.mold));
        Ok(Value::Instance(instance))
    }

    fn arity(&self) -> usize {
        0usize
    }
}

impl fmt::Debug for _LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.identifier)
    }
}