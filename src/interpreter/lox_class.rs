use super::eval_result::EvalResult;
use super::lox_callable::{LoxCallable};

use super::lox_function::LoxFunction;
use super::lox_instance::LoxInstance;
use super::Environment;
use super::Interpreter;
use super::Value;
use crate::parser::{Identifier, IdentifierHandle, IdentifierNames};
use fnv::FnvHashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct _LoxClass {
    pub identifier: IdentifierHandle,
    pub superclass: Option<Rc<LoxClass>>,
    pub methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>,
}

#[derive(Debug)]
pub struct LoxClass {
    pub mold: Rc<_LoxClass>,
    use_natives: bool,
}

impl LoxClass {
    pub fn new(
        identifier: IdentifierHandle,
        superclass: Option<Rc<LoxClass>>,
        methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>,
    ) -> LoxClass {
        LoxClass {
            mold: Rc::new(_LoxClass {
                identifier,
                superclass,
                methods,
            }),
            use_natives: false,
        }
    }

    pub fn new_native(
        identifier: IdentifierHandle,
        superclass: Option<Rc<LoxClass>>,
        methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>>,
    ) -> LoxClass {
        LoxClass {
            mold: Rc::new(_LoxClass {
                identifier,
                superclass,
                methods,
            }),
            use_natives: false,
        }
    }

    pub fn find_method(&self, name: IdentifierHandle) -> Option<Rc<LoxFunction>> {
        let methods = &self.mold.methods;

        if let Some(func) = methods.get(&name) {
            return Some(Rc::clone(func));
        }

        if let Some(parent) = &self.mold.superclass {
            return parent.find_method(name);
        }

        None
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &Interpreter,
        _env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value> {
        let instance = if self.use_natives {
            LoxInstance::new(Rc::clone(&self.mold))
        } else {
            LoxInstance::new_native(Rc::clone(&self.mold))
        };
        if let Some(initializer) = self.find_method(Identifier::init()) {
            let bound_init = initializer.bind(&instance);
            bound_init.call(interpreter, &bound_init.env, args)?;
        }

        Ok(Value::Instance(instance))
    }

    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method(Identifier::init()) {
            return initializer.arity();
        }

        0usize
    }

    fn name(&self, names: &Rc<IdentifierNames>) -> String {
        format!("{}.init", names[self.mold.identifier].clone())
    }
}

impl fmt::Debug for _LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.identifier)
    }
}
