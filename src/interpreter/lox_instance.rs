use super::lox_class::_LoxClass;
use super::lox_callable::LoxCallable;
use super::lox_function::LoxFunction;
use super::natives::NativeValue;
use super::value::{CallableValue, Value};
use crate::parser::IdentifierHandle;
use super::Interpreter;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use super::eval_result::EvalResult;

pub type NativesMap = FnvHashMap<usize, NativeValue>;

#[derive(Debug)]
pub struct _Instance {
    mold: Rc<_LoxClass>,
    fields: FnvHashMap<IdentifierHandle, Value>,
    pub natives: Option<NativesMap>,
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    pub instance: Rc<RefCell<_Instance>>,
}

impl LoxInstance {
    pub fn new(mold: Rc<_LoxClass>) -> LoxInstance {
        LoxInstance {
            instance: Rc::new(RefCell::new(_Instance {
                mold: Rc::clone(&mold),
                fields: FnvHashMap::default(),
                natives: None,
            })),
        }
    }

    pub fn new_native(mold: Rc<_LoxClass>) -> LoxInstance {
        LoxInstance {
            instance: Rc::new(RefCell::new(_Instance {
                mold: Rc::clone(&mold),
                fields: FnvHashMap::default(),
                natives: Some(FnvHashMap::default()),
            })),
        }
    }

    pub fn get(&self, prop: IdentifierHandle) -> Option<Value> {
        if let Some(val) = self.instance.borrow().fields.get(&prop) {
            return Some(val.clone());
        }

        if let Some(method) = self.find_method(prop) {
            return Some(Value::Callable(CallableValue::Function(Rc::new(
                method.bind(self),
            ))));
        }

        None
    }

    pub fn class_name(&self) -> IdentifierHandle {
        self.instance.borrow().mold.identifier
    }

    pub fn find_method(&self, name: IdentifierHandle) -> Option<Rc<LoxFunction>> {
        let mold = &self.instance.borrow().mold;
        if let Some(func) = mold.methods.get(&name) {
            return Some(Rc::clone(func));
        }

        if let Some(parent) = &mold.superclass {
            return parent.find_method(name);
        }

        None
    }

    pub fn call(&self, method_name: IdentifierHandle, interpreter: &Interpreter, args: Vec<Value>) -> Option<EvalResult<Value>> {
        if let Some(method) = self.find_method(method_name) {
            let bound = method.bind(&self);
            return Some(bound.call(interpreter, &bound.env, args));
        }
        
        None
    }

    pub fn set(&self, prop: IdentifierHandle, value: &Value) {
        self.instance
            .borrow_mut()
            .fields
            .insert(prop, value.clone());
    }
}
