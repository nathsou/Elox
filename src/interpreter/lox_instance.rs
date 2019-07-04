use super::lox_class::_LoxClass;
use super::lox_function::LoxFunction;
use super::Value;
use crate::parser::IdentifierHandle;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct _Instance {
    mold: Rc<_LoxClass>,
    fields: FnvHashMap<IdentifierHandle, Value>,
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
            })),
        }
    }

    pub fn get(&self, prop: IdentifierHandle) -> Option<Value> {

        if let Some(val) = self.instance.borrow().fields.get(&prop) {
            return Some(val.clone());
        }

        if let Some(method) = self.find_method(prop) {
            return Some(Value::Callable(Rc::new(method.bind(self))));
        }

        None
    }

    pub fn find_method(&self, name: IdentifierHandle) -> Option<Rc<LoxFunction>> {
        let methods = &self.instance.borrow().mold.methods;

        if let Some(func) = methods.get(&name) {
            return Some(Rc::clone(func));
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