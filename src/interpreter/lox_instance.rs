use super::lox_class::_LoxClass;
use super::lox_function::LoxFunction;
use super::natives::NativeValue;
use super::value::{CallableValue, Value};
use crate::parser::IdentifierHandle;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

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

    fn find_method(&self, name: IdentifierHandle) -> Option<Rc<LoxFunction>> {
        let mold = &self.instance.borrow().mold;
        if let Some(func) = mold.methods.get(&name) {
            return Some(Rc::clone(func));
        }

        if let Some(parent) = &mold.superclass {
            return parent.find_method(name);
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
