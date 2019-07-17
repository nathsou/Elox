use super::eval_result::EvalResult;
use super::lox_callable::LoxCallable;
use super::lox_class::_LoxClass;
use super::lox_function::LoxFunction;
use super::natives::NativeValue;
use super::value::{CallableValue, Value};
use super::Interpreter;
use crate::parser::IdentifierHandle;
use crate::scanner::token::Position;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type NativesMap = FnvHashMap<usize, NativeValue>;
pub type InstanceFields = FnvHashMap<IdentifierHandle, Value>;

#[derive(Debug)]
pub struct _Instance {
    mold: Rc<_LoxClass>,
    fields: InstanceFields,
    pub natives: Option<NativesMap>,
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    pub instance: Rc<RefCell<_Instance>>,
}

impl LoxInstance {
    // class must be defined in the global scope
    pub fn instantiate_global(
        class_name: IdentifierHandle,
        natives: Option<NativesMap>,
        interpreter: &Interpreter,
    ) -> LoxInstance {
        //FIXME: Throw errors instead of panicing
        if let Some(class) = interpreter.lookup_global(class_name) {
            let class = class
                .into_callable_value()
                .expect("Tried to instantiate a non-callable value")
                .into_class()
                .expect("Tried to instantiate");
            return LoxInstance {
                instance: Rc::new(RefCell::new(_Instance {
                    mold: Rc::clone(&class.mold),
                    fields: FnvHashMap::default(),
                    natives,
                })),
            };
        }

        panic!(
            "could not instantiate: {} not found in the given environment",
            interpreter.name(class_name)
        );
    }

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

    pub fn get_native(self, handle: usize) -> Option<NativeValue> {
        if let Some(natives) = &self.instance.borrow().natives {
            if let Some(native) = natives.get(&handle) {
                return Some(native.clone());
            } else {
                return None;
            }
        }

        None
    }

    pub fn method_pos(&self, method_name: IdentifierHandle) -> Option<Position> {
        if let Some(method) = self.find_method(method_name) {
            return method.pos();
        }
        None
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

    pub fn call(
        &self,
        method_name: IdentifierHandle,
        interpreter: &Interpreter,
        args: Vec<Value>,
        call_pos: Position,
    ) -> Option<EvalResult<Value>> {
        if let Some(method) = self.find_method(method_name) {
            let bound = method.bind(&self);
            return Some(bound.call(interpreter, &bound.env, args, call_pos));
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

impl PartialEq for LoxInstance {
    fn eq(&self, other: &LoxInstance) -> bool {
        Rc::ptr_eq(&self.instance, &other.instance)
    }
}
