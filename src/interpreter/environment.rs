extern crate fnv;

use super::lox_array::create_elox_array_class;
use super::natives::Clock;
use super::value::{CallableValue, Value};
use crate::parser::{Identifier, IdentifierHandle, IdentifierHandlesGenerator};
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Environment {
    // we need multiple mutable refs to the parent scope in multiple same-level scopes -> Rc<RefCell>
    pub current: Rc<RefCell<InnerEnv>>,
}

#[derive(Debug)]
pub struct InnerEnv {
    pub values: FnvHashMap<IdentifierHandle, Value>,
    pub parent: Option<Environment>,
}

impl Environment {
    pub fn new(parent: Option<&Environment>) -> Environment {
        let current = InnerEnv {
            values: FnvHashMap::default(),
            parent: if let Some(p) = parent {
                Some(p.clone()) // inexpensive clone
            } else {
                None
            },
        };

        Environment {
            current: Rc::new(RefCell::new(current)),
        }
    }

    pub fn with_natives(
        parent: Option<&Environment>,
        identifiers: &mut IdentifierHandlesGenerator,
    ) -> Self {
        let env = Environment::new(parent);
        env.register_natives(identifiers);

        env
    }

    fn register_natives(&self, identifiers: &mut IdentifierHandlesGenerator) {
        self.define(
            identifiers.by_name("clock"),
            Value::Callable(CallableValue::Native(Rc::new(Clock))),
        );

        self.define(
            Identifier::array(),
            Value::Callable(CallableValue::Class(Rc::new(create_elox_array_class(
                &self,
                identifiers,
            )))),
        );
    }

    pub fn define(&self, identifier: IdentifierHandle, value: Value) {
        self.current.borrow_mut().values.insert(identifier, value);
    }

    pub fn get(&self, depth: usize, identifier: IdentifierHandle) -> Option<Value> {
        let current = self.current.borrow();

        if depth == 0 {
            if let Some(value) = current.values.get(&identifier) {
                return Some(value.clone()); // inexpensive clone
            }
        } else {
            if let Some(parent) = &current.parent {
                return parent.get(depth - 1, identifier);
            }
        }

        None
    }

    pub fn assign(&self, depth: usize, identifier: IdentifierHandle, value: Value) -> bool {
        let mut current = self.current.borrow_mut();

        if depth == 0 {
            current.values.insert(identifier, value);
            return true;
        } else if let Some(parent) = &current.parent {
            return parent.assign(depth - 1, identifier, value);
        }

        false
    }
}
