use super::lox_class::LoxClass;
use super::lox_function::LoxFunction;
use super::lox_instance::{LoxInstance, NativesMap};
use super::natives::NativeValue;
use super::value::Value;
use super::Environment;
use super::Interpreter;
use crate::parser::{Identifier, IdentifierHandlesGenerator};
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn vec_handle() -> usize {
    0
}

pub fn create_lox_array_class(
    env: &Environment,
    identifiers: &mut IdentifierHandlesGenerator,
) -> LoxClass {
    let mut methods = FnvHashMap::default();

    methods.insert(
        Identifier::init(),
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 _args: Vec<Value>| {
                    natives.insert(vec_handle(), NativeValue::Vector(RefCell::new(Vec::new())));
                    Ok(Value::Instance(this.clone()))
                },
            ),
            env.clone(),
            true,
            0,
            Identifier::init(),
        )),
    );

    let push_handle = identifiers.by_name("push");

    methods.insert(
        push_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>| {
                    let mut values = natives
                        .get(&vec_handle())
                        .unwrap()
                        .into_vec()
                        .unwrap()
                        .borrow_mut();
                    values.push(args[0].clone());

                    Ok(Value::Instance(this.clone()))
                },
            ),
            env.clone(),
            false,
            1,
            push_handle,
        )),
    );

    let get_handle = identifiers.by_name("get");

    methods.insert(
        get_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>| {
                    let values = natives
                        .get(&vec_handle())
                        .unwrap()
                        .into_vec()
                        .unwrap()
                        .borrow();

                    match args[0] {
                        Value::Number(n) => {
                            let idx = n.floor() as usize;
                            if n < 0f64 || idx >= values.len() || n % 1f64 != 0f64 {
                                return Ok(Value::Nil);
                            }

                            return Ok(values[idx].clone());
                        }
                        _ => return Ok(Value::Nil),
                    };
                },
            ),
            env.clone(),
            false,
            1,
            get_handle,
        )),
    );

    let set_handle = identifiers.by_name("set"); 

    methods.insert(
        set_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>| {
                    let mut values = natives
                        .get(&vec_handle())
                        .unwrap()
                        .into_vec()
                        .unwrap()
                        .borrow_mut();

                    match args[0] {
                        Value::Number(n) => {
                            let idx = n.floor() as usize;
                            if n < 0f64 || idx >= values.len() || n % 1f64 != 0f64 {
                                return Ok(Value::Boolean(false));
                            }

                            values[idx] = args[1].clone();
                            return Ok(Value::Boolean(true));
                        }
                        _ => return Ok(Value::Nil),
                    };
                },
            ),
            env.clone(),
            false,
            2,
            set_handle,
        )),
    );

    let length_handle = identifiers.by_name("length");

    methods.insert(
        length_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>| {
                    let values = natives
                        .get(&vec_handle())
                        .unwrap()
                        .into_vec()
                        .unwrap()
                        .borrow();

                    Ok(Value::Number(values.len() as f64))
                },
            ),
            env.clone(),
            false,
            0,
            length_handle,
        )),
    );

    LoxClass::new_native(Identifier::array(), None, methods)
}
