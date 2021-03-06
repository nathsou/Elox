use super::eval_result::{EvalError, EvalResult};
use super::lox_class::LoxClass;
use super::lox_function::LoxFunction;
use super::lox_instance::{LoxInstance, NativesMap};
use super::natives::NativeValue;
use super::value::Value;
use super::Environment;
use super::Interpreter;
use crate::parser::expressions::ContextLessFuncParam::*;
use crate::parser::{Identifier, IdentifierHandlesGenerator};
use crate::scanner::token::Position;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn vec_handle() -> usize {
    0
}

pub fn new_elox_array(values: Vec<Value>, interpreter: &Interpreter) -> Value {
    let mut natives = FnvHashMap::default();
    natives.insert(
        vec_handle(),
        NativeValue::Vector(Rc::new(RefCell::new(values))),
    );

    Value::Instance(LoxInstance::instantiate_global(
        Identifier::array(),
        Some(natives),
        interpreter,
    ))
}

pub fn create_elox_array_class(
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
                 args: Vec<Value>,
                 _call_pos: Position| {
                    let capacity = args[0].clone().into_number().unwrap() as usize;
                    natives.insert(
                        vec_handle(),
                        NativeValue::Vector(Rc::new(RefCell::new(Vec::with_capacity(capacity)))),
                    );
                    Ok(Value::Instance(this.clone()))
                },
            ),
            env.clone(),
            true,
            Some(Rc::new(vec![DefaultValued(
                identifiers.by_name("capacity"),
                Value::Number(0f64),
            )])),
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
                 args: Vec<Value>,
                 _call_pos: Position| {
                    let mut values = natives.get(&vec_handle()).unwrap().into_vec().borrow_mut();
                    let pushed_values = args[0].clone().into_instance().unwrap();
                    let pushed_values = pushed_values.get_native(vec_handle()).unwrap();
                    let mut pushed_values = pushed_values.into_vec().borrow_mut();

                    values.append(&mut pushed_values);

                    Ok(Value::Instance(this.clone()))
                },
            ),
            env.clone(),
            false,
            Some(Rc::new(vec![Rest(identifiers.by_name("values"))])),
            push_handle,
        )),
    );

    let get_handle = Identifier::get();

    methods.insert(
        get_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>,
                 call_pos: Position| {
                    let values = natives.get(&vec_handle()).unwrap().into_vec().borrow();

                    match args[0] {
                        Value::Number(n) => {
                            let idx = n.floor() as usize;
                            if idx >= values.len() {
                                return Err(EvalError::ArrayIndexOutOfBounds(
                                    call_pos,
                                    idx,
                                    values.len(),
                                ));
                            }
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
            Some(Rc::new(vec![Required(identifiers.by_name("idx"))])),
            get_handle,
        )),
    );

    let set_handle = Identifier::set();

    methods.insert(
        set_handle,
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 _interpreter: &Interpreter,
                 _env: &Environment,
                 args: Vec<Value>,
                 _call_pos: Position| {
                    let mut values = natives.get(&vec_handle()).unwrap().into_vec().borrow_mut();

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
            Some(Rc::new(vec![
                Required(identifiers.by_name("idx")),
                Required(identifiers.by_name("value")),
            ])),
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
                 _args: Vec<Value>,
                 _call_pos: Position| {
                    let values = natives.get(&vec_handle()).unwrap().into_vec().borrow();

                    Ok(Value::Number(values.len() as f64))
                },
            ),
            env.clone(),
            false,
            None,
            length_handle,
        )),
    );

    methods.insert(
        Identifier::str_(),
        Rc::new(LoxFunction::new_native_method(
            Rc::new(
                |_this: &LoxInstance,
                 natives: &mut NativesMap,
                 _func: &LoxFunction,
                 interpreter: &Interpreter,
                 _env: &Environment,
                 _args: Vec<Value>,
                 call_pos: Position| {
                    let values = natives.get(&vec_handle()).unwrap().into_vec().borrow();

                    let to_str = values
                        .iter()
                        .map(|val| val.to_str(interpreter, call_pos))
                        .collect::<EvalResult<Vec<String>>>();

                    match to_str {
                        Ok(strings) => Ok(Value::String(format!("[{}]", strings.join(", ")))),
                        Err(err) => Err(err),
                    }
                },
            ),
            env.clone(),
            false,
            None,
            Identifier::str_(),
        )),
    );

    LoxClass::new_native(Identifier::array(), None, methods)
}
