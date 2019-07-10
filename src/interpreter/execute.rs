use super::environment::Environment;
use super::eval::Eval;
use super::lox_class::LoxClass;

use super::lox_function::LoxFunction;
use super::value::{CallableValue, Value};
use crate::interpreter::eval_result::{EvalError, EvalResult};
use crate::interpreter::Interpreter;
use crate::parser::expressions::Expr;
use crate::parser::statements::{Stmt};
use crate::parser::{Identifier, IdentifierHandle};
use fnv::FnvHashMap;
use std::rc::Rc;

pub trait Exec {
    fn exec(&self, env: &Environment, stmt: &Stmt) -> EvalResult<()>;
}

impl Exec for Interpreter {
    fn exec(&self, env: &Environment, stmt: &Stmt) -> EvalResult<()> {
        match stmt {
            Stmt::Print(ps) => {
                let val = self.eval(env, &ps.value)?;
                (self.host.print)(format!("{}", val));
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.eval(env, &expr.expr)?;
                Ok(())
            }
            Stmt::VarDecl(decl) => {
                let mut value = Value::Nil;

                if let Some(init_expr) = &decl.initializer {
                    value = self.eval(env, init_expr)?;
                }

                env.define(decl.identifier.name, value);
                Ok(())
            }
            Stmt::Block(block) => {
                let inner_env = Environment::new(Some(env));

                for stmt in &block.stmts {
                    self.exec(&inner_env, stmt)?;
                }

                Ok(())
            },
            Stmt::If(if_stmt) => {
                if (self.eval(env, &if_stmt.condition)?).is_truthy() {
                    self.exec(env, &if_stmt.then_branch)?;
                } else {
                    if let Some(else_branch) = &if_stmt.else_branch {
                        self.exec(env, else_branch)?;
                    }
                }

                Ok(())
            }
            Stmt::While(while_stmt) => {
                while (self.eval(env, &while_stmt.condition)?).is_truthy() {
                    self.exec(env, &while_stmt.body)?;
                }

                Ok(())
            }
            Stmt::Return(ret_stmt) => {
                let value = if let Some(val) = &ret_stmt.value {
                    self.eval(env, &val)?
                } else {
                    Value::Nil
                };

                Err(EvalError::Return(value))
            }
            Stmt::ClassDecl(class_decl) => {
                let mut superclass = None;
                if let Some(parent_class) = &class_decl.superclass {
                    let val = self.eval(env, &Expr::Var(parent_class.clone()))?;
                    let type_ = val.type_();
                    if let Some(callable) = &val.into_callable_value() {
                        match callable {
                            CallableValue::Class(c) => {
                                superclass = Some(Rc::clone(c));
                            }
                            _ => {
                                return Err(EvalError::SuperclassMustBeAClass(
                                    type_
                                ))
                            }
                        }
                    } else {
                        return Err(EvalError::SuperclassMustBeAClass(
                            type_,
                        ));
                    }
                }

                let mut environment = env.clone();

                environment.define(class_decl.identifier.name, Value::Nil);

                if let Some(parent_class) = &superclass {
                    environment = Environment::new(Some(&environment));
                    environment.define(
                        Identifier::super_(),
                        Value::Callable(CallableValue::Class(Rc::clone(parent_class))),
                    );
                }

                let mut methods: FnvHashMap<IdentifierHandle, Rc<LoxFunction>> =
                    FnvHashMap::default();

                for method in &class_decl.methods {
                    let name = method.name.unwrap().name;
                    let func = LoxFunction::new(
                        method.clone(),
                        environment.clone(),
                        name == Identifier::init(),
                    );
                    methods.insert(name, Rc::new(func));
                }

                let lox_class = Rc::new(LoxClass::new(
                    class_decl.identifier.name,
                    superclass,
                    methods,
                ));
                let callable_class = Value::Callable(CallableValue::Class(lox_class));

                if let Some(_) = &class_decl.superclass {
                    environment = env.clone();
                }

                environment.assign(0, class_decl.identifier.name, callable_class.clone());

                Ok(())
            }
        }
    }
}
