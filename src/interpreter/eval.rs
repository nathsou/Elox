use super::environment::Environment;
use super::eval_result::{EvalError, EvalResult};
use super::lox_function::LoxFunction;
use super::value::Value;
use crate::interpreter::Interpreter;
use crate::parser::expressions::{BinaryOperator, Expr, Literal, LogicalOperator, UnaryOperator};
use std::ops::Deref;
use std::rc::Rc;

pub trait Eval {
    fn eval(&self, env: &Environment, expr: &Expr) -> EvalResult<Value>;
}

impl Eval for Interpreter {
    fn eval(&self, env: &Environment, expr: &Expr) -> EvalResult<Value> {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Nil => Ok(Value::Nil),
                Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            },
            Expr::Grouping(sub_expr) => self.eval(env, &sub_expr.deref().expression),
            Expr::Unary(sub_expr) => {
                let expr = sub_expr.deref();
                let val = self.eval(env, &expr.right)?;
                match expr.operator {
                    UnaryOperator::Minus => {
                        if let Value::Number(nb) = val {
                            Ok(Value::Number(-nb))
                        } else {
                            Err(EvalError::UnexpectedUnaryOperatorOperand(
                                UnaryOperator::Minus,
                                val,
                            ))
                        }
                    }
                    UnaryOperator::Bang => Ok(Value::Boolean(!val.is_truthy())),
                }
            }
            Expr::Binary(bin_expr) => {
                let expr = bin_expr.deref();
                let a = self.eval(env, &expr.left)?;
                let b = self.eval(env, &expr.right)?;

                match expr.operator {
                    BinaryOperator::Minus => arithmetic_op(&a, &b, |a, b| Value::Number(a - b)),
                    BinaryOperator::Plus => match (&a, &b) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => {
                            Ok(Value::String(format!("{}{}", a, b)))
                        }
                        (Value::String(_), _) => Err(EvalError::UnexpectedStringConcatOperand(b)),
                        (_, Value::String(_)) => Err(EvalError::UnexpectedStringConcatOperand(a)),
                        _ => Err(EvalError::UnexpectedBinaryOperatorOperands()),
                    },
                    BinaryOperator::Slash => arithmetic_op(&a, &b, |a, b| Value::Number(a / b)),
                    BinaryOperator::Star => arithmetic_op(&a, &b, |a, b| Value::Number(a * b)),
                    BinaryOperator::Percent => arithmetic_op(&a, &b, |a, b| Value::Number(a % b)),

                    BinaryOperator::Greater => arithmetic_op(&a, &b, |a, b| Value::Boolean(a > b)),
                    BinaryOperator::GreaterEqual => {
                        arithmetic_op(&a, &b, |a, b| Value::Boolean(a >= b))
                    }
                    BinaryOperator::Less => arithmetic_op(&a, &b, |a, b| Value::Boolean(a < b)),
                    BinaryOperator::LessEqual => {
                        arithmetic_op(&a, &b, |a, b| Value::Boolean(a <= b))
                    }
                    BinaryOperator::EqualEqual => Ok(Value::Boolean(a == b)),
                    BinaryOperator::BangEqual => Ok(Value::Boolean(a != b)),
                }
            }

            Expr::Var(var_expr) => {
                if let Some(value) = self.lookup_variable(env, &var_expr.identifier) {
                    return Ok(value);
                }

                Err(EvalError::UndefinedVariable(var_expr.identifier.name))
            }
            Expr::Assign(expr) => {
                let assign_expr = expr.deref();
                let value = self.eval(env, &assign_expr.expr)?;

                if self.assign_variable(env, &expr.identifier, value.clone()) {
                    return Ok(value);
                }

                Err(EvalError::UndefinedVariable(assign_expr.identifier.name))
            }
            Expr::Logical(expr) => {
                let left = self.eval(env, &expr.left)?;
                let truthy = left.is_truthy();

                match &expr.operator {
                    LogicalOperator::Or => {
                        if truthy {
                            return Ok(left);
                        }
                    }
                    LogicalOperator::And => {
                        if !truthy {
                            return Ok(left);
                        }
                    }
                }

                self.eval(env, &expr.right)
            }
            Expr::Call(call_expr) => {
                let callee = self.eval(env, &call_expr.callee)?;
                let mut args = Vec::with_capacity(call_expr.args.len());

                for arg in &call_expr.args {
                    args.push(self.eval(env, arg)?);
                }

                if let Some(callable) = callee.into_callable() {
                    if callable.arity() != args.len() {
                        return Err(EvalError::WrongNumberOfArgs(callable.arity(), args.len()));
                    }

                    return Ok(callable.call(&self, env, args)?);
                } else {
                    return Err(EvalError::ValueNotCallable());
                }
            }
            Expr::Func(func_expr) => {
                let func = LoxFunction::new(func_expr.clone(), env.clone());
                let f = Value::Callable(Rc::new(func));

                // if not anonymous
                if let Some(identifier) = func_expr.name {
                    env.define(identifier.name, f.clone());
                }

                Ok(f)
            }
            Expr::Get(get_expr) => {
                let val = self.eval(env, &get_expr.object)?;

                if let Some(instance) = val.into_instance() {
                    if let Some(prop_val) = instance.get(get_expr.property.name) {
                        return Ok(prop_val);
                    } else {
                        return Err(EvalError::UndefinedProperty(get_expr.property.name));
                    }
                }
                
                Err(EvalError::OnlyInstancesHaveProperties())
            }
            Expr::Set(set_expr) => {
                let obj = self.eval(env, &set_expr.object)?;

                if let Some(instance) = &obj.into_instance() {
                    let val = self.eval(env, &set_expr.value)?;
                    instance.set(set_expr.property.name, &val);
                    return Ok(val);
                }

                Err(EvalError::OnlyInstancesHaveProperties())
            }
            Expr::This(this_expr) => {
                if let Some(this) = self.lookup_variable(env, &this_expr.identifier) {
                    return Ok(this);
                }   

                Ok(Value::Nil)
            }
        }
    }
}

#[inline]
fn arithmetic_op<F>(a: &Value, b: &Value, op: F) -> EvalResult<Value>
where
    F: Fn(&f64, &f64) -> Value,
{
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(op(a, b)),
        _ => Err(EvalError::UnexpectedBinaryOperatorOperands()),
    }
}
