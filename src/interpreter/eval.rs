use super::environment::Environment;
use super::eval_result::{EvalError, EvalResult};
use super::lox_function::LoxFunction;
use super::value::{CallableValue, Value};
use crate::interpreter::Interpreter;
use crate::parser::expressions::{BinaryOperator, BinaryOperatorCtx, ExprCtx, Expr, Literal, LogicalOperator, UnaryOperator};
use crate::parser::Identifier;
use std::ops::Deref;
use std::rc::Rc;

pub trait Eval {
    fn eval(&self, env: &Environment, expr: &ExprCtx) -> EvalResult<Value>;
}

impl Eval for Interpreter {
    fn eval(&self, env: &Environment, expr_ctx: &ExprCtx) -> EvalResult<Value> {
        match &expr_ctx.expr {
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(n.clone())),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Nil => Ok(Value::Nil),
                Literal::Boolean(b) => Ok(Value::Boolean(b.clone())),
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
                                expr.right.pos,
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

                let op_ctx = &expr.operator;

                match op_ctx.op {
                    BinaryOperator::Minus => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Number(a - b)),
                    BinaryOperator::Plus => match (&a, &b) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (_, _) => Ok(Value::String(format!("{}{}", a, b))),
                    },
                    BinaryOperator::Slash => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Number(a / b)),
                    BinaryOperator::Star => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Number(a * b)),
                    BinaryOperator::Percent => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Number(a % b)),

                    BinaryOperator::Greater => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Boolean(a > b)),
                    BinaryOperator::GreaterEqual => {
                        arithmetic_op(op_ctx, &a, &b, |a, b| Value::Boolean(a >= b))
                    }
                    BinaryOperator::Less => arithmetic_op(op_ctx, &a, &b, |a, b| Value::Boolean(a < b)),
                    BinaryOperator::LessEqual => {
                        arithmetic_op(op_ctx, &a, &b, |a, b| Value::Boolean(a <= b))
                    }
                    BinaryOperator::EqualEqual => Ok(Value::Boolean(a == b)),
                    BinaryOperator::BangEqual => Ok(Value::Boolean(a != b)),
                }
            }

            Expr::Var(var_expr) => {
                if let Some(value) = self.lookup_variable(env, &var_expr.identifier) {
                    return Ok(value);
                }

                Err(EvalError::UndefinedVariable(expr_ctx.pos, self.name(var_expr.identifier.name)))
            }
            Expr::Assign(expr) => {
                let assign_expr = expr.deref();
                let value = self.eval(env, &assign_expr.expr)?;

                if self.assign_variable(env, &expr.identifier, value.clone()) {
                    return Ok(value);
                }

                Err(EvalError::UndefinedVariable(expr_ctx.pos, self.name(assign_expr.identifier.name)))
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

                match callee {
                    Value::Callable(callable_value) => {
                        let callable = callable_value.into_callable();
                        if callable.arity() != args.len() {
                            let name = callable.name(&self.names);
                            return Err(EvalError::WrongNumberOfArgs(expr_ctx.pos, callable.arity(), args.len(), name));
                        }

                        return Ok(callable.call(&self, env, args)?);
                    }
                    _ => return Err(EvalError::ValueNotCallable(expr_ctx.pos, callee.type_())),
                }
            }
            Expr::Func(func_expr) => {
                let func = LoxFunction::new(
                    func_expr.clone(),
                    env.clone(), false
                );

                let f = Value::Callable(CallableValue::Function(Rc::new(func)));

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
                        return Err(EvalError::UndefinedProperty(expr_ctx.pos, self.name(get_expr.property.name)));
                    }
                }

                Err(EvalError::OnlyInstancesHaveProperties(
                    expr_ctx.pos,
                    self.eval(env, &get_expr.object).unwrap().type_()))
            }
            Expr::Set(set_expr) => {
                let obj = self.eval(env, &set_expr.object)?;

                if let Some(instance) = &obj.into_instance() {
                    let val = self.eval(env, &set_expr.value)?;
                    instance.set(set_expr.property.name, &val);
                    return Ok(val);
                }

                Err(EvalError::OnlyInstancesHaveProperties(expr_ctx.pos, self.eval(env, &set_expr.object).unwrap().type_()))
            }
            Expr::This(this_expr) => {
                if let Some(this) = self.lookup_variable(env, &this_expr.identifier) {
                    return Ok(this);
                }

                Ok(Value::Nil)
            }
            Expr::Super(super_expr) => {
                if let Some(&depth) = self.depths.get(&super_expr.identifier.use_handle) {
                    if let Some(superclass) = env.get(depth, Identifier::super_()) {
                        if let Some(Value::Instance(instance)) =
                            env.get(depth - 1, Identifier::this())
                        {
                            if let Some(CallableValue::Class(parent)) =
                                superclass.into_callable_value()
                            {
                                if let Some(method) = parent.find_method(super_expr.method.name) {
                                    return Ok(Value::Callable(CallableValue::Function(Rc::new(
                                        method.bind(&instance),
                                    ))));
                                } else {
                                    return Err(EvalError::UndefinedProperty(
                                        expr_ctx.pos,
                                        self.name(super_expr.method.name)
                                    ));
                                }
                            }
                        }
                    }
                }

                Ok(Value::Nil)
            }
        }
    }
}

#[inline]
fn arithmetic_op<F>(op_ctx: &BinaryOperatorCtx, a: &Value, b: &Value, operation: F) -> EvalResult<Value>
where
    F: Fn(&f64, &f64) -> Value,
{
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(operation(a, b)),
        _ => Err(EvalError::UnexpectedBinaryOperatorOperands(op_ctx.pos.clone(), op_ctx.op.clone(), a.type_(), b.type_())),
    }
}
