use super::environment::Environment;
use super::eval::Eval;
use super::value::Value;
use crate::interpreter::{
    eval_result::{EvalError, EvalResult},
    Interpreter,
};
use crate::parser::statements::Stmt;

pub trait Exec {
    fn exec(&self, env: &Environment, stmt: &Stmt) -> EvalResult<()>;
}

impl Exec for Interpreter {
    fn exec(&self, env: &Environment, stmt: &Stmt) -> EvalResult<()> {
        match stmt {
            Stmt::Print(ps) => {
                let val = self.eval(env, &ps.value)?;
                println!("{}", val);
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
            }
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
        }
    }
}
