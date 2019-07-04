use super::eval_result::{EvalError, EvalResult};
use super::execute::Exec;
use super::lox_callable::LoxCallable;
use super::lox_instance::LoxInstance;
use super::Environment;
use super::Interpreter;
use crate::parser::Identifier;
use super::Value;
use crate::parser::expressions::FuncExpr;

pub struct LoxFunction {
    pub func: FuncExpr,
    pub env: Environment,
}

impl LoxFunction {
    pub fn new(func: FuncExpr, env: Environment) -> LoxFunction {
        LoxFunction { func, env }
    }

    pub fn bind(&self, instance: &LoxInstance) -> LoxFunction {
        let new_env = Environment::new(Some(&self.env));
        new_env.define(Identifier::this(), Value::Instance(instance.clone()));

        LoxFunction::new(self.func.clone(), new_env)
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        _env: &Environment,
        args: Vec<Value>,
    ) -> EvalResult<Value> {
        let func_env = Environment::new(Some(&self.env));

        for (index, param) in self.func.params.iter().enumerate() {
            func_env.define(param.name, args[index].clone());
        }

        for stmt in &self.func.body {
            match interpreter.exec(&func_env, stmt) {
                Err(EvalError::Return(val)) => return Ok(val),
                _ => {}
            }
        }

        Ok(Value::Nil)
    }

    fn arity(&self) -> usize {
        self.func.params.len()
    }
}

impl std::fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name = if let Some(_) = &self.func.name {
            "func"
        } else {
            "anonymous"
        };

        write!(f, "<fn {}>", name)
    }
}
