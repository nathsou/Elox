use crate::interpreter::value::Value;
use crate::parser::expressions::UnaryOperator;
use std::fmt;

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalError {
    UnexpectedUnaryOperatorOperand(UnaryOperator, Value),
    UnexpectedBinaryOperatorOperands(),
    UndefinedVariable(String),
    ValueNotCallable(String),
    WrongNumberOfArgs(usize, usize, String),
    CouldNotGetTime(),
    OnlyInstancesHaveProperties(String),
    UndefinedProperty(String),
    SuperclassMustBeAClass(String),
    ToStringMethodMustReturnAString(String, String),
    Return(Value),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnexpectedUnaryOperatorOperand(op, val) => write!(
                f,
                "unexpected operand type for operator: '{:?}' found '{:?}', expected a number",
                op, val
            ),
            EvalError::UndefinedVariable(id) => write!(f, "Undefined variable: '{}'", id),
            EvalError::UnexpectedBinaryOperatorOperands() => {
                write!(f, "Unexpected binary operator operands")
            }
            EvalError::ValueNotCallable(typ) => {
                write!(f, "Value of type '{}' is not callable", typ)
            }
            EvalError::WrongNumberOfArgs(expected, got, name) => {
                write!(f, "'{}' expected {} arguments, got {}", name, expected, got)
            }
            EvalError::CouldNotGetTime() => write!(f, "Could not get time"),
            EvalError::Return(_) => unreachable!(),
            EvalError::OnlyInstancesHaveProperties(typ) => write!(f, "Only instances have properties, found: '{}'", typ),
            EvalError::UndefinedProperty(id) => write!(f, "Undefined property: '{}'", id),
            EvalError::SuperclassMustBeAClass(typ) => {
                write!(f, "Superclass must be a class, found: '{}'", typ)
            }
            EvalError::ToStringMethodMustReturnAString(class_name, return_type) => {
                write!(f, "#str trait method must return a string, got '{}' in class '{}'", return_type, class_name)
            }
        }
    }
}
