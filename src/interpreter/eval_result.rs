use crate::interpreter::value::Value;
use crate::parser::expressions::UnaryOperator;
use crate::parser::IdentifierHandle;
use std::fmt;

pub type EvalResult<T> = Result<T, EvalError>;

pub enum EvalError {
    UnexpectedUnaryOperatorOperand(UnaryOperator, Value),
    UnexpectedBinaryOperatorOperands(),
    UndefinedVariable(IdentifierHandle),
    ValueNotCallable(),
    WrongNumberOfArgs(usize, usize),
    CouldNotGetTime(),
    OnlyInstancesHaveProperties(),
    UndefinedProperty(IdentifierHandle),
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
            EvalError::ValueNotCallable() => write!(f, "Value is not callable"),
            EvalError::WrongNumberOfArgs(expected, got) => {
                write!(f, "Expected {} arguments, got {}", expected, got)
            }
            EvalError::CouldNotGetTime() => write!(f, "Could not get time"),
            EvalError::Return(_) => unreachable!(),
            EvalError::OnlyInstancesHaveProperties() => write!(f, "Only instances have properties"),
            EvalError::UndefinedProperty(id) => write!(f, "Undefined property: {}", id),
        }
    }
}
