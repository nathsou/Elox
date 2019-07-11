use crate::interpreter::value::Value;
use crate::parser::expressions::{UnaryOperator, BinaryOperator};
use std::fmt;
use crate::scanner::token::Position;
use crate::scanner::scanner_result::ErrorPosition;

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalError {
    UnexpectedUnaryOperatorOperand(Position, UnaryOperator, Value),
    UnexpectedBinaryOperatorOperands(Position, BinaryOperator, String, String),
    UndefinedVariable(Position, String),
    ValueNotCallable(Position, String),
    WrongNumberOfArgs(Position, usize, usize, String),
    CouldNotGetTime(),
    OnlyInstancesHaveProperties(Position, String),
    UndefinedProperty(Position, String),
    SuperclassMustBeAClass(Position, String),
    ToStringMethodMustReturnAString(Position, String, String),
    Return(Value),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnexpectedUnaryOperatorOperand(_, op, val) => write!(
                f,
                "Unexpected operand type for operator: '{}' found '{}', expected a number",
                op, val.type_()
            ),
            EvalError::UndefinedVariable(_, id) => write!(f, "Undefined variable: '{}'", id),
            EvalError::UnexpectedBinaryOperatorOperands(_, op, a, b) => {
                write!(f, "Unexpected operand types for operator: '{}', found '{}' and '{}'", op, a, b)
            }
            EvalError::ValueNotCallable(_, typ) => {
                write!(f, "Value of type '{}' is not callable", typ)
            }
            EvalError::WrongNumberOfArgs(_, expected, got, name) => {
                write!(f, "'{}' expected {} arguments, got {}", name, expected, got)
            }
            EvalError::CouldNotGetTime() => write!(f, "Could not get time"),
            EvalError::Return(_) => unreachable!(),
            EvalError::OnlyInstancesHaveProperties(_, typ) => write!(f, "Only instances have properties, found: '{}'", typ),
            EvalError::UndefinedProperty(_, id) => write!(f, "Undefined property: '{}'", id),
            EvalError::SuperclassMustBeAClass(_, typ) => {
                write!(f, "Superclass must be a class, found: '{}'", typ)
            }
            EvalError::ToStringMethodMustReturnAString(_, class_name, return_type) => {
                write!(f, "#str trait method must return a string, got '{}' in class '{}'", return_type, class_name)
            }
        }
    }
}

impl ErrorPosition for EvalError {
    fn position(&self) -> &Position {
        use EvalError::*;
        match self {
            UnexpectedUnaryOperatorOperand(pos, _, _) |
            UnexpectedBinaryOperatorOperands(pos,_,  _, _) |
            UndefinedVariable(pos, _) |
            ValueNotCallable(pos, _) |
            WrongNumberOfArgs(pos, _, _, _) |
            OnlyInstancesHaveProperties(pos, _) |
            UndefinedProperty(pos, _) |
            SuperclassMustBeAClass(pos, _) |
            ToStringMethodMustReturnAString(pos, _, _) => pos,
            CouldNotGetTime() => panic!(format!("{}", self)),
            Return(_) => unreachable!(),
        }
    }
}