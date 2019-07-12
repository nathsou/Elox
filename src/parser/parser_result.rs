use super::IdentifierHandle;
use super::Position;
use crate::scanner::scanner_result::{ErrorPosition, ScannerError};
use std::fmt;

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ScannerError(ScannerError),
    UnmatchingClosingParen(Position),
    ExpectedStatement(Position),
    ExpectedSemicolonAfterExpr(Position),
    UnexpectedToken(Position, String),
    ExpectedVarName(Position, String),
    InvalidAssignmentTarget(Position),
    ExpectedRightBraceAfterBlock(Position),
    ExpectedLeftParenAfterIf(Position),
    ExpectedRightParenAfterIf(Position),
    ExpectedLeftParenAfterLoop(Position),
    ExpectedRightParenAfterLoop(Position),
    ExpectedSemicolonAfterLoopCondition(Position),
    ExpectedRightParenAfterForClauses(Position),
    ExpectedRightParenAfterCallExpr(Position),
    ExpectedFuncParamName(Position),
    ExpectedLeftBraceBeforeFuncBody(Position),
    ExpectedSemiColonAfterReturnValue(Position),
    ExpectedClassName(Position),
    ExpectedLeftBraceBeforeClassBody(Position),
    ExpectedRightBraceAfterClassBody(Position),
    ExpectedPropertyNameAfterDot(Position),
    ExpectedMethodDeclarationInClass(Position, IdentifierHandle),
    ExpectedSuperclassName(Position),
    ExpectedSuperclassMethodName(Position),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ScannerError(e) => write!(f, "{}", e),
            ParserError::UnmatchingClosingParen(_) => write!(f, "Expected ')' after expression"),
            ParserError::ExpectedStatement(_) => write!(f, "Expected a statement"),
            ParserError::ExpectedSemicolonAfterExpr(_) => {
                write!(f, "Expected ';' after expression")
            }
            ParserError::UnexpectedToken(_, t) => write!(f, "Unexpected token: '{}'", t),
            ParserError::ExpectedVarName(_, t) => write!(f, "Expected variable name, got: '{}'", t),
            ParserError::InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
            ParserError::ExpectedRightBraceAfterBlock(_) => write!(f, "Expected '}}' after block"),
            ParserError::ExpectedLeftParenAfterIf(_) => write!(f, "Expected '(' after 'if'"),
            ParserError::ExpectedRightParenAfterIf(_) => write!(f, "Expected ')' after 'if'"),
            ParserError::ExpectedLeftParenAfterLoop(_) => write!(f, "Expected '(' after loop"),
            ParserError::ExpectedRightParenAfterLoop(_) => write!(f, "Expected ')' after loop"),
            ParserError::ExpectedRightParenAfterCallExpr(_) => {
                write!(f, "Expected ')' after call arguments")
            }
            ParserError::ExpectedSemicolonAfterLoopCondition(_) => {
                write!(f, "Expected ';' after loop condition")
            }
            ParserError::ExpectedRightParenAfterForClauses(_) => {
                write!(f, "Expected ')' after for clauses")
            }
            ParserError::ExpectedFuncParamName(_) => {
                write!(f, "Expected parameter name for function")
            }
            ParserError::ExpectedLeftBraceBeforeFuncBody(_) => {
                write!(f, "Expected '{{' before function body")
            }
            ParserError::ExpectedSemiColonAfterReturnValue(_) => {
                write!(f, "Expected ';' after return value")
            }
            ParserError::ExpectedClassName(_) => write!(f, "Expected class name"),
            ParserError::ExpectedLeftBraceBeforeClassBody(_) => {
                write!(f, "Expected '{{' before class body")
            }
            ParserError::ExpectedRightBraceAfterClassBody(_) => {
                write!(f, "Expected '}}' after class body")
            }
            ParserError::ExpectedPropertyNameAfterDot(_) => {
                write!(f, "Expected property name after '.'")
            }
            ParserError::ExpectedMethodDeclarationInClass(_, class_id) => {
                write!(f, "Expected method declaration in class: {}", class_id)
            }
            ParserError::ExpectedSuperclassName(_) => write!(f, "Expected superclass name"),
            ParserError::ExpectedSuperclassMethodName(_) => {
                write!(f, "Expected superclass method name")
            }
        }
    }
}

impl ErrorPosition for ParserError {
    fn position(&self) -> &Position {
        use ParserError::*;

        match self {
            ScannerError(e) => e.position(),
            UnmatchingClosingParen(pos) => pos,
            ExpectedStatement(pos) => pos,
            ExpectedSemicolonAfterExpr(pos) => pos,
            UnexpectedToken(pos, _) => pos,
            ExpectedVarName(pos, _) => pos,
            InvalidAssignmentTarget(pos) => pos,
            ExpectedRightBraceAfterBlock(pos) => pos,
            ExpectedLeftParenAfterIf(pos) => pos,
            ExpectedRightParenAfterIf(pos) => pos,
            ExpectedLeftParenAfterLoop(pos) => pos,
            ExpectedRightParenAfterLoop(pos) => pos,
            ExpectedSemicolonAfterLoopCondition(pos) => pos,
            ExpectedRightParenAfterForClauses(pos) => pos,
            ExpectedRightParenAfterCallExpr(pos) => pos,
            ExpectedFuncParamName(pos) => pos,
            ExpectedLeftBraceBeforeFuncBody(pos) => pos,
            ExpectedSemiColonAfterReturnValue(pos) => pos,
            ExpectedClassName(pos) => pos,
            ExpectedLeftBraceBeforeClassBody(pos) => pos,
            ExpectedRightBraceAfterClassBody(pos) => pos,
            ExpectedPropertyNameAfterDot(pos) => pos,
            ExpectedMethodDeclarationInClass(pos, _) => pos,
            ExpectedSuperclassName(pos) => pos,
            ExpectedSuperclassMethodName(pos) => pos,
        }
    }
}
