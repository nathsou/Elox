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
    RestParameterMustBeLast(Position),
    OptionalParamCannotPrecedeRequiredParam(Position),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParserError::*;
        match self {
            ScannerError(e) => write!(f, "{}", e),
            UnmatchingClosingParen(_) => write!(f, "Expected ')' after expression"),
            ExpectedStatement(_) => write!(f, "Expected a statement"),
            ExpectedSemicolonAfterExpr(_) => write!(f, "Expected ';' after expression"),
            UnexpectedToken(_, t) => write!(f, "Unexpected token: '{}'", t),
            ExpectedVarName(_, t) => write!(f, "Expected variable name, got: '{}'", t),
            InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
            ExpectedRightBraceAfterBlock(_) => write!(f, "Expected '}}' after block"),
            ExpectedLeftParenAfterIf(_) => write!(f, "Expected '(' after 'if'"),
            ExpectedRightParenAfterIf(_) => write!(f, "Expected ')' after 'if'"),
            ExpectedLeftParenAfterLoop(_) => write!(f, "Expected '(' after loop"),
            ExpectedRightParenAfterLoop(_) => write!(f, "Expected ')' after loop"),
            ExpectedRightParenAfterCallExpr(_) => write!(f, "Expected ')' after call arguments"),
            ExpectedSemicolonAfterLoopCondition(_) => {
                write!(f, "Expected ';' after loop condition")
            }
            ExpectedRightParenAfterForClauses(_) => write!(f, "Expected ')' after for clauses"),
            ExpectedFuncParamName(_) => write!(f, "Expected parameter name for function"),
            ExpectedLeftBraceBeforeFuncBody(_) => write!(f, "Expected '{{' before function body"),
            ExpectedSemiColonAfterReturnValue(_) => write!(f, "Expected ';' after return value"),
            ExpectedClassName(_) => write!(f, "Expected class name"),
            ExpectedLeftBraceBeforeClassBody(_) => write!(f, "Expected '{{' before class body"),
            ExpectedRightBraceAfterClassBody(_) => write!(f, "Expected '}}' after class body"),
            ExpectedPropertyNameAfterDot(_) => write!(f, "Expected property name after '.'"),
            ExpectedMethodDeclarationInClass(_, class_id) => {
                write!(f, "Expected method declaration in class: {}", class_id)
            }
            ExpectedSuperclassName(_) => write!(f, "Expected superclass name"),
            ExpectedSuperclassMethodName(_) => write!(f, "Expected superclass method name"),
            RestParameterMustBeLast(_) => {
                write!(f, "A rest parameter must be last in a parameter list")
            }
            OptionalParamCannotPrecedeRequiredParam(_) => write!(
                f,
                "An optional parameter cannot precede a required parameter"
            ),
        }
    }
}

impl ErrorPosition for ParserError {
    fn position(&self) -> &Position {
        use ParserError::*;

        match self {
            ScannerError(e) => e.position(),
            UnmatchingClosingParen(pos)
            | ExpectedStatement(pos)
            | ExpectedSemicolonAfterExpr(pos)
            | UnexpectedToken(pos, _)
            | ExpectedVarName(pos, _)
            | InvalidAssignmentTarget(pos)
            | ExpectedRightBraceAfterBlock(pos)
            | ExpectedLeftParenAfterIf(pos)
            | ExpectedRightParenAfterIf(pos)
            | ExpectedLeftParenAfterLoop(pos)
            | ExpectedRightParenAfterLoop(pos)
            | ExpectedSemicolonAfterLoopCondition(pos)
            | ExpectedRightParenAfterForClauses(pos)
            | ExpectedRightParenAfterCallExpr(pos)
            | ExpectedFuncParamName(pos)
            | ExpectedLeftBraceBeforeFuncBody(pos)
            | ExpectedSemiColonAfterReturnValue(pos)
            | ExpectedClassName(pos)
            | ExpectedLeftBraceBeforeClassBody(pos)
            | ExpectedRightBraceAfterClassBody(pos)
            | ExpectedPropertyNameAfterDot(pos)
            | ExpectedMethodDeclarationInClass(pos, _)
            | ExpectedSuperclassName(pos)
            | ExpectedSuperclassMethodName(pos)
            | OptionalParamCannotPrecedeRequiredParam(pos)
            | RestParameterMustBeLast(pos) => pos,
        }
    }
}
