use super::TokenType;
use std::fmt;
use super::IdentifierHandle;

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    UnmatchingClosingParen(),
    ExpectedStatement(),
    ExpectedSemicolonAfterExpr(),
    UnexpectedToken(TokenType),
    ExpectedVarName(TokenType),
    InvalidAssignmentTarget(),
    ExpectedRightBraceAfterBlock(),
    ExpectedLeftParenAfterIf(),
    ExpectedRightParenAfterIf(),
    ExpectedLeftParenAfterLoop(),
    ExpectedRightParenAfterLoop(),
    ExpectedSemicolonAfterLoopCondition(),
    ExpectedRightParenAfterForClauses(),
    ExpectedRightParenAfterCallExpr(),
    ExpectedFuncParamName(),
    ExpectedLeftBraceBeforeFuncBody(),
    ExpectedSemiColonAfterReturnValue(),
    ExpectedClassName(),
    ExpectedLeftBraceBeforeClassBody(),
    ExpectedRightBraceAfterClassBody(),
    ExpectedPropertyNameAfterDot(),
    ExpectedMethodDeclarationInClass(IdentifierHandle),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::UnmatchingClosingParen() => write!(f, "Expected ')' after expression"),
            ParserError::ExpectedStatement() => write!(f, "Expected a statement"),
            ParserError::ExpectedSemicolonAfterExpr() => write!(f, "Expected ';' after expression"),
            ParserError::UnexpectedToken(t) => write!(f, "Unexpected token: {:?}", t),
            ParserError::ExpectedVarName(t) => write!(f, "Expected variable name, got: {:?}", t),
            ParserError::InvalidAssignmentTarget() => write!(f, "Invalid assignment target"),
            ParserError::ExpectedRightBraceAfterBlock() => write!(f, "Expected '}}' after block"),
            ParserError::ExpectedLeftParenAfterIf() => write!(f, "Expected '(' after 'if'"),
            ParserError::ExpectedRightParenAfterIf() => write!(f, "Expected ')' after 'if'"),
            ParserError::ExpectedLeftParenAfterLoop() => write!(f, "Expected '(' after loop"),
            ParserError::ExpectedRightParenAfterLoop() => write!(f, "Expected ')' after loop"),
            ParserError::ExpectedRightParenAfterCallExpr() => {
                write!(f, "Expected ')' after call arguments")
            }
            ParserError::ExpectedSemicolonAfterLoopCondition() => {
                write!(f, "Expected ';' after loop condition")
            }
            ParserError::ExpectedRightParenAfterForClauses() => {
                write!(f, "Expected ')' after for clauses")
            }
            ParserError::ExpectedFuncParamName() => {
                write!(f, "Expected parameter name for function")
            }
            ParserError::ExpectedLeftBraceBeforeFuncBody() => {
                write!(f, "Expected '{{' before function body")
            }
            ParserError::ExpectedSemiColonAfterReturnValue() => {
                write!(f, "Expected ';' after return value")
            }
            ParserError::ExpectedClassName() => {
                write!(f, "Expected class name")
            }
            ParserError::ExpectedLeftBraceBeforeClassBody() => {
                write!(f, "Expected '{{' before class body")
            }
            ParserError::ExpectedRightBraceAfterClassBody() => {
                write!(f, "Expected '}}' after class body")
            }
            ParserError::ExpectedPropertyNameAfterDot() => {
                write!(f, "Expected property name after '.'")
            }
            ParserError::ExpectedMethodDeclarationInClass(class_id) => {
                write!(f, "Expected method declaration in class: {}", class_id)
            }
        }
    }
}
