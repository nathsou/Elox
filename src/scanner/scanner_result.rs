use std::fmt;
use super::Position;

pub type ScannerResult<T> = Result<T, ScannerError>;

#[derive(Debug, Clone)]
pub enum ScannerError {
    UnexpectedCharacter(Position, char),
    UnterminatedString(Position),
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScannerError::UnexpectedCharacter(_, c) => {
                write!(f, "Unexpected character '{}'", c)
            }
            ScannerError::UnterminatedString(_) => {
                write!(f, "Unterminated string")
            }
        }
    }
}

impl ScannerError {
    pub fn position(&self) -> &Position {
        use ScannerError::*;
        match self {
            UnexpectedCharacter(pos, _) => pos,
            UnterminatedString(pos) => pos,
        }
    }
}