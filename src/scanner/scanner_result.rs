use std::fmt;

pub type ScannerResult<T> = Result<T, ScannerError>;

#[derive(Debug)]
pub enum ScannerError {
    UnexpectedCharacter(char, usize),
    UnterminatedString(usize),
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScannerError::UnexpectedCharacter(c, line) => {
                write!(f, "Unexpected character '{}' at line {}", c, line)
            }
            ScannerError::UnterminatedString(line) => {
                write!(f, "Unterminated string at line {}", line)
            }
        }
    }
}
