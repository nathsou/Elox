pub mod token_type;
use std::fmt;
use token_type::TokenType;

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn newline(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn next(&mut self) {
        self.col += 1;
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}:{}]", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub pos: Position,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, col: usize) -> Self {
        Token {
            token_type,
            lexeme,
            pos: Position { line, col },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?} -> {}]", self.token_type, self.lexeme)
    }
}
