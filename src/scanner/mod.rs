pub mod scanner_result;
pub mod token;
use scanner_result::{ScannerError, ScannerResult};
use std::iter::Peekable;
use std::str::Chars;
use token::{token_type::TokenType, Token, Position};

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    current_lexeme: String,
    pos: Position,
}

impl<'a> Scanner<'a> {
    pub fn new(source: Peekable<Chars<'a>>) -> Self {
        Scanner {
            source,
            current_lexeme: "".into(),
            pos: Position {line: 1, col: 1},
        }
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();

        if let Some(c) = next {
            self.current_lexeme.push(c);
            if c == '\n' {
                self.pos.newline();
            } else {
                self.pos.next();
            }
        }

        next
    }

    #[allow(dead_code)]
    pub fn scan_tokens(&mut self) -> ScannerResult<Vec<Token>> {
        let mut tokens = Vec::new();

        while let Some(_) = self.source.peek() {
            self.skip_whitespace();

            match self.scan_token() {
                Ok(token) => tokens.push(token),
                Err(err) => return Err(err),
            };
        }

        Ok(tokens)
    }

    fn match_next(
        &mut self,
        matching_char: &char,
        if_token: TokenType,
        else_token: TokenType,
    ) -> Token {
        if self.source.peek() == Some(matching_char) {
            self.advance();
            self.token(if_token)
        } else {
            self.token(else_token)
        }
    }

    fn match_op(&mut self, operator_token: TokenType, simple_token: TokenType) -> Token {
        self.match_next(&'=', operator_token, simple_token)
    }

    fn match_plus_assignment_shorthand(&mut self) -> Token {
        match self.source.peek() {
            Some(&'=') => {
                self.advance();
                self.token(TokenType::PlusEqual)
            }
            Some(&'+') => {
                self.advance();
                self.token(TokenType::PlusPlus)
            }
            _ => self.token(TokenType::Plus),
        }
    }

    fn match_minus_assignment_shorthand(&mut self) -> Token {
        match self.source.peek() {
            Some(&'=') => {
                self.advance();
                self.token(TokenType::MinusEqual)
            }
            Some(&'-') => {
                self.advance();
                self.token(TokenType::MinusMinus)
            }
            _ => self.token(TokenType::Minus),
        }
    }

    fn skip_line(&mut self) {
        while self.source.peek() != None && self.source.peek() != Some(&'\n') {
            self.advance();
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.source.peek() {
            if !c.is_whitespace() {
                return;
            }

            self.advance();
        }
    }

    fn scan_token(&mut self) -> ScannerResult<Token> {
        self.current_lexeme.clear();
        let c = self.advance();

        use token::token_type::TokenType::*;

        match c {
            Some('(') => Ok(self.token(LeftParen)),
            Some(')') => Ok(self.token(RightParen)),
            Some('{') => Ok(self.token(LeftBrace)),
            Some('}') => Ok(self.token(RightBrace)),
            Some('[') => Ok(self.token(LeftBracket)),
            Some(']') => Ok(self.token(RightBracket)),
            Some(',') => Ok(self.token(Comma)),
            Some('.') => Ok(self.token(Dot)),
            Some('-') => Ok(self.match_minus_assignment_shorthand()),
            Some('+') => Ok(self.match_plus_assignment_shorthand()),
            Some(';') => Ok(self.token(SemiColon)),
            Some('*') => Ok(self.match_next(&'=', StarEqual, Star)),
            Some('%') => Ok(self.match_next(&'=', PercentEqual, Percent)),
            Some('!') => Ok(self.match_op(BangEqual, Bang)),
            Some('=') => Ok(self.match_op(EqualEqual, Equal)),
            Some('<') => Ok(self.match_op(LessEqual, Less)),
            Some('>') => Ok(self.match_op(GreaterEqual, Greater)),
            Some('/') => {
                Ok(match self.source.peek() {
                    Some(&'/') => {
                        // it's a comment
                        self.skip_line();
                        self.scan_token()?
                    }
                    Some(&'=') => {
                        self.advance();
                        self.token(SlashEqual)
                    }
                    _ => self.token(Slash),
                })
            }
            Some('"') => self.scan_string(),
            Some(c) => {
                if c == '\n' {
                    self.scan_token()
                } else if c.is_digit(10) {
                    self.scan_number()
                } else if c.is_alphabetic() || c == '_' || c == '#' {
                    self.scan_identifier()
                } else {
                    Err(ScannerError::UnexpectedCharacter(self.pos.clone(), c))
                }
            }
            _ => Ok(self.token(EOF)),
        }
    }

    fn scan_string(&mut self) -> ScannerResult<Token> {
        while let Some(&c) = self.source.peek() {
            if c == '"' {
                self.advance(); // get rid of the terminating '"'
                return Ok(self.token(TokenType::String(
                    self.current_lexeme[1..self.current_lexeme.len() - 1].to_owned(),
                )));
            } else {
                self.advance();
            }
        }

        Err(ScannerError::UnterminatedString(self.pos.clone()))
    }

    fn scan_number(&mut self) -> ScannerResult<Token> {
        while let Some(&c) = self.source.peek() {
            if c.is_digit(10) {
                self.advance();
            } else if c == '.' {
                self.advance();

                while let Some(&c) = self.source.peek() {
                    if c.is_digit(10) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        let value: f64 = self.current_lexeme.parse().unwrap();

        Ok(self.token(TokenType::Number(value)))
    }

    fn scan_identifier(&mut self) -> ScannerResult<Token> {
        while let Some(&c) = self.source.peek() {
            if c.is_alphanumeric() || c == '_' || c == '#' {
                self.advance();
            } else {
                break;
            }
        }

        Ok(self.token(self.identifier()))
    }

    fn token(&mut self, token_type: TokenType) -> Token {
        Token::new(token_type, self.current_lexeme.clone(), self.pos.line, self.pos.col)
    }

    fn identifier(&self) -> TokenType {
        use TokenType::*;

        match self.current_lexeme.as_ref() {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier(self.current_lexeme.clone()),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = ScannerResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(_) = self.source.peek() {
            self.skip_whitespace();

            return Some(self.scan_token());
        }

        None // EOF
    }
}
