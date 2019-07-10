pub mod scanner_result;
pub mod token;
use scanner_result::{ScannerError, ScannerResult};
use std::iter::Peekable;
use std::str::Chars;
use token::{token_type::TokenType, Token};

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    current_lexeme: String,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: Peekable<Chars<'a>>) -> Self {
        Scanner {
            source,
            current_lexeme: "".into(),
            line: 1,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();

        if let Some(c) = next {
            self.current_lexeme.push(c);
            if c == '\n' {
                self.line += 1;
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

    fn skip_line(&mut self) {
        while self.source.peek() != None && self.source.peek() != Some(&'\n') {
            self.source.next();
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.source.peek() {
            if !c.is_whitespace() {
                return;
            } else if c == '\n' {
                self.line += 1;
            }

            self.source.next();
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
            Some('-') => Ok(self.token(Minus)),
            Some('+') => Ok(self.token(Plus)),
            Some(';') => Ok(self.token(SemiColon)),
            Some('*') => Ok(self.token(Star)),
            Some('%') => Ok(self.token(Percent)),
            Some('!') => Ok(self.match_op(BangEqual, Bang)),
            Some('=') => Ok(self.match_op(EqualEqual, Equal)),
            Some('<') => Ok(self.match_op(LessEqual, Less)),
            Some('>') => Ok(self.match_op(GreaterEqual, Greater)),
            Some('/') => {
                if self.source.peek() == Some(&'/') {
                    // it's a comment
                    self.skip_line();
                    self.scan_token()
                } else {
                    Ok(self.token(Slash))
                }
            }
            Some('"') => self.scan_string(),
            Some(c) => {
                if c == '\n' {
                    self.line += 1;
                    self.scan_token()
                } else if c.is_digit(10) {
                    self.scan_number()
                } else if c.is_alphabetic() || c == '_' || c == '#' {
                    self.scan_identifier()
                } else {
                    Err(ScannerError::UnexpectedCharacter(c, self.line))
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
            } else if c == '\n' {
                self.line += 1;
            } else {
                self.advance();
            }
        }

        Err(ScannerError::UnterminatedString(self.line))
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
        Token::new(token_type, self.current_lexeme.clone(), self.line)
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
