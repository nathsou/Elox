pub mod interp;

use crate::interpreter::lexical_scope::LexicalScopeResolutionError;
use crate::interpreter::{eval_result::EvalError};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use crate::parser::{parser_result::ParserError};

use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::process;

pub enum EloxError {
    Parser(ParserError),
    Eval(EvalError),
    Resolution(LexicalScopeResolutionError),
}

impl std::fmt::Display for EloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EloxError::Eval(eval_err) => write!(f, "{}", eval_err),
            EloxError::Parser(parser_err) => write!(f, "{}", parser_err),
            EloxError::Resolution(res_error) => write!(f, "{}", res_error),
        }
    }
}

impl ErrorPosition for EloxError {
    fn position(&self) -> &Position {
        match self {
            EloxError::Eval(err) => err.position(),
            EloxError::Parser(err) => err.position(),
            EloxError::Resolution(err) => err.position(),
        }
    }
}

pub type EloxResult = Result<(), EloxError>;

pub trait EloxRunner {
    fn run(&mut self, source: &str) -> EloxResult;
    fn throw_error(&mut self, err: impl ErrorPosition);
}

pub trait EloxFileAndPromptRunner {
    fn run_file(&mut self, path: &Path);
    fn run_prompt(&mut self);
}

impl<R: EloxRunner> EloxFileAndPromptRunner for R {
    fn run_file(&mut self, path: &Path) {
        let contents = fs::read_to_string(path).expect("incorrect file path");
        if let Err(err) = self.run(&contents) {
            self.throw_error(err);
            process::exit(65);
        }
    }

    fn run_prompt(&mut self) {
        println!("Welcome to the elox REPL");

        loop {
            print!("> ");
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("failed to read line");

            if let Err(err) = self.run(&input) {
                self.throw_error(err);
            }
        }
    }
}
