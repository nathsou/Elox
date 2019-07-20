pub mod interp;

use crate::interpreter::eval_result::EvalError;
use crate::interpreter::lexical_scope::LexicalScopeResolutionError;
use crate::parser::parser_result::ParserError;
use crate::scanner::scanner_result::{ErrorPosition, ScannerError};
use crate::scanner::token::Position;

use std::env;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::process;

pub enum EloxError {
    Scanner(ScannerError),
    Parser(ParserError),
    Eval(EvalError),
    Resolution(LexicalScopeResolutionError),
}

impl std::fmt::Display for EloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EloxError::Scanner(scanner_err) => write!(f, "{}", scanner_err),
            EloxError::Eval(eval_err) => write!(f, "{}", eval_err),
            EloxError::Parser(parser_err) => write!(f, "{}", parser_err),
            EloxError::Resolution(res_error) => write!(f, "{}", res_error),
        }
    }
}

impl ErrorPosition for EloxError {
    fn position(&self) -> &Position {
        match self {
            EloxError::Scanner(err) => err.position(),
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
    fn run_from_std_args(&mut self);
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

    fn run_from_std_args(&mut self) {
        let args: Vec<String> = env::args().collect();

        match args.len() {
            1 => self.run_prompt(),
            2 => self.run_file(Path::new(&args[1])),
            _ => {
                println!("Usage: elox [script]");
                process::exit(64);
            }
        }
    }
}
