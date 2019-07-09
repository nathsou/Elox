use crate::interpreter::Interpreter;
// use crate::parser::pretty_printer::PrettyPrinter;
use crate::interpreter::environment::Environment;
use crate::interpreter::host::Host;
use crate::interpreter::lexical_scope::Resolver;
use crate::parser::{IdentifierHandlesGenerator, Parser};
use crate::scanner::Scanner;

use std::fs;
use std::io;
use std::path::Path;
use std::process;
use std::rc::Rc;

pub struct Lox {
    had_error: bool,
    host: Rc<Host>,
}

impl Lox {
    pub fn new(host: Host) -> Lox {
        Lox {
            had_error: false,
            host: Rc::new(host),
        }
    }

    pub fn run_file(&mut self, path: &Path) {
        let contents = fs::read_to_string(path).expect("incorrect file path");
        self.run(&contents);
        if self.had_error {
            process::exit(65);
        }
    }

    pub fn run_prompt(&mut self) {
        println!("Welcome to the lox REPL");

        loop {
            print!("> ");
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("failed to read line");

            self.run(&input);
            if self.had_error {
                self.had_error = false;
            }
        }
    }

    pub fn run(&mut self, source: &str) {
        let scanner = Scanner::new(source.chars().peekable());
        let mut identifiers = IdentifierHandlesGenerator::new();
        let global = Environment::with_natives(None, &mut identifiers);
        let mut parser = Parser::new(scanner.peekable(), identifiers);
        let mut interpreter = Interpreter::new(global, &self.host);
        let mut resolver = Resolver::new(&mut interpreter);

        match parser.parse() {
            Ok(ast) => {
                // for stmt in &ast {
                //     println!("{}", stmt.pretty_print());
                // }

                match resolver.resolve(&ast) {
                    Ok(()) => {
                        let res = interpreter.interpret(&ast);
                        match res {
                            Ok(()) => {}
                            Err(err) => {
                                self.had_error = true;
                                (self.host.error)(format!("{}", err));
                            }
                        }
                    }
                    Err(err) => {
                        self.had_error = true;
                        (self.host.error)(format!("{}", err));
                    }
                }
            }
            Err(err) => {
                self.had_error = true;
                (self.host.error)(format!("{}", err));
            }
        }
    }
}
