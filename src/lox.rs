use crate::interpreter::Interpreter;
// use crate::parser::pretty_printer::PrettyPrinter;
use crate::interpreter::environment::Environment;
use crate::parser::IdentifierHandlesGenerator;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::interpreter::lexical_scope::Resolver;
use std::fs;
use std::io;
use std::path::Path;
use std::process;

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Lox {
        Lox { had_error: false }
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

    fn run(&mut self, source: &String) {
        let scanner = Scanner::new(source.chars().peekable());
        let mut identifiers = IdentifierHandlesGenerator::new();
        let global = Environment::with_natives(None, &mut identifiers);
        let mut parser = Parser::new(scanner.peekable(), identifiers);
        let mut interpreter = Interpreter::new(global);
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
                            Err(err) => println!("{}", err),
                        }
                    }
                    Err(e) => {
                        println!("{}", e);
                    }
                }

            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
