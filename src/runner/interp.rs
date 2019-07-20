use super::{EloxError, EloxResult, EloxRunner};
use crate::interpreter::environment::Environment;
use crate::interpreter::host::Host;
use crate::interpreter::lexical_scope::Resolver;
use crate::interpreter::Interpreter;
use crate::parser::{IdentifierHandlesGenerator, Parser};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::Scanner;
use std::rc::Rc;

pub struct EloxInterpreter {
    host: Rc<Host>,
}

impl EloxInterpreter {
    pub fn new(host: Host) -> EloxInterpreter {
        EloxInterpreter {
            host: Rc::new(host),
        }
    }
}

impl EloxRunner for EloxInterpreter {
    fn run(&mut self, source: &str) -> EloxResult {
        let scanner = Scanner::new(source.chars().peekable());
        let mut identifiers = IdentifierHandlesGenerator::new();
        let global = Environment::with_natives(None, &mut identifiers);
        let mut parser = Parser::new(scanner.peekable(), &mut identifiers);

        match parser.parse() {
            Ok(ast) => {
                // for stmt in &ast {
                //     println!("{}", stmt.pretty_print());
                // }

                let names = Rc::new(parser.names());

                // println!("{:?}", identifier_names);

                let mut interpreter = Interpreter::new(global, &self.host, &names);
                let mut resolver = Resolver::new(&mut interpreter, &names);

                match resolver.resolve(&ast) {
                    Ok(()) => {
                        let res = interpreter.interpret(&ast);
                        match res {
                            Ok(()) => return Ok(()),
                            Err(err) => return Err(EloxError::Eval(err)),
                        }
                    }
                    Err(err) => return Err(EloxError::Resolution(err)),
                }
            }
            Err(err) => Err(EloxError::Parser(err)),
        }
    }

    fn throw_error(&mut self, err: impl ErrorPosition) {
        let pos = err.position();
        (self.host.error)(format!("{}", err), pos.line, pos.col);
    }
}
