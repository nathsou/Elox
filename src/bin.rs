mod interpreter;
mod lox;
mod parser;
mod scanner;

use interpreter::host::Host;
use lox::Lox;
use std::env;
use std::path::Path;
use std::process;

fn main() {
    let mut lox = Lox::new(Host::default());
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => lox.run_prompt(),
        2 => lox.run_file(Path::new(&args[1])),
        _ => {
            println!("Usage: lox [script]");
            process::exit(64);
        }
    }
}
