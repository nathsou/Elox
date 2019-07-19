extern crate elox;

use crate::elox::interpreter::host::Host;
use crate::elox::runner::interp::EloxInterpreter;
use crate::elox::runner::EloxFileAndPromptRunner;
use std::env;
use std::path::Path;
use std::process;

fn main() {
    let mut elox = EloxInterpreter::new(Host::default());
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => elox.run_prompt(),
        2 => elox.run_file(Path::new(&args[1])),
        _ => {
            println!("Usage: elox [script]");
            process::exit(64);
        }
    }
}
