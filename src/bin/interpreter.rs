extern crate elox;

use crate::elox::interpreter::host::Host;
use crate::elox::runner::interp::EloxInterpreter;
use crate::elox::runner::EloxFileAndPromptRunner;

fn main() {
    let mut elox = EloxInterpreter::new(Host::default());
    if let Err(err) = elox.run_from_std_args() {
        println!("{}", err);
    }
}
