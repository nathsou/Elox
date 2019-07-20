extern crate elox;

use crate::elox::runner::EloxFileAndPromptRunner;
use crate::elox::vm::EloxVM;

fn main() {
    let mut vm = EloxVM::new();
    vm.run_from_std_args();
}
