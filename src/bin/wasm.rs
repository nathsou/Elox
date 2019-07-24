extern crate elox;

// use elox::vm::wasm_target::WasmTarget;
use elox::runner::EloxFileAndPromptRunner;
use elox::vm::target::WasmTarget;

fn main() {
    let mut wasm = WasmTarget::new();
    wasm.run_from_std_args();
}
