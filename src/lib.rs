extern crate wasm_bindgen;

pub mod interpreter;
pub mod parser;
pub mod runner;
pub mod scanner;
pub mod vm;

use crate::interpreter::host::Host;
use runner::{interp::EloxInterpreter, EloxRunner};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(raw_module = "../web/index.js")]
extern "C" {
    pub fn log(s: String);
    pub fn error(err: String, line: usize, col: usize);
    pub fn clock() -> f64;
}

#[wasm_bindgen]
pub fn run(source: &str) {
    let host = Host {
        print: Box::new(|msg| {
            log(msg);
        }),
        error: Box::new(|err, line, col| {
            error(err, line, col);
        }),
        clock: Box::new(|| Some(clock())),
    };

    let mut elox = EloxInterpreter::new(host);
    if let Err(err) = elox.run(source) {
        elox.throw_error(err);
    }
}
