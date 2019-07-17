extern crate wasm_bindgen;

mod elox;
mod interpreter;
mod parser;
mod scanner;

use crate::interpreter::host::Host;
use elox::Elox;
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

    let mut lox = Elox::new(host);
    lox.run(source);
}
