extern crate wasm_bindgen;

mod interpreter;
mod lox;
mod parser;
mod scanner;

use crate::interpreter::host::Host;
use lox::Lox;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(raw_module = "../www/index.js")]
extern "C" {
    pub fn log(s: String);
    pub fn clock() -> f64;
}

#[wasm_bindgen]
pub fn run(source: &str) {
    let host = Host {
        print: Box::new(|msg| {
            log(msg);
        }),
        clock: Box::new(|| Some(clock())),
    };

    let mut lox = Lox::new(host);
    lox.run(source);
}
