extern crate wasm_bindgen;

pub mod interpreter;
pub mod parser;
pub mod runner;
pub mod scanner;
pub mod vm;

use crate::interpreter::host::Host;
use runner::{interp::EloxInterpreter, EloxRunner};
use crate::runner::EloxResult;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(raw_module = "../web/index.js")]
extern "C" {
    pub fn log(s: String);
    pub fn error(err: String, line: usize, col: usize);
    pub fn clock() -> f64;
}

#[wasm_bindgen]
pub fn run(source: &str) -> EloxResult {
    let host = Host {
        print: Rc::new(|_, msg| {
            log(msg);
            Ok(())
        }),
        error: Rc::new(|_, err, line, col| {
            error(err, line, col);
            Ok(())
        }),
        clock: Rc::new(|_| Ok(clock())),
    };

    let mut elox = EloxInterpreter::new(host);
    if let Err(err) = elox.run(source) {
        elox.throw_error(err)?;
    }
    Ok(())
}
