use std::default::Default;
use std::time::SystemTime;
use std::rc::Rc;
use crate::scanner::token::Position;
use crate::runner::{EloxError, EloxResult};
use crate::interpreter::eval_result::EvalError;

pub struct Host {
    pub print: Rc<(Fn(Position, String) -> EloxResult)>,               // msg
    pub error: Rc<(Fn(Position, String, usize, usize) -> EloxResult)>, // err_msg, line, col
    pub clock: Rc<(Fn(Position) -> Result<f64, EloxError>)>,
}

impl Default for Host {
    fn default() -> Host {
        Host {
            print: Rc::new(|_, msg| {
                println!("{}", msg);
                Ok(())
            }),
            clock: Rc::new(|pos| {
                if let Ok(now) = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                    Ok((now.as_millis() as f64) / 1000f64)
                } else {
                    Err(EloxError::Eval(EvalError::CouldNotGetTime(pos)))
                }
            }),
            error: Rc::new(|_, err, line, col| {
                eprintln!("Error: [line {}:{}]: {}", line, col, err);
                Ok(())
            }),
        }
    }
}
