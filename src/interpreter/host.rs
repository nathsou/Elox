use std::default::Default;
use std::time::SystemTime;

pub struct Host {
    pub print: Box<Fn(String)>,               // msg
    pub error: Box<Fn(String, usize, usize)>, // err_msg, line, col
    pub clock: Box<Fn() -> Option<f64>>,
}

impl Default for Host {
    fn default() -> Host {
        Host {
            print: Box::new(|msg| {
                println!("{}", msg);
            }),
            clock: Box::new(|| {
                if let Ok(now) = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                    return Some((now.as_millis() as f64) / 1000f64);
                }

                None
            }),
            error: Box::new(|err, line, col| {
                eprintln!("Error: [line {}:{}]: {}", line, col, err);
            }),
        }
    }
}
