use std::time::SystemTime;
use std::default::Default;

pub struct Host {
    pub print: Box<Fn(String)>,
    pub error: Box<Fn(String)>,
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
            error: Box::new(|err| {
                println!("Error: {}", err);
            }),
        }
    }
}
