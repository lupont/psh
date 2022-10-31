use crate::input::{input, prompt};
use crate::Engine;
use crate::Result;

pub fn run() -> Result<()> {
    let mut engine = Engine::new();
    let mut last_status = None;

    loop {
        prompt(engine.writer(), &last_status)?;

        if let Some(command) = input(&mut engine)? {
            last_status = Some(engine.execute(command)?);
        }
    }
}
