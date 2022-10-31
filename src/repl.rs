use crate::input;
use crate::Engine;
use crate::Result;

pub(crate) fn run() -> Result<()> {
    let mut engine = Engine::new();
    let mut last_status = None;

    loop {
        input::prompt(engine.writer(), last_status)?;

        let command = crate::input::rread_line(&mut engine)?;
        last_status = Some(engine.execute(command)?);
    }
}
