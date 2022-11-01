use std::io::Stdout;

use crate::input::{input, prompt};
use crate::Engine;
use crate::Result;

pub struct Repl {
    engine: Engine<Stdout>,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            engine: Engine::new(),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut last_status = None;

        loop {
            self.engine
                .execute(crate::engine::Command::Valid(crate::input::Input {
                    cmd: "ls".into(),
                    raw_args: vec!["-lah".into()],
                }))?;
            prompt(self.engine.writer(), &last_status)?;

            if let Some(command) = input(&mut self.engine)? {
                last_status = Some(self.engine.execute(command)?);
            }
        }
    }
}
