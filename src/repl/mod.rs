pub mod input;

use std::env;
use std::io::Stdout;

use crossterm::{queue, style, terminal};

use crate::config::Colors;
use crate::path;
use crate::{Engine, ExitStatus, Result};

pub struct Repl {
    engine: Engine<Stdout>,
    last_status: Option<ExitStatus>,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            engine: Engine::new(),
            last_status: None,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.prompt()?;

            if let Some(command) = input::input(&mut self.engine)? {
                self.last_status = Some(self.engine.execute(command)?);
            }
        }
    }

    pub fn prompt(&mut self) -> Result<()> {
        let _raw = RawMode::init()?;

        let cwd = format!(
            "{} ",
            env::current_dir()?
                .display()
                .to_string()
                .replacen(&path::home_dir()?, "~", 1)
        );

        queue!(
            self.engine.writer,
            style::SetForegroundColor(Colors::CWD),
            style::Print(cwd),
        )?;

        match self.last_status {
            Some(ExitStatus { code }) if code != 0 => {
                let exit_code = format!("[{code}] ");
                queue!(
                    self.engine.writer,
                    style::SetForegroundColor(Colors::NON_ZERO_RC),
                    style::Print(exit_code),
                )?;
            }

            _ => {}
        }

        queue!(
            self.engine.writer,
            style::SetForegroundColor(Colors::PROMPT),
            style::Print("$ "),
            style::SetForegroundColor(style::Color::Reset)
        )?;

        Ok(())
    }
}

pub struct RawMode;

impl RawMode {
    pub fn init() -> Result<Self> {
        terminal::enable_raw_mode()?;
        Ok(Self)
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        terminal::disable_raw_mode().expect("could not disable raw mode");
    }
}
