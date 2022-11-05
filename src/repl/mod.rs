pub mod input;

use std::env;
use std::io::{Stdout, Write};
use std::process;

use crossterm::{execute, style, terminal};

use crate::config::{self, Colors};
use crate::path::Expand;

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
            if let Err(e) = self.prompt() {
                writeln!(
                    self.engine.writer,
                    "rush: Error occurred when computing the prompt: {e}"
                )?;
            }

            match input::input(&mut self.engine) {
                Ok(Some(command)) => match self.engine.execute(command) {
                    Ok(status) => {
                        self.last_status = Some(status);
                    }

                    Err(e) => {
                        writeln!(
                            self.engine.writer,
                            "rush: Error occurred when executing command: {e}"
                        )?;
                    }
                },

                Ok(None) => {}

                Err(e) => {
                    writeln!(
                        self.engine.writer,
                        "rush: Error occurred when reading input: {e}"
                    )?;
                }
            };
        }
    }

    pub fn prompt(&mut self) -> Result<()> {
        let _raw = RawMode::init()?;

        let cwd = format!("{} ", env::current_dir()?.display().to_string().expand()?);

        let exit_code = match self.last_status {
            Some(ExitStatus { code }) if code != 0 => {
                format!("[{code}] ")
            }

            _ => "".to_string(),
        };

        let prompt = format!("{} ", if is_root() { "#" } else { config::PROMPT });

        Ok(execute!(
            self.engine.writer,
            style::SetForegroundColor(Colors::CWD),
            style::Print(cwd),
            style::SetForegroundColor(Colors::NON_ZERO_RC),
            style::Print(exit_code),
            style::SetForegroundColor(Colors::PROMPT),
            style::Print(prompt),
            style::ResetColor,
        )?)
    }
}

fn is_root() -> bool {
    matches!(process::Command::new("whoami").output(), Ok(output) if output.stdout == b"root\n")
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
