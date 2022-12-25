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
    last_status: Option<Vec<ExitStatus>>,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            engine: Engine::default(),
            last_status: None,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            if let Err(e) = self.prompt() {
                writeln!(
                    self.engine.writer,
                    "posh: Error occurred when computing the prompt: {e}"
                )?;
            }

            match self.engine.read_and_execute() {
                Ok(statuses) if statuses.is_empty() => {}

                Ok(statuses) => {
                    self.last_status = Some(statuses);
                }

                Err(e) => {
                    writeln!(
                        self.engine.writer,
                        "posh: Error occurred when reading or executing: {e}"
                    )?;
                }
            }
        }
    }

    pub fn prompt(&mut self) -> Result<()> {
        let _raw = RawMode::init()?;

        let cwd = format!("{} ", env::current_dir()?.display().to_string().expand());

        let exit_code = match &self.last_status {
            Some(codes) if !codes.iter().all(|c| c.code == 0) => {
                let codes = codes
                    .iter()
                    .map(|c| format!("{}", c.code))
                    .collect::<Vec<_>>()
                    .join("|");

                format!("[{codes}] ")
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
