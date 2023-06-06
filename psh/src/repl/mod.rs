pub mod input;

use std::io::{Stdout, Write};
use std::process;

use crossterm::{execute, style, terminal};

use psh_core::engine::parser::{semtok, tok};
use psh_core::{parse, Engine, Error, ExitStatus, Result};

use crate::config::{self, Colors};
use crate::repl::input::read_line;

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

    pub fn run(&mut self, tokenize: bool, lex: bool, ast: bool) -> Result<()> {
        ctrlc::set_handler(|| {}).expect("psh: Error setting ctrl-c handler");

        loop {
            if let Err(e) = self.prompt(false) {
                writeln!(
                    self.engine.writer,
                    "psh: Error occurred when computing the prompt: {e}"
                )?;
            }

            let mut line = read_line(&mut self.engine, false)?;

            while parse(&line, false).is_err() {
                self.prompt(true)?;
                match read_line(&mut self.engine, true) {
                    Ok(l) => line += &l,
                    Err(Error::CancelledLine) => {
                        line = String::new();
                        break;
                    }
                    Err(e) => return Err(e),
                }
            }

            if tokenize && line != "exit" {
                for token in tok::tokenize(line) {
                    println!("{token:?}");
                }
            } else if lex && line != "exit" {
                for token in semtok::lex(line) {
                    println!("{token:?}");
                }
            } else if ast && line != "exit" {
                let ast = parse(line, true)?;
                println!("{ast:#?}");
            } else {
                self.engine.history.append(&line)?;
                match self.engine.execute_line(line) {
                    Ok(statuses) if statuses.is_empty() => {}

                    Ok(statuses) => {
                        self.last_status = Some(statuses);
                    }

                    Err(e) => {
                        writeln!(
                            self.engine.writer,
                            "psh: Error occurred when reading or executing: {e}"
                        )?;
                    }
                }
            }
        }
    }

    pub fn prompt(&mut self, ps2: bool) -> Result<()> {
        let _raw = RawMode::init()?;

        let prompt = format!(
            "{} ",
            if ps2 {
                config::PS2_PROMPT
            } else if is_root() {
                config::ROOT_PROMPT
            } else {
                config::USER_PROMPT
            }
        );

        Ok(execute!(
            self.engine.writer,
            style::SetForegroundColor(Colors::PROMPT),
            style::Print(prompt),
            style::ResetColor,
        )?)
    }
}

fn is_root() -> bool {
    let id = process::Command::new("id").arg("-u").output();
    matches!(id, Ok(id) if id.stdout == b"0\n")
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
