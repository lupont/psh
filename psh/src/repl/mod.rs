pub mod input;

use std::env;
use std::io::{Stdout, Write};
use std::process;

use crossterm::{execute, style, terminal};

use psh_core::engine::parser::{semtok, tok};
use psh_core::path::compress_tilde;
use psh_core::{parse, Engine, ExitStatus, Result};

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
        let (tx, rx) = std::sync::mpsc::channel();
        ctrlc::set_handler(move || {
            tx.send(true).unwrap();
        })
        .expect("Error setting Ctrl-C handler");

        loop {
            if let Err(e) = self.prompt(rx.try_recv().unwrap_or_default()) {
                writeln!(
                    self.engine.writer,
                    "psh: Error occurred when computing the prompt: {e}"
                )?;
            }

            let line = read_line(&mut self.engine)?;

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

    pub fn prompt(&mut self, ctrlc: bool) -> Result<()> {
        let _raw = RawMode::init()?;

        let cwd = format!(
            "{} ",
            compress_tilde(env::current_dir()?.display().to_string())
        );

        let exit_code = if ctrlc {
            "[SIGINT] ".to_string()
        } else {
            match &self.last_status {
                Some(codes) if !codes.iter().all(ExitStatus::is_ok) => {
                    let codes = codes
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join("|");

                    format!("[{codes}] ")
                }

                _ => "".to_string(),
            }
        };

        let prompt = format!(
            "{} ",
            if is_root() {
                config::ROOT_PROMPT
            } else {
                config::USER_PROMPT
            }
        );

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
