pub mod input;

use std::env;
use std::io::{Stdout, Write};
use std::process;

use crossterm::{execute, style, terminal};

use psh_core::engine::parser::{semtok, tok};
use psh_core::{parse, path, Engine, Error, ExitStatus, Result};

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

    fn read_init_file(&mut self) -> Result<()> {
        match self.engine.execute_file(path::init_file()) {
            Ok(_) => Ok(()),
            Err(Error::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        }
    }

    pub fn run(&mut self, tokenize: bool, lex: bool, ast: bool) -> Result<()> {
        self.read_init_file()?;

        if env::var("PS1").is_err() {
            env::set_var(
                "PS1",
                match is_root() {
                    true => config::PS1_ROOT_PROMPT,
                    false => config::PS1_USER_PROMPT,
                },
            );
        }
        if env::var("PS2").is_err() {
            env::set_var("PS2", config::PS2_PROMPT);
        }

        ctrlc::set_handler(|| {}).expect("psh: Error setting ^C handler");

        loop {
            self.prompt(false)?;

            let start_pos = crossterm::cursor::position()?;
            let mut line = read_line(&mut self.engine, true, start_pos, None)?;

            while let Err(Error::Incomplete(_)) = parse(&line, false) {
                line.push('\n');

                self.prompt(true)?;
                match read_line(&mut self.engine, false, start_pos, Some(&line)) {
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
                        writeln!(self.engine.writer, "psh: {e}")?;
                    }
                }
            }
        }
    }

    pub fn prompt(&mut self, ps2: bool) -> Result<()> {
        let _raw = RawMode::init()?;

        let prompt = if ps2 {
            env::var("PS2").unwrap()
        } else {
            env::var("PS1").unwrap()
        };

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
