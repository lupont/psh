use std::env;
use std::fs;
use std::io::{Stdout, Write};
use std::path::PathBuf;
use std::process;

use crate::engine::{Engine, ExitStatus};
use crate::input::Input;
use crate::{path, Result};

pub(crate) trait Builtins: CdBuiltin + ExitBuiltin + HistoryBuiltin {
    fn execute_builtin(&mut self, input: &Input) -> Result<ExitStatus>;
    fn builtin_names() -> Vec<String> {
        vec!["cd".to_string(), "exit".to_string(), "history".to_string()]
    }
}

impl Builtins for Engine<Stdout> {
    fn execute_builtin(&mut self, input: &Input) -> Result<ExitStatus> {
        match (input.cmd.as_str(), &input.raw_args()) {
            ("exit", _) => self.exit(0),

            ("cd", [dir]) => self.cd(Some(dir)),
            ("cd", []) => self.cd(None),
            ("cd", _) => {
                writeln!(self.writer, "invalid number of arguments")?;
                Ok(ExitStatus::from(1))
            }

            ("history", [c]) if c == "clear" => self.history_clear(),
            ("history", [c]) if c == "path" => self.history_path(),
            ("history", [c]) if c == "show" => self.history_show(),
            ("history", []) => self.history_show(),
            ("history", [c]) => {
                writeln!(self.writer, "invalid argument for history: {}", c)?;
                Ok(ExitStatus::from(1))
            }
            ("history", _) => {
                writeln!(self.writer, "invalid number of arguments")?;
                Ok(ExitStatus::from(2))
            }

            _ => todo!(),
        }
    }
}

pub(crate) trait CdBuiltin {
    fn cd(&mut self, dir: Option<&str>) -> Result<ExitStatus>;
}

pub(crate) trait ExitBuiltin {
    fn exit(&self, code: i32) -> !;
}

pub(crate) trait HistoryBuiltin {
    fn history_show(&self) -> Result<ExitStatus>;
    fn history_path(&self) -> Result<ExitStatus>;
    fn history_clear(&self) -> Result<ExitStatus>;
}

impl ExitBuiltin for Engine<Stdout> {
    fn exit(&self, code: i32) -> ! {
        process::exit(code)
    }
}

impl CdBuiltin for Engine<Stdout> {
    fn cd(&mut self, dir: Option<&str>) -> Result<ExitStatus> {
        let path = match dir {
            Some("-") if self.prev_dir.is_some() => self.prev_dir.take().unwrap(),

            Some("-") => {
                println!("cd: No previous directory.");
                return Ok(ExitStatus::from(1));
            }

            Some(dir) if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

            Some(dir) if PathBuf::from(dir).exists() => {
                println!("cd: '{}' is not a directory.", dir);
                return Ok(ExitStatus::from(3));
            }

            Some(dir) => {
                println!("cd: '{}' does not exist.", dir);
                return Ok(ExitStatus::from(2));
            }

            None => {
                let home = path::home_dir().expect("could not read $HOME");
                PathBuf::from(home)
            }
        };

        self.prev_dir = Some(env::current_dir()?);
        Ok(env::set_current_dir(path).map(|_| ExitStatus::from(0))?)
    }
}

impl HistoryBuiltin for Engine<Stdout> {
    fn history_show(&self) -> Result<ExitStatus> {
        let history = path::hist_file()?;
        for line in fs::read_to_string(history)?.trim().split('\n') {
            println!("{line}");
        }
        Ok(ExitStatus::from(0))
    }

    fn history_path(&self) -> Result<ExitStatus> {
        let history = path::hist_file()?;
        println!("{}", history.display());
        Ok(ExitStatus::from(0))
    }

    fn history_clear(&self) -> Result<ExitStatus> {
        let history = path::hist_file()?;
        fs::OpenOptions::new()
            .write(true)
            .open(history)?
            .set_len(0)?;
        Ok(ExitStatus::from(0))
    }
}
