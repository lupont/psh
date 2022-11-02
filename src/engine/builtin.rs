use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process;

use crate::engine::Line;
use crate::path;
use crate::{Engine, ExitStatus, Result};

pub trait Builtins: CdBuiltin + ExitBuiltin + HistoryBuiltin {
    fn execute_builtin(&mut self, input: &Line) -> Result<ExitStatus>;
    fn builtin_names() -> Vec<&'static str> {
        vec!["cd", "exit", "history"]
    }
}

pub trait CdBuiltin {
    fn cd(&mut self, dir: Option<&str>) -> Result<ExitStatus>;
}

pub trait ExitBuiltin {
    fn exit(&self, code: i32) -> !;
}

pub trait HistoryBuiltin {
    fn history_show(&mut self) -> Result<ExitStatus>;
    fn history_path(&mut self) -> Result<ExitStatus>;
    fn history_clear(&mut self) -> Result<ExitStatus>;
}

impl<W: Write> Builtins for Engine<W> {
    fn execute_builtin(&mut self, input: &Line) -> Result<ExitStatus> {
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

impl<W: Write> ExitBuiltin for Engine<W> {
    fn exit(&self, code: i32) -> ! {
        process::exit(code)
    }
}

impl<W: Write> CdBuiltin for Engine<W> {
    fn cd(&mut self, dir: Option<&str>) -> Result<ExitStatus> {
        let path = match dir {
            Some("-") if self.prev_dir.is_some() => self.prev_dir.take().unwrap(),

            Some("-") => {
                writeln!(self.writer, "cd: No previous directory.")?;
                return Ok(ExitStatus::from(1));
            }

            Some(dir) if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

            Some(dir) if PathBuf::from(dir).exists() => {
                writeln!(self.writer, "cd: '{}' is not a directory.", dir)?;
                return Ok(ExitStatus::from(3));
            }

            Some(dir) => {
                writeln!(self.writer, "cd: '{}' does not exist.", dir)?;
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

impl<W: Write> HistoryBuiltin for Engine<W> {
    fn history_show(&mut self) -> Result<ExitStatus> {
        let lines = self.history.read_lines()?;
        for line in lines {
            writeln!(self.writer, "{line}")?;
        }
        Ok(ExitStatus::from(0))
    }

    fn history_path(&mut self) -> Result<ExitStatus> {
        writeln!(self.writer, "{}", self.history.path.display())?;
        Ok(ExitStatus::from(0))
    }

    fn history_clear(&mut self) -> Result<ExitStatus> {
        Ok(ExitStatus::from(match self.history.clear() {
            Ok(()) => 0,
            Err(_) => 1,
        }))
    }
}
