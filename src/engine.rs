use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process;

use crate::builtin::Builtins;
use crate::input::Input;
use crate::{path, Result};

pub struct Engine<W: Write> {
    pub writer: W,
    pub prev_dir: Option<PathBuf>,
    pub commands: Vec<String>,
    pub builtins: Vec<&'static str>,
}

pub enum Command {
    Builtin(Input),
    Valid(Input),
    Invalid(Input),
}

pub struct ExitStatus {
    pub code: i32,
}

impl<W: Write> Engine<W> {
    pub fn has_builtin(&self, builtin: impl AsRef<str>) -> bool {
        self.builtins.iter().any(|&b| b == builtin.as_ref())
    }

    pub fn has_command(&self, cmd: impl AsRef<str>) -> bool {
        self.commands
            .iter()
            .any(|c| c.ends_with(&format!("/{}", cmd.as_ref())))
    }
}

impl Engine<Stdout> {
    pub fn new() -> Self {
        Self {
            prev_dir: None,
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            builtins: Self::builtin_names(),
        }
    }

    pub fn writer(&mut self) -> &mut Stdout {
        &mut self.writer
    }

    pub fn execute(&mut self, cmd: Command) -> Result<ExitStatus> {
        match cmd {
            Command::Builtin(input) => self.execute_builtin(&input),
            Command::Valid(input) => execute_command(input),
            Command::Invalid(input) => {
                println!("Unknown command: {}", input.cmd);
                Ok(ExitStatus { code: 1 })
            }
        }
    }
}

impl Default for Engine<Stdout> {
    fn default() -> Self {
        Self::new()
    }
}

impl ExitStatus {
    pub fn from(code: i32) -> Self {
        Self { code }
    }
}

fn execute_command(input: Input) -> Result<ExitStatus> {
    let child = process::Command::new(&input.cmd)
        .args(&input.raw_args)
        .spawn()?;
    let result = child.wait_with_output()?;

    // FIXME: append new line if child did not print one?
    // println!("stdout: '{}'", String::from_utf8_lossy(&result.stdout));

    // FIXME: `.code()` returns `None` if killed by signal
    let code = result.status.code().unwrap();
    Ok(ExitStatus { code })
}
