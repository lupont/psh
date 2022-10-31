use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process;

use crate::builtin::Builtins;
use crate::input::Input;
use crate::{path, Result};

pub(crate) enum Command {
    Builtin(Input),
    Valid(Input),
    Invalid(Input),
}

pub(crate) struct ExitStatus {
    pub(crate) code: i32,
}

impl ExitStatus {
    pub(crate) fn from(code: i32) -> Self {
        Self { code }
    }
}

pub(crate) struct Engine<W: Write> {
    pub(crate) prev_dir: Option<PathBuf>,
    pub(crate) writer: W,
    pub(crate) commands: Vec<String>,
    pub(crate) builtins: Vec<String>,
}

impl<W: Write> Engine<W> {
    pub(crate) fn has_builtin(&self, builtin: impl AsRef<str>) -> bool {
        self.builtins.iter().any(|b| b == builtin.as_ref())
    }

    pub(crate) fn has_command(&self, cmd: impl AsRef<str>) -> bool {
        self.commands
            .iter()
            .any(|c| c.ends_with(&format!("/{}", cmd.as_ref())))
    }
}

impl Engine<Stdout> {
    pub(crate) fn new() -> Self {
        Self {
            prev_dir: None,
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            builtins: Self::builtin_names(),
        }
    }

    pub(crate) fn writer(&mut self) -> &mut Stdout {
        &mut self.writer
    }

    pub(crate) fn execute(&mut self, cmd: Command) -> Result<ExitStatus> {
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

// fn execute_builtin(&mut self, input: &Input) -> Result<i32> {
//     match input.cmd.as_str() {
//         "exit" => self.exit_builtin(),
//         "debug" => Ok(self.debug_builtin()),
//         "cd" => {
//             let dir = input.raw_args.get(0).map(|d| d.as_ref());
//             self.cd_builtin(dir)
//         }
//         "history" => self.history_builtin(&input.raw_args),

//         cmd => {
//             println!("{cmd} is not recognized as a builtin.");
//             Ok(1)
//         }
//     }
// }

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
