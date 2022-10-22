use std::ffi::OsStr;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

use crate::{Error, Result};

#[derive(Debug)]
pub(crate) struct Repl {
    available_cmds: Vec<String>,
    builtins: Vec<String>,
}

impl Repl {
    pub(crate) fn init() -> Self {
        Self {
            available_cmds: get_cmds_from_path(),
            builtins: vec!["debug".to_string()],
        }
    }

    fn has_cmd(&self, cmd: impl AsRef<str>) -> bool {
        self.available_cmds
            .iter()
            .any(|s| s.ends_with(&("/".to_string() + cmd.as_ref())))
    }

    fn has_builtin(&self, cmd: impl AsRef<str>) -> bool {
        self.builtins.iter().any(|s| s == cmd.as_ref())
    }

    fn execute_builtin(&self, cmd: impl AsRef<str>) -> i32 {
        match cmd.as_ref() {
            "debug" => {
                println!("{:#?}", self);
                0
            }

            cmd => {
                println!("{cmd} is not recognized as a builtin.");
                1
            }
        }
    }

    pub(crate) fn run(&self) -> Result<()> {
        let mut prev_rc: Option<i32> = None;

        loop {
            match prev_rc {
                Some(code) if code != 0 => {
                    print!("\x1b[91m[{}]\x1b[0m ", code);
                }

                _ => {}
            }

            print!("\x1b[93m$\x1b[0m ");
            io::stdout().flush()?;

            let (command, args) = read_input_and_save_history()?;
            match command.as_str() {
                "" => {}

                "exit" => process::exit(0),

                cmd if self.has_builtin(cmd) => {
                    prev_rc = Some(self.execute_builtin(cmd));
                }

                cmd if self.has_cmd(cmd) => {
                    prev_rc = Some(run_cmd(cmd, &args)?);
                }

                cmd => {
                    println!("Unknown command: {cmd}");
                    prev_rc = Some(1);
                }
            }

            io::stdout().flush()?;
        }
    }
}

fn run_cmd(cmd: impl AsRef<OsStr>, args: &[impl AsRef<OsStr>]) -> Result<i32> {
    let mut child = process::Command::new(cmd).args(args).spawn().unwrap();
    let result = child.wait()?;

    // FIXME: `.code()` returns `None` if killed by signal
    Ok(result.code().unwrap())
}

fn get_cmds_from_path() -> Vec<String> {
    let raw_path = std::env::var("PATH").unwrap();
    let raw_path = raw_path.split(':');

    let mut cmds = Vec::new();

    for path in raw_path {
        match std::fs::read_dir(path) {
            Ok(dirs) => cmds.extend(dirs.map(|d| format!("{}", d.unwrap().path().display()))),
            _ => {}
        }
    }

    cmds
}

/// Reads input from the user and saves it to the history file.
fn read_input_and_save_history() -> Result<(String, Vec<String>)> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    let buffer = buffer.trim().to_string();

    let home = match std::env::var("HOME") {
        Ok(home) => home,
        Err(_) => return Err(Error::NoHome),
    };

    if !buffer.is_empty() {
        let history_file = match std::env::var("RUSH_HISTFILE") {
            Ok(path) => PathBuf::from(path),
            Err(_) => PathBuf::from(&home).join(".rush_history"),
        };

        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(history_file)?;
        file.write_all(buffer.as_bytes())?;
        file.write_all(b"\n")?;
    }

    match buffer.find(' ') {
        Some(_) => {
            let (cmd, args) = buffer.split_once(' ').unwrap();
            let args = args
                .split_ascii_whitespace()
                .map(ToString::to_string)
                .map(|s| s.replace("~", &home))
                .collect::<Vec<_>>();
            Ok((cmd.to_string(), args))
        }
        _ => Ok((buffer, Default::default())),
    }
}
