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
            builtins: vec!["debug", "cd", "exit"]
                .iter()
                .map(ToString::to_string)
                .collect(),
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

    fn execute_builtin(&self, cmd: impl AsRef<str>, args: &[impl AsRef<str>]) -> i32 {
        match cmd.as_ref() {
            "exit" => process::exit(0),

            "debug" => {
                println!("{:#?}", self);
                0
            }

            "cd" => {
                let path = if args.is_empty() {
                    PathBuf::from(home_dir().unwrap())
                } else {
                    let path = PathBuf::from(args[0].as_ref());
                    if path.exists() {
                        path
                    } else {
                        println!("cd: The path '{}' does not exist", args[0].as_ref());
                        return 1;
                    }
                };
                std::env::set_current_dir(path).unwrap();
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
            print!("{} ", std::env::current_dir().unwrap().display().to_string().replace(&home_dir()?, "~"));
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

                cmd if self.has_builtin(cmd) => {
                    prev_rc = Some(self.execute_builtin(cmd, &args));
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
    let child = process::Command::new(cmd).args(args).spawn().unwrap();
    let result = child.wait_with_output()?;

    // FIXME: append new line if child did not print one?
    // println!("stdout: '{}'", String::from_utf8_lossy(&result.stdout));

    // FIXME: `.code()` returns `None` if killed by signal
    Ok(result.status.code().unwrap())
}

fn get_cmds_from_path() -> Vec<String> {
    let raw_path = std::env::var("PATH").unwrap();
    let raw_path = raw_path.split(':');

    let mut cmds = Vec::new();

    for path in raw_path {
        if let Ok(dirs) = std::fs::read_dir(path) {
            cmds.extend(dirs.map(|d| format!("{}", d.unwrap().path().display())));
        }
    }

    cmds
}

fn home_dir() -> Result<String> {
    std::env::var("HOME").map_err(|_| Error::NoHome)
}

/// Reads input from the user and saves it to the history file.
fn read_input_and_save_history() -> Result<(String, Vec<String>)> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    let buffer = buffer.trim().to_string();

    let home = home_dir()?;

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
                .map(|s| s.replace('~', &home))
                .collect::<Vec<_>>();
            Ok((cmd.to_string(), args))
        }
        _ => Ok((buffer, Default::default())),
    }
}
