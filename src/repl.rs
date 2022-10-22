use std::env;
use std::ffi::OsStr;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

use crate::{Error, Result};

#[derive(Debug)]
pub(crate) struct Repl {
    available_cmds: Vec<String>,
    builtins: Vec<String>,
    prev_dir: Option<PathBuf>,
}

impl Repl {
    pub(crate) fn init() -> Self {
        Self {
            available_cmds: get_cmds_from_path(),
            builtins: vec!["debug", "cd", "exit"]
                .iter()
                .map(ToString::to_string)
                .collect(),
            prev_dir: None,
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

    fn exit_builtin(&self) -> ! {
        process::exit(0)
    }

    fn debug_builtin(&self) -> i32 {
        println!("{:#?}", self);
        0
    }

    fn cd_builtin(&mut self, dir: Option<&str>) -> i32 {
        let path = match dir {
            Some("-") if self.prev_dir.is_some() => self.prev_dir.take().unwrap(),

            Some("-") => {
                println!("cd: No previous directory.");
                return 1;
            }

            Some(dir) if PathBuf::from(dir).exists() => PathBuf::from(dir),

            Some(dir) => {
                println!("cd: '{}' does not exist.", dir);
                return 2;
            }

            None => {
                let home = home_dir().expect("could not read $HOME");
                PathBuf::from(home)
            }
        };

        self.prev_dir = Some(env::current_dir().unwrap());
        env::set_current_dir(path).unwrap();
        0
    }

    fn execute_builtin(&mut self, cmd: impl AsRef<str>, args: &[impl AsRef<str>]) -> i32 {
        match cmd.as_ref() {
            "exit" => self.exit_builtin(),
            "debug" => self.debug_builtin(),
            "cd" => {
                let dir = args.get(0);
                self.cd_builtin(dir.map(|d| d.as_ref()))
            }

            cmd => {
                println!("{cmd} is not recognized as a builtin.");
                1
            }
        }
    }

    fn prompt(&self, prev_rc: Option<i32>) -> Result<()> {
        print!(
            "{} ",
            env::current_dir()
                .unwrap()
                .display()
                .to_string()
                .replace(&home_dir()?, "~")
        );

        match prev_rc {
            Some(code) if code != 0 => {
                print!("\x1b[91m[{}]\x1b[0m ", code);
            }

            _ => {}
        }

        print!("\x1b[93m$\x1b[0m ");
        Ok(io::stdout().flush()?)
    }

    pub(crate) fn run(&mut self) -> Result<()> {
        let mut prev_rc: Option<i32> = None;

        loop {
            self.prompt(prev_rc)?;
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
    let raw_path = env::var("PATH").unwrap();
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
    env::var("HOME").map_err(|_| Error::NoHome)
}

/// Reads input from the user and saves it to the history file.
fn read_input_and_save_history() -> Result<(String, Vec<String>)> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    let buffer = buffer.trim().to_string();

    let home = home_dir()?;

    if !buffer.is_empty() {
        let history_file = match env::var("RUSH_HISTFILE") {
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
