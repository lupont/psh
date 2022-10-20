use std::ffi::OsStr;
use std::io::{self, Write};
use std::process;

use crate::Result;

pub(crate) struct Repl {
    available_cmds: Vec<String>,
}

impl Repl {
    pub(crate) fn init() -> Self {
        Self {
            available_cmds: get_cmds_from_path(),
        }
    }

    pub(crate) fn run(&self) -> Result<()> {
        let mut prev_rc: Option<i32> = None;

        loop {
            if let Some(exit_code) = prev_rc {
                if exit_code != 0 {
                    print!("\x1b[91m[{}]\x1b[0m ", exit_code);
                }
            }

            print!("\x1b[93m$\x1b[0m ");
            io::stdout().flush()?;

            let (command, args) = read_input()?;
            match command.as_str() {
                "" => {}

                "exit" => process::exit(0),

                cmd if self.available_cmds.iter().any(|s| s.ends_with(cmd)) => {
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

fn read_input() -> Result<(String, Vec<String>)> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    let buffer = buffer.trim().to_string();

    match buffer.find(' ') {
        Some(_) => {
            let (cmd, args) = buffer.split_once(' ').unwrap();
            let args = args
                .split_ascii_whitespace()
                .map(ToString::to_string)
                .collect::<Vec<_>>();
            Ok((cmd.to_string(), args))
        }
        _ => Ok((buffer, Default::default())),
    }
}
