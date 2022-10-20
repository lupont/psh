use std::io::{self, Write};
use std::process::Command;

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
        let mut prev_rc: Option<std::process::ExitStatus> = None;

        loop {
            if let Some(exit_code) = prev_rc {
                if !exit_code.success() {
                    // FIXME: `.code()` returns `None` if killed by signal
                    print!("[{}] ", exit_code.code().unwrap());
                }
            }
            print!("$ ");
            io::stdout().flush()?;

            let (cmd, args) = read_input()?;

            if self.available_cmds.contains(&cmd) {
                let mut child = Command::new(cmd).args(args).spawn().unwrap();
                let code = child.wait().unwrap();
                prev_rc = Some(code);
            } else {
                if cmd == "exit" {
                    std::process::exit(0);
                }
                println!("{cmd} {}", args.join(" "));
            }

            io::stdout().flush()?;
        }
    }
}

fn get_cmds_from_path() -> Vec<String> {
    let raw_path = std::env::var("PATH").unwrap();
    let raw_path = raw_path.split(':');

    let mut cmds = Vec::new();

    for path in raw_path {
        let output = Command::new("ls").arg(path).output().unwrap();

        let s = String::from_utf8(output.stdout).unwrap();
        let s = s.split_ascii_whitespace().map(|s| s.to_string()).collect::<Vec<_>>();
        cmds.extend(s);
    }

    cmds
}

fn read_input() -> Result<(String, Vec<String>)> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    let buffer = buffer.trim().to_string();

    // println!("Buffer: '{buffer}'");

    if buffer.find(' ').is_some() {
        let (cmd, args) = buffer.split_once(' ').unwrap();
        let args = args.split_ascii_whitespace().map(ToString::to_string).collect::<Vec<_>>();
        return Ok((cmd.to_string(), args));
    }

    Ok((buffer.to_string(), Vec::new()))
}
