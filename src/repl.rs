use std::env;
use std::ffi::OsStr;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

use crate::config::Colors;
use crate::path::{get_cmds_from_path, hist_file, home_dir};
use crate::Result;

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

    pub(crate) fn run(&mut self) -> Result<()> {
        let mut prev_rc: Option<i32> = None;

        loop {
            self.prompt(prev_rc)?;
            let (command, args) = read_input_and_save_history(
                self.available_cmds.as_slice(),
                self.builtins.as_slice(),
            )?;

            match command.as_str() {
                "" => {}

                cmd if self.has_builtin(cmd) => {
                    prev_rc = Some(self.execute_builtin(cmd, &args)?);
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

    fn has_cmd(&self, cmd: impl AsRef<str>) -> bool {
        let needle = "/".to_string() + cmd.as_ref();
        self.available_cmds.iter().any(|s| s.ends_with(&needle))
    }

    fn has_builtin(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        self.builtins.iter().any(|s| s == cmd)
    }

    fn exit_builtin(&self) -> ! {
        process::exit(0)
    }

    fn debug_builtin(&self) -> i32 {
        println!("{:#?}", self);
        0
    }

    fn cd_builtin(&mut self, dir: Option<&str>) -> Result<i32> {
        let path = match dir {
            Some("-") if self.prev_dir.is_some() => self.prev_dir.take().unwrap(),

            Some("-") => {
                println!("cd: No previous directory.");
                return Ok(1);
            }

            Some(dir) if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

            Some(dir) if PathBuf::from(dir).exists() => {
                println!("cd: '{}' is not a directory.", dir);
                return Ok(3);
            }

            Some(dir) => {
                println!("cd: '{}' does not exist.", dir);
                return Ok(2);
            }

            None => {
                let home = home_dir().expect("could not read $HOME");
                PathBuf::from(home)
            }
        };

        self.prev_dir = Some(env::current_dir()?);
        Ok(env::set_current_dir(path).map(|_| 0)?)
    }

    fn execute_builtin(&mut self, cmd: impl AsRef<str>, args: &[impl AsRef<str>]) -> Result<i32> {
        match cmd.as_ref() {
            "exit" => self.exit_builtin(),
            "debug" => Ok(self.debug_builtin()),
            "cd" => {
                let dir = args.get(0).map(|d| d.as_ref());
                self.cd_builtin(dir)
            }

            cmd => {
                println!("{cmd} is not recognized as a builtin.");
                Ok(1)
            }
        }
    }

    fn prompt(&self, prev_rc: impl Into<Option<i32>>) -> Result<()> {
        use crossterm::queue;
        use crossterm::style;

        let mut stdout = io::stdout();
        crossterm::terminal::enable_raw_mode()?;

        let cwd = format!(
            "{} ",
            env::current_dir()
                .unwrap()
                .display()
                .to_string()
                .replace(&home_dir()?, "~")
        );
        queue!(
            stdout,
            style::SetForegroundColor(Colors::CWD),
            style::Print(cwd),
            style::SetForegroundColor(style::Color::Reset)
        )?;

        match prev_rc.into() {
            Some(code) if code != 0 => {
                let exit_code = format!("[{code}] ");
                queue!(
                    stdout,
                    style::SetForegroundColor(Colors::NON_ZERO_RC),
                    style::Print(exit_code),
                    style::SetForegroundColor(style::Color::Reset),
                )?;
            }

            _ => {}
        }

        queue!(
            stdout,
            style::SetForegroundColor(Colors::PROMPT),
            style::Print("$ "),
            style::SetForegroundColor(style::Color::Reset)
        )?;

        crossterm::terminal::disable_raw_mode()?;
        Ok(stdout.flush()?)
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

/// Reads input from the user and saves it to the history file.
fn read_input_and_save_history(
    cmds: &[String],
    builtins: &[String],
) -> Result<(String, Vec<String>)> {
    let buffer = crate::input::read_line(&mut io::stdout(), cmds, builtins)?
        .trim()
        .to_string();

    let home = home_dir()?;

    if !buffer.is_empty() {
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(hist_file()?)?;
        file.write_all(buffer.as_bytes())?;
        file.write_all(b"\n")?;
    }

    match buffer.find(' ') {
        Some(_) => {
            let (cmd, args) = buffer.split_once(' ').unwrap();
            let args = args
                .split_ascii_whitespace()
                .map(|s| s.replace('~', &home))
                .collect::<Vec<_>>();
            Ok((cmd.to_string(), args))
        }
        _ => Ok((buffer, Default::default())),
    }
}
