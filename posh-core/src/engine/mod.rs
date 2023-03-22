pub mod history;
pub mod parser;

use std::io::{self, Read, Stdout, Write};
use std::path::PathBuf;
use std::process::{self, Stdio};

use crate::{path, Result};

pub use self::history::{FileHistory, History};
use self::parser::ast::{parse, AndOrList, Command, CompleteCommand, Pipeline, SyntaxTree};

pub struct Engine<W: Write> {
    pub writer: W,
    pub prev_dir: Option<PathBuf>,
    pub commands: Vec<String>,
    pub history: Box<dyn History>,
}

impl<W: Write> Engine<W> {
    fn _cd(&mut self, dir: Option<&str>) -> Result<ExitStatus> {
        let path = match dir {
            Some("-") if self.prev_dir.is_some() => self.prev_dir.take().unwrap(),

            Some("-") => {
                writeln!(self.writer, "cd: No previous directory.")?;
                return Ok(ExitStatus::from_code(1));
            }

            Some(dir) if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

            Some(dir) if PathBuf::from(dir).exists() => {
                writeln!(self.writer, "cd: '{}' is not a directory.", dir)?;
                return Ok(ExitStatus::from_code(3));
            }

            Some(dir) => {
                writeln!(self.writer, "cd: '{}' does not exist.", dir)?;
                return Ok(ExitStatus::from_code(2));
            }

            None => PathBuf::from(path::home_dir()),
        };

        self.prev_dir = Some(std::env::current_dir()?);
        std::env::set_current_dir(path)?;
        Ok(ExitStatus::from_code(0))
    }

    fn _exit(&self, code: i32) -> ! {
        std::process::exit(code)
    }

    pub fn has_builtin(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        let has = |s| cmd == s || cmd.starts_with(&format!("{s} "));
        has("cd") || has("exit")
    }

    pub fn execute_builtin(&mut self, _cmd: CompleteCommand) -> Result<ExitStatus> {
        todo!()
        // let command = cmd.cmd_name();
        // let args = cmd.args();

        // match (command.as_str(), &args[..]) {
        //     ("exit", []) => self.exit(0),
        //     ("exit", [code]) => {
        //         if let Ok(s) = code.parse::<i32>() {
        //             self.exit(s)
        //         } else {
        //             writeln!(self.writer, "invalid integer: '{}'", code)?;
        //             Ok(ExitStatus::from(1))
        //         }
        //     }

        //     ("cd", [dir]) => self.cd(Some(dir)),
        //     ("cd", []) => self.cd(None),
        //     ("cd", _) => {
        //         writeln!(self.writer, "invalid number of arguments")?;
        //         Ok(ExitStatus::from(1))
        //     }

        //     (c, _) => Err(Error::UnknownCommand(c.to_string())),
        // }
    }

    pub fn has_command(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        path::has_relative_command(cmd)
            || self
                .commands
                .iter()
                .any(|c| c == cmd || c.ends_with(&format!("/{}", cmd)))
    }

    pub fn execute_line(&mut self, line: impl ToString) -> Result<Vec<ExitStatus>> {
        let ast = parse(line.to_string(), false)?;
        self.walk_ast(ast)
    }

    pub fn execute_pipeline(&mut self, pipeline: &Pipeline) -> Result<Vec<ExitStatus>> {
        let pipeline = pipeline.pipeline();
        let mut pipeline = pipeline.iter().peekable();
        let mut codes: Vec<ExitStatus> = Vec::new();
        let mut last_stdout: Option<process::ChildStdout> = None;

        while let Some(Command::Simple(cmd)) = pipeline.next() {
            if let Some(name) = cmd.name() {
                let mut command = process::Command::new(name);

                let stdin = match last_stdout {
                    Some(output) => Stdio::from(output),
                    None => Stdio::null(),
                };

                let stdout = if pipeline.peek().is_none() {
                    Stdio::inherit()
                } else {
                    Stdio::piped()
                };

                let mut child = command
                    .stdin(stdin)
                    .stdout(stdout)
                    .args(cmd.args())
                    .spawn()?;

                last_stdout = child.stdout.take();
                let output = child.wait_with_output()?;
                codes.push(ExitStatus::from(output.status));
            }
        }

        if let Some(mut last_stdout) = last_stdout {
            let mut buf = String::new();
            last_stdout.read_to_string(&mut buf)?;
            self.writer.write_all(buf.as_bytes())?;
        }

        Ok(codes)
    }

    pub fn execute_logical_expr(&mut self, logical_expr: &AndOrList) -> Result<Vec<ExitStatus>> {
        self.execute_pipeline(&logical_expr.first)
    }

    pub fn execute(&mut self, cmd: &CompleteCommand) -> Result<Vec<ExitStatus>> {
        let list = &cmd.list;
        let exit_status = self.execute_logical_expr(&list.first)?;
        Ok(exit_status)
    }

    fn walk_ast(&mut self, ast: SyntaxTree) -> Result<Vec<ExitStatus>> {
        ast.program
            .into_iter()
            .fold(Ok(vec![]), |_, c| self.execute(&c))
    }
}

impl Engine<Stdout> {
    pub fn new() -> Self {
        let history = FileHistory::init().expect("could not initialize history");
        Self {
            prev_dir: None,
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            history: Box::new(history),
        }
    }
}

impl Default for Engine<Stdout> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct ExitStatus {
    pub code: i32,
}

impl ExitStatus {
    pub fn from_code(code: i32) -> Self {
        Self { code }
    }
}

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(status: std::process::ExitStatus) -> Self {
        Self {
            // FIXME: handle None case
            code: status.code().unwrap(),
        }
    }
}
