pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process;

use crate::{path, Result};

pub use self::history::{FileHistory, History};
use self::parser::ast::{
    parse, AndOrList, Command, CompleteCommand, Pipeline, SimpleCommand, SyntaxTree,
};

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

            None => PathBuf::from(path::home_dir()),
        };

        self.prev_dir = Some(std::env::current_dir()?);
        std::env::set_current_dir(path)?;
        Ok(ExitStatus::from(0))
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

    // fn build_command(
    //     &self,
    //     command: &CompleteCommand,
    //     prev_stdout: Option<Option<ChildStdout>>,
    //     final_cmd: bool,
    // ) -> Result<process::Command> {
    //     let (stdin_redirect, stdout_redirect, stderr_redirect) = command.redirections();

    //     let mut stdin = match prev_stdout {
    //         Some(Some(stdout)) => Stdio::from(stdout),
    //         Some(None) => Stdio::null(),
    //         None => Stdio::inherit(),
    //     };

    //     if let Some(Redirect::Input { to }) = stdin_redirect {
    //         let f = std::fs::OpenOptions::new().read(true).open(to.name)?;
    //         stdin = Stdio::from(f);
    //     }

    //     let stdout = if let Some(Redirect::Output {
    //         from: _,
    //         to,
    //         append,
    //     }) = stdout_redirect
    //     {
    //         let f = std::fs::OpenOptions::new()
    //             .write(true)
    //             .create(true)
    //             .append(append)
    //             .open(to.name)?;
    //         Stdio::from(f)
    //     } else if final_cmd {
    //         Stdio::inherit()
    //     } else {
    //         Stdio::piped()
    //     };

    //     let stderr = if let Some(Redirect::Output {
    //         from: _,
    //         to,
    //         append,
    //     }) = stderr_redirect
    //     {
    //         let f = std::fs::OpenOptions::new()
    //             .write(true)
    //             .create(true)
    //             .append(append)
    //             .open(to.name)?;
    //         Stdio::from(f)
    //     } else {
    //         Stdio::inherit()
    //     };

    //     let mut cmd = process::Command::new(command.cmd_name());
    //     let cmd = cmd
    //         .args(command.args())
    //         .envs(command.vars())
    //         .stdin(stdin)
    //         .stdout(stdout)
    //         .stderr(stderr);

    //     let dummy = process::Command::new("tmp");
    //     Ok(std::mem::replace(cmd, dummy))
    // }

    pub fn execute_simple_command(&mut self, cmd: &SimpleCommand) -> Result<ExitStatus> {
        if let Some(name) = cmd.name() {
            let mut command = process::Command::new(name);
            let mut child = command.args(cmd.args()).spawn()?;
            let exit_status = child.wait()?;
            Ok(ExitStatus::from(exit_status.code().unwrap()))
        } else {
            // FIXME: figure out if this case is correct
            Ok(ExitStatus::from(0))
        }
    }

    pub fn execute_command(&mut self, cmd: &Command) -> Result<ExitStatus> {
        match cmd {
            Command::Simple(cmd) => self.execute_simple_command(cmd),
            Command::Compound(_compound) => todo!(),
            Command::FunctionDefinition(_def) => todo!(),
        }
    }

    pub fn execute_pipeline(&mut self, pipeline: &Pipeline) -> Result<Vec<ExitStatus>> {
        if pipeline.rest.is_empty() {
            let status = self.execute_command(&pipeline.first)?;
            Ok(vec![status])
        } else {
            todo!()
        }
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
    pub fn from(code: i32) -> Self {
        Self { code }
    }
}
