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
    fn cd(&mut self, dir: Option<&str>) -> Result<ExitStatus> {
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

    fn exit(&self, code: i32) -> ! {
        std::process::exit(code)
    }

    pub fn has_builtin(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        let has = |s| cmd == s || cmd.starts_with(&format!("{s} "));
        has("cd") || has("exit")
    }

    pub fn execute_builtin(&mut self, cmd: CompleteCommand) -> Result<ExitStatus> {
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
        let ast = parse(line.to_string())?;
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
        let mut command = process::Command::new(cmd.name());

        let mut child = command.args(cmd.args()).spawn()?;
        let exit_status = child.wait()?;
        // self.writer.write_all(&output.stdout)?;
        // let code = output.status.code().unwrap_or_default();
        Ok(ExitStatus::from(exit_status.code().unwrap()))
    }

    pub fn execute_command(&mut self, cmd: &Command) -> Result<ExitStatus> {
        match cmd {
            Command::Simple(cmd) => self.execute_simple_command(cmd),
            Command::Compound(compound) => todo!(),
            Command::FunctionDefinition(def) => todo!(),
        }
    }

    pub fn execute_pipeline(&mut self, pipeline: &Pipeline) -> Result<Vec<ExitStatus>> {
        if let Command::Simple(cmd) = &pipeline.first {
            let status = self.execute_simple_command(cmd)?;
            return Ok(vec![status]);
        }
        todo!()
    }

    pub fn execute_logical_expr(&mut self, logical_expr: &AndOrList) -> Result<Vec<ExitStatus>> {
        self.execute_pipeline(&logical_expr.first)
    }

    pub fn execute(&mut self, cmd: &CompleteCommand) -> Result<Vec<ExitStatus>> {
        let list = &cmd.list;

        let exit_status = self.execute_logical_expr(&list.first)?;

        Ok(exit_status)
    }

    // pub fn execute(&mut self, cmd: CommandType) -> Result<Vec<ExitStatus>> {
    //     let cmd = cmd.expand()?;

    //     match cmd {
    //         CommandType::Single(cmd) if self.has_builtin(cmd.cmd_name()) => {
    //             self.execute_builtin(cmd).map(|r| vec![r])
    //         }

    //         CommandType::Single(cmd) => {
    //             if !self.has_command(cmd.cmd_name()) {
    //                 writeln!(self.writer, "Unknown command: {}", cmd.cmd_name())?;
    //                 return Ok(vec![ExitStatus::from(127)]);
    //             }

    //             let mut command = self.build_command(&cmd, None, true)?;
    //             let output = command.output()?;
    //             self.writer.write_all(&output.stdout)?;
    //             let code = output.status.code().unwrap_or_default();

    //             Ok(vec![ExitStatus::from(code)])
    //         }

    //         CommandType::Pipeline(cmds) => {
    //             if let Some(cmd) = cmds.iter().find(|cmd| {
    //                 !self.has_command(cmd.cmd_name()) && !self.has_builtin(cmd.cmd_name())
    //             }) {
    //                 writeln!(self.writer, "Unknown command: {}", cmd.cmd_name())?;
    //                 return Ok(vec![ExitStatus::from(127)]);
    //             }

    //             let mut prev_result: Option<ChildStdout> = None;
    //             let mut statuses = Vec::with_capacity(cmds.len());

    //             for (i, cmd) in cmds.iter().enumerate() {
    //                 let is_final = i == cmds.len() - 1;
    //                 let mut command = self.build_command(cmd, Some(prev_result), is_final)?;

    //                 let mut child = command.spawn()?;

    //                 prev_result = child.stdout.take();

    //                 let output = child.wait_with_output()?;
    //                 self.writer.write_all(&output.stdout)?;

    //                 statuses.push(ExitStatus::from(output.status.code().unwrap_or_default()));
    //             }

    //             Ok(statuses)
    //         }
    //     }
    // }

    fn walk_ast(&mut self, ast: SyntaxTree) -> Result<Vec<ExitStatus>> {
        // ast.commands
        //     .into_iter()
        //     .fold(Ok(vec![]), |_, c| self.execute(c))
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
