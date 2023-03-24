pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process::{self, Stdio};

use crate::ast::{CompoundCommand, FunctionDefinition, LogicalOp, SimpleCommand};
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

    fn execute_simple_command(
        &mut self,
        cmd: &SimpleCommand,
        stdin: Stdio,
        stdout: Stdio,
    ) -> Result<process::Child> {
        if let Some(name) = cmd.name() {
            let mut command = process::Command::new(name);

            let child = command
                .stdin(stdin)
                .stdout(stdout)
                .args(cmd.args())
                .spawn()?;

            Ok(child)
        } else {
            todo!()
        }
    }

    fn execute_compound_command(
        &mut self,
        _cmd: &CompoundCommand,
        _stdin: Stdio,
        _stdout: Stdio,
    ) -> Result<process::Child> {
        todo!()
    }

    fn execute_function_defenition(
        &mut self,
        _func_def: &FunctionDefinition,
        _stdin: Stdio,
        _stdout: Stdio,
    ) -> Result<process::Child> {
        todo!()
    }

    fn execute_command(
        &mut self,
        command: &Command,
        stdin: Stdio,
        stdout: Stdio,
    ) -> Result<process::Child> {
        match command {
            Command::Simple(cmd) => self.execute_simple_command(cmd, stdin, stdout),
            Command::Compound(cmd) => self.execute_compound_command(cmd, stdin, stdout),
            Command::FunctionDefinition(func_def) => {
                self.execute_function_defenition(func_def, stdin, stdout)
            }
        }
    }

    pub fn execute_pipeline(
        &mut self,
        pipeline: &Pipeline,
        background: bool,
    ) -> Result<(ExitStatus, Vec<ExitStatus>)> {
        let pipeline_cmds = pipeline.pipeline();
        let mut pipeline_iter = pipeline_cmds.iter().peekable();
        let mut pids = Vec::with_capacity(pipeline_iter.len());

        let mut last_stdout = None;
        let mut last_status = ExitStatus::from_code(0);

        while let Some(cmd) = pipeline_iter.next() {
            let stdin = match last_stdout {
                Some(stdout) => Stdio::from(stdout),
                None => Stdio::inherit(),
            };

            let stdout = match pipeline_iter.peek() {
                Some(_) => Stdio::piped(),
                None => Stdio::inherit(),
            };

            let mut child = self.execute_command(cmd, stdin, stdout)?;
            last_stdout = child.stdout.take();
            pids.push(child.id());

            if !background && pipeline_iter.peek().is_none() {
                let status = child.wait().unwrap();
                last_status = ExitStatus::from(status);
            }
        }

        let exit_status = match (pipeline.has_bang(), last_status) {
            (false, code) => code,
            (true, code) if code.is_ok() => ExitStatus::from_code(1),
            (true, _) => ExitStatus::from_code(0),
        };

        Ok((exit_status, vec![exit_status]))
    }

    pub fn execute_and_or_list(
        &mut self,
        logical_expr: &AndOrList,
        background: bool,
    ) -> Result<Vec<ExitStatus>> {
        let (mut prev_status, mut codes) =
            self.execute_pipeline(&logical_expr.first, background)?;

        for (op, expr) in &logical_expr.rest {
            match (op, prev_status.is_ok()) {
                (LogicalOp::And(_), true) | (LogicalOp::Or(_), false) => {
                    let (status, mut pipeline_codes) = self.execute_pipeline(expr, background)?;
                    prev_status = status;
                    codes.append(&mut pipeline_codes);
                }
                _ => {}
            }
        }

        Ok(codes)
    }

    pub fn execute(&mut self, cmd: &CompleteCommand) -> Result<Vec<ExitStatus>> {
        let lists_with_separator = cmd.list_with_separator();

        let mut codes = Vec::new();

        for (and_or_list, separator) in lists_with_separator {
            codes.append(&mut self.execute_and_or_list(and_or_list, separator.is_async())?);
        }

        Ok(codes)
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

#[derive(Debug, Clone, Copy)]
pub struct ExitStatus {
    pub code: i32,
}

impl ExitStatus {
    pub fn from_code(code: i32) -> Self {
        Self { code }
    }

    pub fn is_ok(&self) -> bool {
        self.code == 0
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
