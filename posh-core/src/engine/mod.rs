pub mod expand;
pub mod history;
pub mod parser;

use std::fs;
use std::io::{self, Stdout, Write};
use std::ops::Not;
use std::path::PathBuf;
use std::process::{self, Stdio};

use crate::{path, Error, Result};

use self::expand::Expand;
pub use self::history::{FileHistory, History};
use self::parser::ast::{
    parse, AndOrList, Command, CompleteCommand, CompoundCommand, FunctionDefinition, LogicalOp,
    Pipeline, Redirection, SimpleCommand, SyntaxTree,
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

    fn exit(&self, code: i32) -> ! {
        std::process::exit(code)
    }

    pub fn has_builtin(&self, s: impl AsRef<str>) -> bool {
        let name = s.as_ref();
        let has = |s| name == s || name.starts_with(&format!("{s} "));
        has("cd") || has("exit") || has(":") || has("debug")
    }

    pub fn is_builtin(&self, cmd: &Command) -> bool {
        match cmd {
            Command::Simple(cmd) => {
                if let Some(name) = cmd.name() {
                    self.has_builtin(name)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn execute_builtin(&mut self, cmd: &Command) -> Result<ExitStatus> {
        let Command::Simple(cmd) = cmd else {
            return Err(Error::Unimplemented("tried to execute complex command as builtin".to_string()));
        };

        let Some(command) = cmd.name() else {
            return Err(Error::Unimplemented("tried to execute empty command as builtin".to_string()));
        };

        let args = cmd.args().collect::<Vec<_>>();

        match (command.as_str(), &args[..]) {
            ("debug", _) => {
                writeln!(self.writer, "prev_dir: {:?}", self.prev_dir)?;
                writeln!(self.writer, "bindings: {:?}", self.bindings)?;
                Ok(ExitStatus::from_code(0))
            }

            (":", _) => Ok(ExitStatus::from_code(0)),

            ("exit", []) => self.exit(0),
            ("exit", [code]) => {
                if let Ok(s) = code.parse::<i32>() {
                    self.exit(s)
                } else {
                    writeln!(self.writer, "invalid integer: '{}'", code)?;
                    Ok(ExitStatus::from_code(1))
                }
            }

            ("cd", [dir]) => self.cd(Some(dir)),
            ("cd", []) => self.cd(None),
            ("cd", _) => {
                writeln!(self.writer, "invalid number of arguments")?;
                Ok(ExitStatus::from_code(1))
            }

            (c, _) => Err(Error::UnknownCommand(c.to_string())),
        }
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
        let ast = ast.expand(self);
        self.walk_ast(ast)
    }

    fn execute_simple_command(
        &mut self,
        cmd: &SimpleCommand,
        stdin: Stdio,
        stdout: Stdio,
        stderr: Stdio,
    ) -> Result<(bool, process::Child)> {
        let Some(name) = cmd.name() else {
            return Err(crate::Error::Unimplemented("".to_string()));
        };

        let mut command = process::Command::new(name);

        let mut assignments = Vec::new();

        for assignment in cmd.assignments() {
            let lhs = &assignment.lhs;
            let rhs = match &assignment.rhs {
                Some(rhs) => rhs.name.as_str(),
                None => "",
            };
            assignments.push((lhs, rhs));
        }

        let mut stdin_override = None;
        let mut stdout_override = None;
        let mut stderr_override = None;

        for redirection in cmd.redirections() {
            match redirection {
                Redirection::Output {
                    file_descriptor,
                    append,
                    target,
                } => {
                    let fd = &file_descriptor.name;

                    let file = fs::OpenOptions::new()
                        .read(false)
                        .write(true)
                        .append(*append)
                        .create(true)
                        .open(&target.name)?;

                    if fd.is_empty() || fd == "1" {
                        stdout_override = Some(Stdio::from(file));
                    } else if fd == "2" {
                        stderr_override = Some(Stdio::from(file));
                    }
                }

                Redirection::Input {
                    file_descriptor,
                    target,
                } => {
                    let fd = &file_descriptor.name;

                    if fd.is_empty() || fd == "0" {
                        let file = fs::OpenOptions::new()
                            .read(true)
                            .write(false)
                            .open(&target.name)?;
                        stdin_override = Some(Stdio::from(file));
                    }
                }

                Redirection::HereDocument {
                    file_descriptor: _,
                    delimiter: _,
                } => todo!(),
            }
        }

        let stdout_redirected = stdout_override.is_some();

        let child = command
            .envs(assignments)
            .stdin(stdin_override.unwrap_or(stdin))
            .stdout(stdout_override.unwrap_or(stdout))
            .stderr(stderr_override.unwrap_or(stderr))
            .args(cmd.args())
            .spawn()?;

        Ok((stdout_redirected, child))
    }

    fn _execute_compound_command(
        &mut self,
        _cmd: &CompoundCommand,
        _stdin: Stdio,
        _stdout: Stdio,
        _stderr: Stdio,
    ) -> Result<process::Child> {
        todo!()
    }

    fn _execute_function_defenition(
        &mut self,
        _func_def: &FunctionDefinition,
        _stdin: Stdio,
        _stdout: Stdio,
        _stderr: Stdio,
    ) -> Result<process::Child> {
        todo!()
    }

    fn execute_command(
        &mut self,
        command: &Command,
        stdin: Stdio,
        stdout: Stdio,
        stderr: Stdio,
    ) -> Result<(bool, process::Child)> {
        match command {
            Command::Simple(cmd) => self.execute_simple_command(cmd, stdin, stdout, stderr),
            Command::Compound(_cmd, _redirections) => todo!(), //self.execute_compound_command(cmd, stdin, stdout, stderr),
            Command::FunctionDefinition(_func_def) => todo!(), //self.execute_function_defenition(func_def, stdin, stdout, stderr)
        }
    }

    pub fn execute_pipeline(
        &mut self,
        pipeline: &Pipeline,
        background: bool,
    ) -> Result<ExitStatus> {
        let pipeline_cmds = pipeline.full();
        let mut pipeline_iter = pipeline_cmds.iter().peekable();
        let mut pids = Vec::with_capacity(pipeline_iter.len());

        let mut last_stdout = None;
        let mut last_status = ExitStatus::from_code(0);

        while let Some(cmd) = pipeline_iter.next() {
            if self.is_builtin(cmd) {
                last_status = self.execute_builtin(cmd)?;
                break;
            }

            let stdin = match last_stdout {
                Some(Some(stdout)) => Stdio::from(stdout),
                Some(None) => Stdio::null(),
                None => Stdio::inherit(),
            };

            let stdout = match pipeline_iter.peek() {
                Some(_) => Stdio::piped(),
                None => Stdio::inherit(),
            };

            // FIXME: figure out how to do this on-spec
            let stderr = Stdio::inherit();

            let (stdout_redirected, mut child) =
                self.execute_command(cmd, stdin, stdout, stderr)?;

            last_stdout = if stdout_redirected {
                Some(None)
            } else {
                Some(child.stdout.take())
            };

            pids.push(child.id());

            if !background && pipeline_iter.peek().is_none() {
                let status = child.wait().unwrap();
                last_status = ExitStatus::from(status);
            }
        }

        Ok(if pipeline.has_bang() {
            !last_status
        } else {
            last_status
        })
    }

    pub fn execute_and_or_list(
        &mut self,
        logical_expr: &AndOrList,
        background: bool,
    ) -> Result<Vec<ExitStatus>> {
        let mut prev_status = self.execute_pipeline(&logical_expr.first, background)?;
        let mut codes = vec![prev_status];

        for (op, expr) in &logical_expr.rest {
            match (op, prev_status.is_ok()) {
                (LogicalOp::And(_), true) | (LogicalOp::Or(_), false) => {
                    prev_status = self.execute_pipeline(expr, background)?;
                    codes.push(prev_status);
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

impl Not for ExitStatus {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self.code {
            0 => Self::Output { code: 1 },
            _ => Self::Output { code: 0 },
        }
    }
}
