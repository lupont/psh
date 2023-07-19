mod builtin;
pub mod expand;
pub mod history;
pub mod parser;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Stdout, Write};
use std::ops::Not;
use std::os::fd::FromRawFd;
use std::os::unix::prelude::ExitStatusExt;
use std::path::PathBuf;
use std::process::{self, Stdio};

use self::parser::ast::prelude::*;
pub use crate::engine::history::{FileHistory, History};
use crate::{path, Error, Result};

pub struct Engine {
    pub writer: Stdout,
    pub commands: Vec<String>,
    pub history: Box<dyn History>,
    pub assignments: HashMap<String, String>,
    pub aliases: HashMap<String, String>,
    pub abbreviations: HashMap<String, String>,
    pub last_status: Vec<ExitStatus>,
}

impl Engine {
    pub fn new() -> Self {
        let history = FileHistory::init().expect("could not initialize history");
        Self {
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            history: Box::new(history),
            assignments: Default::default(),
            aliases: Default::default(),
            abbreviations: Default::default(),
            last_status: vec![ExitStatus::from_code(0)],
        }
    }

    pub fn get_value_of(&self, var_name: impl AsRef<str>) -> Option<String> {
        let var = var_name.as_ref();
        self.assignments
            .get(var)
            .cloned()
            .or_else(|| env::var(var).ok())
    }

    pub fn has_executable(&self, cmd: &str) -> bool {
        self.has_command(cmd) || self.has_alias(cmd) || builtin::has(cmd)
    }

    pub fn has_command(&self, cmd: &str) -> bool {
        path::has_relative_command(cmd)
            || self
                .commands
                .iter()
                .any(|c| c == cmd || c.ends_with(&format!("/{}", cmd)))
    }

    pub fn has_alias(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        self.aliases.keys().any(|a| a == cmd)
    }

    pub fn has_abbreviation(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        self.abbreviations.keys().any(|a| a == cmd)
    }

    pub fn execute_line(&mut self, line: impl ToString) -> Result<Vec<ExitStatus>> {
        let ast = parse(line.to_string(), false)?;
        self.walk_ast(ast)
    }

    pub fn execute_file(&mut self, path: PathBuf) -> Result<Vec<ExitStatus>> {
        let lines = std::fs::read_to_string(path)?;
        let ast = parse(lines, false)?;
        self.walk_ast(ast)
    }

    pub fn execute_builtin(&mut self, cmd: Command) -> Result<ExitStatus> {
        let Command::Simple(cmd) = cmd else {
            return Err(Error::Unimplemented("tried to execute complex command as builtin".to_string()));
        };

        let cmd = cmd.expand_name(self).expand_suffixes(self);

        let Some(command) = cmd.name() else {
            return Err(Error::Unimplemented("tried to execute empty command as builtin".to_string()));
        };

        let args = cmd.args().map(|s| s.as_str()).collect::<Vec<_>>();

        builtin::execute(self, command, &args)
    }

    fn expand_alias(&self, name: &str) -> (String, Vec<String>) {
        let initial_name = <&str>::clone(&name);
        let (mut name, mut args) = (name.to_string(), Vec::new());
        let mut stop = false;
        while let Some(expanded) = self.aliases.get(&name) {
            if stop {
                break;
            }
            let (a, b) = expanded.split_once(' ').unwrap_or((expanded, ""));
            let b = b
                .split(' ')
                .filter(|s| !s.is_empty())
                .map(ToString::to_string)
                .collect::<Vec<_>>();
            if a == initial_name {
                stop = true;
            }
            (name, args) = (a.to_string(), b);
        }
        (name, args)
    }

    fn execute_simple_command(
        &mut self,
        cmd: SimpleCommand,
        stdin: Stdio,
        stdout: Stdio,
        stderr: Stdio,
        has_multiple_commands: bool,
    ) -> Result<Option<(bool, process::Child)>> {
        let cmd = cmd.expand_name(self);
        let cmd = cmd.expand_prefixes(self);
        let cmd = cmd.expand_suffixes(self);

        let mut assignments = Vec::new();
        for assignment in cmd.assignments() {
            let lhs = assignment.lhs.name.clone();
            let rhs = match &assignment.rhs {
                Some(rhs) => rhs.name.clone(),
                None => "".to_string(),
            };
            assignments.push((lhs, rhs));
        }

        let Some(name) = cmd.name() else {
            if !has_multiple_commands {
                for (k, v) in assignments {
                    self.assignments.insert(k, v);
                }
            }
            return Ok(None);
        };

        let (name, alias_args) = self.expand_alias(name);

        if !self.has_executable(&name) {
            return Err(Error::UnknownCommand(name));
        }

        let mut command = process::Command::new(name);

        let mut stdin_override = None;
        let mut stdout_override = None;
        let mut stderr_override = None;
        for redirection in cmd.redirections() {
            match redirection {
                Redirection::File {
                    ty: RedirectionType::Input,
                    target,
                    ..
                } => {
                    let file = fs::OpenOptions::new()
                        .read(true)
                        .write(false)
                        .append(false)
                        .create(false)
                        .open(&target.name)?;
                    stdin_override = Some(Stdio::from(file));
                }
                Redirection::File {
                    input_fd,
                    ty: RedirectionType::Output,
                    target,
                    ..
                } => {
                    let file = fs::OpenOptions::new()
                        .read(false)
                        .write(true)
                        .create(true)
                        .append(false)
                        .truncate(true)
                        .open(&target.name)?;
                    match input_fd {
                        None | Some(FileDescriptor::Stdout) => {
                            stdout_override = Some(Stdio::from(file))
                        }
                        Some(FileDescriptor::Stderr) => stderr_override = Some(Stdio::from(file)),
                        _ => {}
                    };
                }
                Redirection::File {
                    input_fd,
                    ty: RedirectionType::OutputAppend,
                    target,
                    ..
                } => {
                    let file = fs::OpenOptions::new()
                        .read(false)
                        .write(true)
                        .create(true)
                        .append(true)
                        .open(&target.name)?;
                    match input_fd {
                        None | Some(FileDescriptor::Stdout) => {
                            stdout_override = Some(Stdio::from(file))
                        }
                        Some(FileDescriptor::Stderr) => stderr_override = Some(Stdio::from(file)),
                        _ => {}
                    };
                }
                Redirection::File {
                    input_fd,
                    ty: RedirectionType::OutputClobber,
                    target,
                    ..
                } => {
                    // TODO: real implementation
                    let file = fs::OpenOptions::new()
                        .read(false)
                        .write(true)
                        .create(true)
                        .append(false)
                        .open(&target.name)?;
                    match input_fd {
                        None | Some(FileDescriptor::Stdout) => {
                            stdout_override = Some(Stdio::from(file))
                        }
                        Some(FileDescriptor::Stderr) => stderr_override = Some(Stdio::from(file)),
                        _ => {}
                    };
                }
                Redirection::File {
                    input_fd,
                    ty: RedirectionType::ReadWrite,
                    target,
                    ..
                } => {
                    // TODO: real implementation
                    let file = fs::OpenOptions::new()
                        .read(false)
                        .write(true)
                        .create(true)
                        .append(false)
                        .open(&target.name)?;
                    match input_fd {
                        None | Some(FileDescriptor::Stdout) => {
                            stdout_override = Some(Stdio::from(file))
                        }
                        Some(FileDescriptor::Stderr) => stderr_override = Some(Stdio::from(file)),
                        _ => {}
                    };
                }

                Redirection::File {
                    ty: RedirectionType::InputFd,
                    target,
                    ..
                } => {
                    // TODO: does not currently work
                    let fd = target.to_string().parse::<i32>().unwrap();
                    let file = unsafe { fs::File::from_raw_fd(fd) };
                    stdin_override = Some(Stdio::from(file));
                }
                Redirection::File {
                    input_fd,
                    ty: RedirectionType::OutputFd,
                    target,
                    ..
                } => {
                    // TODO: does not currently work
                    let fd = target.to_string().parse::<i32>().unwrap();
                    let file = unsafe { fs::File::from_raw_fd(fd) };
                    match input_fd {
                        None | Some(FileDescriptor::Stdout) => {
                            stdout_override = Some(Stdio::from(file))
                        }
                        Some(FileDescriptor::Stderr) => stderr_override = Some(Stdio::from(file)),
                        _ => {}
                    };
                }

                Redirection::Here { .. } => {}
            }
        }

        let stdout_redirected = stdout_override.is_some();

        let mut proc = command
            .envs(env::vars())
            .envs(assignments)
            .stdin(stdin_override.unwrap_or(stdin))
            .stdout(stdout_override.unwrap_or(stdout))
            .stderr(stderr_override.unwrap_or(stderr))
            .args(cmd.args());

        if !alias_args.is_empty() {
            proc = proc.args(alias_args);
        }

        let child = proc.spawn()?;

        Ok(Some((stdout_redirected, child)))
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
        command: Command,
        stdin: Stdio,
        stdout: Stdio,
        stderr: Stdio,
        has_multiple_commands: bool,
    ) -> Result<Option<(bool, process::Child)>> {
        match command {
            Command::Simple(cmd) => {
                self.execute_simple_command(cmd, stdin, stdout, stderr, has_multiple_commands)
            }
            Command::Compound(_cmd, _redirections) => todo!(), //self.execute_compound_command(cmd, stdin, stdout, stderr),
            Command::FunctionDefinition(_func_def) => todo!(), //self.execute_function_defenition(func_def, stdin, stdout, stderr)
        }
    }

    pub fn execute_pipeline(&mut self, pipeline: Pipeline, background: bool) -> Result<ExitStatus> {
        let has_bang = pipeline.has_bang();
        let pipeline_cmds = pipeline.full();
        let has_multiple_commands = pipeline_cmds.len() > 1;
        let mut pipeline_iter = pipeline_cmds.into_iter().peekable();
        let mut pids = Vec::with_capacity(pipeline_iter.len());

        let mut last_stdout = None;
        let mut last_status = ExitStatus::from_code(0);

        while let Some(cmd) = pipeline_iter.next() {
            if cmd.is_builtin() {
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

            if let Some((stdout_redirected, mut child)) =
                self.execute_command(cmd, stdin, stdout, stderr, has_multiple_commands)?
            {
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
            } else {
                last_stdout = Some(None);
            }
        }

        self.last_status = vec![last_status];

        Ok(if has_bang { !last_status } else { last_status })
    }

    pub fn execute_and_or_list(
        &mut self,
        and_or_list: AndOrList,
        background: bool,
    ) -> Result<Vec<ExitStatus>> {
        let mut prev_status = self.execute_pipeline(and_or_list.head, background)?;
        let mut codes = vec![prev_status];

        for (op, _, expr) in and_or_list.tail {
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

    pub fn execute(&mut self, cmd: CompleteCommand) -> Result<Vec<ExitStatus>> {
        let lists_with_separator = cmd.list_with_separator();

        let mut codes = Vec::new();

        for (and_or_list, separator) in lists_with_separator {
            let res = self.execute_and_or_list(and_or_list, separator.is_async());

            if let Err(e @ Error::UnknownCommand(_)) = res {
                codes.push(ExitStatus::from_code(127));
                writeln!(self.writer, "psh: {e}")?;
            } else {
                codes.append(&mut res?);
            }
        }

        Ok(codes)
    }

    fn walk_ast(&mut self, ast: SyntaxTree) -> Result<Vec<ExitStatus>> {
        let mut results = Vec::new();
        if let Some((cmds, _)) = ast.commands {
            for cmd in cmds.full() {
                results.append(&mut self.execute(cmd)?);
            }
        }
        Ok(results)
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExitStatus {
    Code(i32),
    Signal(i32),
}

impl ExitStatus {
    pub fn from_code(code: i32) -> Self {
        if code > 255 {
            Self::Signal(code - 255)
        } else {
            Self::Code(code)
        }
    }

    pub fn raw_code(&self) -> i32 {
        match self {
            Self::Code(code) => *code,
            Self::Signal(signal) => 255 + signal,
        }
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Code(0))
    }
}

impl ToString for ExitStatus {
    fn to_string(&self) -> String {
        match self {
            Self::Code(c) => format!("{c}"),
            Self::Signal(s) => match s {
                1 => "SIGHUP",
                2 => "SIGINT",
                3 => "SIGQUIT",
                4 => "SIGILL",
                5 => "SIGTRAP",
                6 => "SIGABRT",
                7 => "SIGBUS",
                8 => "SIGFPE",
                9 => "SIGKILL",
                10 => "SIGUSR1",
                11 => "SIGSEGV",
                12 => "SIGUSR2",
                13 => "SIGPIPE",
                14 => "SIGALRM",
                15 => "SIGTERM",
                16 => "SIGSTKFLT",
                17 => "SIGCHLD",
                18 => "SIGCONT",
                19 => "SIGSTOP",
                20 => "SIGTSTP",
                21 => "SIGTTIN",
                22 => "SIGTTOU",
                23 => "SIGURG",
                24 => "SIGXCPU",
                25 => "SIGXFSZ",
                26 => "SIGVTALRM",
                27 => "SIGPROF",
                28 => "SIGWINCH",
                29 => "SIGIO",
                30 => "SIGPWR",
                31 => "SIGSYS",
                _ => "???",
            }
            .to_string(),
        }
    }
}

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(status: std::process::ExitStatus) -> Self {
        if let Some(code) = status.code() {
            Self::Code(code)
        } else if let Some(signal) = status.signal() {
            Self::Signal(signal)
        } else {
            todo!()
        }
    }
}

impl Not for ExitStatus {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Code(0) => Self::Output::Code(1),
            Self::Code(_) => Self::Output::Code(0),

            // TODO: figure out if this is correct
            Self::Signal(s) => Self::Output::Signal(s),
        }
    }
}
