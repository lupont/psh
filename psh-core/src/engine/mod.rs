pub mod builtin;
pub mod expand;
pub mod history;
mod util;

use std::collections::HashMap;
use std::env;
use std::ffi::CString;
use std::ops::Not;
use std::os::fd::RawFd;
use std::os::unix::prelude::ExitStatusExt;
use std::path::PathBuf;

use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{close, dup, dup2, execvp, pipe};

use crate::ast::nodes::*;
use crate::ast::parse;
use crate::engine::expand::Expand;
use crate::engine::history::{FileHistory, History};
use crate::{path, Error, Result};

pub struct Engine {
    pub history: Box<dyn History>,
    pub assignments: HashMap<String, String>,
    pub aliases: HashMap<String, String>,
    pub abbreviations: HashMap<String, String>,
    pub last_status: Vec<ExitStatus>,
}

#[derive(Debug, Clone)]
struct ExecutionContext {
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
    fds: Vec<(FileDescriptor, FileDescriptor)>,
    assignments: HashMap<String, String>,
    background: bool,
}

impl ExecutionContext {
    fn dup_fds(&self) -> Result<()> {
        for &(src, dst) in &self.fds {
            if src != dst {
                dup2(src.as_raw_fd(), dst.as_raw_fd())?;
            }
        }

        if !self.fds.iter().any(|&(_, dst)| dst.is_stdin()) {
            dup2(self.stdin, FileDescriptor::Stdin.as_raw_fd())?;
        }

        if !self.fds.iter().any(|&(_, dst)| dst.is_stdout()) {
            dup2(self.stdout, FileDescriptor::Stdout.as_raw_fd())?;
        }

        if !self.fds.iter().any(|&(_, dst)| dst.is_stderr()) {
            dup2(self.stderr, FileDescriptor::Stderr.as_raw_fd())?;
        }

        Ok(())
    }
}

impl Default for ExecutionContext {
    fn default() -> Self {
        Self {
            stdin: 0,
            stdout: 1,
            stderr: 2,
            fds: Default::default(),
            assignments: Default::default(),
            background: false,
        }
    }
}

impl Engine {
    pub fn new() -> Self {
        let history = FileHistory::init().expect("could not initialize history");
        Self {
            history: Box::new(history),
            assignments: Default::default(),
            aliases: Default::default(),
            abbreviations: Default::default(),
            last_status: vec![ExitStatus::from_code(0)],
        }
    }

    pub fn get_file_in_path(&self, file: &str) -> Option<String> {
        if let Some(path) = self.get_value_of("PATH") {
            let paths = path.split(':');

            for path in paths {
                if let Ok(dirs) = std::fs::read_dir(path) {
                    for entry in dirs.filter_map(|f| f.ok()) {
                        if file == entry.file_name() {
                            return Some(format!("{}", entry.path().display()));
                        }
                    }
                }
            }
        }

        None
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
            || (self
                .get_file_in_path(cmd)
                .map(|file| util::is_executable(&file))
                .unwrap_or(false))
    }

    pub fn has_alias(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        self.aliases.keys().any(|a| a == cmd)
    }

    pub fn has_abbreviation(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        self.abbreviations.keys().any(|a| a == cmd)
    }

    // FIXME: this needs to be totally reworked. the best way would be
    //        to replace the actual input string as needed, but this
    //        would require us to be able to take a SyntaxTree, update
    //        the originating string and re-parse
    fn expand_alias(&self, name: &str) -> Vec<String> {
        let (mut name, mut args) = (name.to_string(), Vec::new());
        // should also be recursive
        if let Some(expanded) = self.aliases.get(&name) {
            let (a, b) = expanded.split_once(' ').unwrap_or((expanded, ""));
            let b = b
                .split(' ')
                .filter(|s| !s.is_empty())
                .map(ToString::to_string)
                .collect::<Vec<_>>();
            (name, args) = (a.to_string(), b);
        }
        args.insert(0, name);
        args
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

    fn execute_builtin(
        &mut self,
        args: &[impl AsRef<str>],
        context: ExecutionContext,
    ) -> Result<ExitStatus> {
        let args = args.iter().map(|s| s.as_ref()).collect::<Vec<_>>();

        let old_fds = [(dup(0)?, 0), (dup(1)?, 1), (dup(2)?, 2)];
        context.dup_fds()?;
        let status = builtin::execute(self, args[0], &args[1..])?;

        for (fd, n) in old_fds {
            dup2(fd, n)?;
            close(fd)?;
        }

        Ok(status)
    }

    fn execute_external_command(
        &mut self,
        args: &[impl AsRef<str>],
        context: ExecutionContext,
    ) -> Result<ExitStatus> {
        let child = util::spawn_subshell(|| {
            context.dup_fds()?;

            for (key, val) in &context.assignments {
                env::set_var(key, val);
            }

            let args = args
                .iter()
                .map(|s| CString::new(s.as_ref()).unwrap())
                .collect::<Vec<_>>();

            match execvp(&args[0], &args) {
                Ok(_) => unreachable!(),
                Err(e) => panic!("psh: error in exec: {e}"),
            }
        })?;

        let mut rc = 0;
        if !context.background {
            if let Ok(WaitStatus::Exited(_, code)) = waitpid(child, None) {
                rc = code;
            }
        }

        Ok(ExitStatus::from_code(rc))
    }

    pub fn execute_pipeline(&mut self, pipeline: Pipeline, background: bool) -> Result<ExitStatus> {
        let has_bang = pipeline.has_bang();
        let pipeline_cmds = pipeline.full();
        let pipeline_amount = pipeline_cmds.len();
        let mut pipeline_iter = pipeline_cmds.into_iter().peekable();

        let mut stdin = 0;
        let mut last_status = ExitStatus::from_code(0);

        'outer: while let Some(cmd) = pipeline_iter.next() {
            if let Command::Simple(cmd) = cmd {
                let (pipe_read, pipe_write) = pipe()?;

                let stdout = if pipeline_iter.peek().is_some() {
                    pipe_write
                } else {
                    1
                };

                let mut fds = Vec::new();

                for redirection in cmd.redirections() {
                    let Redirection::File {
                        input_fd,
                        ty,
                        target,
                        ..
                    } = redirection else {
                        continue;
                    };

                    let target = target.clone().expand(self).join(" ");
                    match ty.default_src_fd(&target) {
                        Ok(mut src_fd) => {
                            let dst_fd = input_fd.unwrap_or_else(|| ty.default_dst_fd());
                            if src_fd == FileDescriptor::Stdin {
                                src_fd = FileDescriptor::from(stdin);
                            } else if src_fd == FileDescriptor::Stdout {
                                src_fd = FileDescriptor::from(stdout);
                            }
                            fds.push((src_fd, dst_fd));
                        }
                        Err(e) => {
                            eprintln!("psh: {e}");
                            break 'outer;
                        }
                    }
                }

                let assignments = {
                    let mut assignments = HashMap::new();
                    for assignment in cmd.assignments() {
                        let rhs = if let Some(rhs) = &assignment.rhs {
                            rhs.clone().expand(self).join(" ")
                        } else {
                            Default::default()
                        };
                        assignments.insert(assignment.lhs.to_string(), rhs);
                    }
                    assignments
                };

                let context = ExecutionContext {
                    stdin,
                    stdout,
                    stderr: 2,
                    fds,
                    background,
                    assignments,
                };

                if cmd.name().is_some() {
                    let mut args = cmd.expand_into_args(self);

                    if !args.is_empty() {
                        let alias_args = self.expand_alias(&args[0]);
                        args.splice(0..1, alias_args);
                        last_status = if !self.has_executable(&args[0]) {
                            return Err(Error::UnknownCommand(args[0].to_string()));
                        } else if cmd.is_builtin() {
                            // TODO: assignments
                            self.execute_builtin(&args, context)?
                        } else {
                            self.execute_external_command(&args, context)?
                        };
                    }
                } else if pipeline_amount == 1 {
                    for (key, val) in context.assignments {
                        self.assignments.insert(key, val);
                    }
                }

                stdin = pipe_read;
                close(pipe_write)?;
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
                eprintln!("psh: {e}");
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
