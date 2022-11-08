pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process::{self, ChildStdout, Stdio};

use crate::config::ABBREVIATIONS;
use crate::repl::input::read_line;
use crate::{path, Result};

pub use self::history::History;
use self::parser::ast::{parse, CommandType, SyntaxTree};

pub fn read_and_execute<W: Write>(engine: &mut Engine<W>) -> Result<Vec<ExitStatus>> {
    let line = read_line(engine)?;
    let ast = parse(line);
    walk_ast(engine, ast)
}

fn walk_ast<W: Write>(engine: &mut Engine<W>, ast: SyntaxTree) -> Result<Vec<ExitStatus>> {
    ast.commands()
        .iter()
        .fold(Ok(vec![]), |_, c| engine.execute(c))
}

pub struct Engine<W: Write> {
    pub writer: W,
    pub prev_dir: Option<PathBuf>,
    pub commands: Vec<String>,
    pub history: History,
}

impl Engine<Vec<u8>> {
    pub fn in_memory() -> Self {
        let history = History::init().expect("could not initialize history");
        Self {
            writer: Vec::new(),
            prev_dir: None,
            commands: path::get_cmds_from_path(),
            history,
        }
    }
}

impl<W: Write> Engine<W> {
    pub fn has_builtin(&self, _builtin: impl AsRef<str>) -> bool {
        // builtins will return...
        false
    }

    pub fn has_command(&self, cmd: impl AsRef<str>) -> bool {
        self.commands
            .iter()
            .any(|c| c.ends_with(&format!("/{}", cmd.as_ref())))
    }

    pub fn has_abbreviation(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        ABBREVIATIONS.iter().any(|&(a, _)| a == cmd)
    }

    pub fn writer(&mut self) -> &mut W {
        &mut self.writer
    }

    fn _expand_all(&self, _cmd: &CommandType) -> Result<Vec<String>> {
        // This is the place in which we will expand subcommands,
        // variables, globs, and tildes
        todo!()
    }

    pub fn execute(&mut self, cmd: &CommandType) -> Result<Vec<ExitStatus>> {
        match cmd {
            CommandType::Single(cmd) => {
                let child = process::Command::new(cmd.cmd_name())
                    .args(cmd.args())
                    .spawn()?;
                let result = child.wait_with_output()?;
                let code = result.status.code().unwrap_or_default();

                Ok(vec![ExitStatus::from(code)])
            }

            CommandType::Pipeline(cmds) if cmds.is_empty() => todo!(),

            CommandType::Pipeline(cmds) => {
                let mut prev_result: Option<(Option<ChildStdout>, i32)> = None;
                let mut statuses = Vec::with_capacity(cmds.len());

                for (i, cmd) in cmds.iter().enumerate() {
                    let stdin = match prev_result {
                        Some((Some(stdout), _)) => Stdio::from(stdout),
                        _ => Stdio::inherit(),
                    };

                    let stdout = if i == cmds.len() - 1 {
                        Stdio::inherit()
                    } else {
                        Stdio::piped()
                    };

                    let mut child = process::Command::new(cmd.cmd_name())
                        .args(cmd.args())
                        .stdin(stdin)
                        .stdout(stdout)
                        .spawn()?;

                    prev_result = Some((child.stdout.take(), 0));

                    let result = child.wait_with_output()?;
                    statuses.push(ExitStatus::from(result.status.code().unwrap_or_default()));
                }
                Ok(statuses)
            }
        }
    }
}

impl Engine<Stdout> {
    pub fn new() -> Self {
        let history = History::init().expect("could not initialize history");
        Self {
            prev_dir: None,
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            history,
        }
    }
}

impl Default for Engine<Stdout> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ExitStatus {
    pub code: i32,
}

impl ExitStatus {
    pub fn from(code: i32) -> Self {
        Self { code }
    }
}
