pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process::{self, ChildStdout, Stdio};

use crate::config::ABBREVIATIONS;
use crate::repl::input::read_line;
use crate::{path, Result};

use self::history::DummyHistory;
pub use self::history::{FileHistory, History};
use self::parser::ast::{parse, CommandType, SyntaxTree};

pub struct Engine<W: Write> {
    pub writer: W,
    pub prev_dir: Option<PathBuf>,
    pub commands: Vec<String>,
    pub history: Box<dyn History>,
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

    pub fn read_and_execute(&mut self) -> Result<Vec<ExitStatus>> {
        let line = read_line(self)?;
        self.history.append(&line)?;
        let ast = parse(line);
        self.walk_ast(ast)
    }

    pub fn execute_line(&mut self, line: impl ToString) -> Result<Vec<ExitStatus>> {
        let ast = parse(line.to_string());
        self.walk_ast(ast)
    }

    fn walk_ast(&mut self, ast: SyntaxTree) -> Result<Vec<ExitStatus>> {
        ast.consume()
            .into_iter()
            .fold(Ok(vec![]), |_, c| self.execute(c))
    }

    pub fn execute(&mut self, cmd: CommandType) -> Result<Vec<ExitStatus>> {
        match cmd {
            CommandType::Single(cmd) => {
                let cmd = cmd.expand_all();
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

impl Default for Engine<Stdout> {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine<Vec<u8>> {
    pub fn in_memory() -> Self {
        let history = DummyHistory;
        Self {
            writer: Vec::new(),
            prev_dir: None,
            commands: path::get_cmds_from_path(),
            history: Box::new(history),
        }
    }

    pub fn output(&self) -> std::borrow::Cow<str> {
        String::from_utf8_lossy(&self.writer)
    }

    pub fn execute_line(&mut self, line: impl ToString) -> Result<()> {
        let ast = parse(line.to_string());
        self.walk_ast(ast)
    }

    fn walk_ast(&mut self, ast: SyntaxTree) -> Result<()> {
        ast.commands().iter().fold(Ok(()), |_, c| self.execute(c))
    }

    fn execute(&mut self, cmd: &CommandType) -> Result<()> {
        match cmd {
            CommandType::Single(cmd) => {
                let mut child = process::Command::new(cmd.cmd_name())
                    .args(cmd.args())
                    .output()?;

                self.writer.append(&mut child.stdout);

                Ok(())
            }

            CommandType::Pipeline(cmds) => {
                let mut prev_result: Option<(Option<ChildStdout>, i32)> = None;

                for (i, cmd) in cmds.iter().enumerate() {
                    let stdin = match prev_result {
                        Some((Some(stdout), _)) => Stdio::from(stdout),
                        _ => Stdio::inherit(),
                    };

                    if i == cmds.len() - 1 {
                        let mut child = process::Command::new(cmd.cmd_name())
                            .args(cmd.args())
                            .stdin(stdin)
                            .output()?;
                        self.writer.append(&mut child.stdout);
                        break;
                    } else {
                        let mut child = process::Command::new(cmd.cmd_name())
                            .args(cmd.args())
                            .stdin(stdin)
                            .stdout(Stdio::piped())
                            .spawn()?;

                        prev_result = Some((child.stdout.take(), 0));
                    }
                }

                Ok(())
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_commands_work() {
        let mut engine = Engine::in_memory();

        engine.execute_line("echo oof | rev; echo bar").unwrap();
        engine.execute_line("echo baz").unwrap();

        assert_eq!("foo\nbar\nbaz\n", engine.output());
    }
}
