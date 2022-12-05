pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process::{self, ChildStdout, Stdio};

use crate::config::ABBREVIATIONS;
use crate::repl::input::read_line;
use crate::{path, Error, Result};

pub use self::history::{FileHistory, History};
use self::parser::ast::{parse, Command, CommandType, SyntaxTree};

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
        std::process::exit(code);
    }

    pub fn has_builtin(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        let has = |s| cmd == s || cmd.starts_with(&format!("{s} "));
        has("cd") || has("exit")
    }

    pub fn execute_builtin(&mut self, cmd: Command) -> Result<ExitStatus> {
        let cmd = cmd.expand_all();
        let command = cmd.cmd_name();
        let args = cmd.args();

        match (command.as_str(), &args[..]) {
            ("exit", []) => self.exit(0),
            ("exit", [code]) => {
                if let Ok(s) = code.parse::<i32>() {
                    self.exit(s)
                } else {
                    writeln!(self.writer, "invalid integer: '{}'", code)?;
                    Ok(ExitStatus::from(1))
                }
            }

            ("cd", [dir]) => self.cd(Some(dir)),
            ("cd", []) => self.cd(None::<&str>),
            ("cd", _) => {
                writeln!(self.writer, "invalid number of arguments")?;
                Ok(ExitStatus::from(1))
            }

            (c, _) => Err(Error::UnknownCommand(c.to_string())),
        }
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
            CommandType::Single(cmd) if self.has_builtin(cmd.cmd_name()) => {
                self.execute_builtin(cmd).map(|r| vec![r])
            }

            CommandType::Single(cmd) if self.has_command(cmd.cmd_name()) => {
                let cmd = cmd.expand_all();
                let child = process::Command::new(cmd.cmd_name())
                    .args(cmd.args())
                    .spawn()?;
                let result = child.wait_with_output()?;
                let code = result.status.code().unwrap_or_default();

                Ok(vec![ExitStatus::from(code)])
            }

            CommandType::Single(cmd) => {
                writeln!(self.writer, "Unknown command: {}", cmd.cmd_name())?;
                Ok(vec![ExitStatus::from(127)])
            }

            CommandType::Pipeline(cmds) if cmds.is_empty() => todo!(),

            CommandType::Pipeline(cmds) => {
                if let Some(cmd) = cmds.iter().find(|cmd| {
                    !self.has_command(cmd.cmd_name()) && !self.has_builtin(cmd.cmd_name())
                }) {
                    writeln!(self.writer, "Unknown command: {}", cmd.cmd_name())?;
                    return Ok(vec![ExitStatus::from(127)]);
                }

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

    impl Engine<Vec<u8>> {
        pub fn in_memory() -> Self {
            let history = self::history::DummyHistory;
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

    #[test]
    fn basic_commands_work() {
        let mut engine = Engine::in_memory();

        engine.execute_line("echo oof | rev; echo bar").unwrap();
        engine.execute_line("echo baz").unwrap();

        assert_eq!("foo\nbar\nbaz\n", engine.output());
    }
}
