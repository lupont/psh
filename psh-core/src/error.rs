use std::fmt;
use std::io;
use std::path::PathBuf;

use crate::ast::prelude::*;

pub type Result<T> = std::result::Result<T, Error>;

pub type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    NoHome,
    InvalidHistfile(PathBuf),
    HistoryOutOfBounds,
    UnknownCommand(String),
    UnknownBuiltin(String),
    Unimplemented(String),
    SyntaxError(String),
    ParseError(ParseError),
    CancelledLine,
    Incomplete(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Io(e) => e.to_string(),
                Self::NoHome => "could not read $HOME".to_string(),
                Self::InvalidHistfile(path) =>
                    format!("$POSH_HISTFILE contains invalid path: {}", path.display()),
                Self::HistoryOutOfBounds => "tried to read beyond the history bounds.".to_string(),
                Self::UnknownCommand(cmd) => format!("unknown command: '{}'", cmd),
                Self::UnknownBuiltin(cmd) => format!("unknown builtin: '{}'", cmd),
                Self::Unimplemented(s) => s.to_string(),
                Self::SyntaxError(s) => format!("could not parse the following: {s}"),
                Self::ParseError(e) => e.to_string(),
                Self::CancelledLine => "line input cancelled".to_string(),
                Self::Incomplete(line) => format!("incomplete line: '{line}'"),
            }
        )
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Self::ParseError(e)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidName(String),
    None,
    Done,
    Unimplemented(String),
    UnfinishedCompleteCommands(LeadingWhitespace, CompleteCommands),
    UnfinishedCompleteCommand(LeadingWhitespace, CompleteCommand),
    UnfinishedList(LeadingWhitespace, List),
    UnfinishedAndOrList(LeadingWhitespace, AndOrList),
    UnfinishedPipeline(LeadingWhitespace, Pipeline),
    UnfinishedPipeSequence(LeadingWhitespace, PipeSequence),
    UnfinishedCommand(Command),
    UnfinishedSimpleCommand(SimpleCommand),
    UnfinishedWord(Word),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidName(name) => format!("`{name}` is not a valid name"),
                Self::None => "could not parse this here".to_string(),
                Self::Done => "all finished".to_string(),
                Self::Unimplemented(s) => format!("not yet implemented: {s}"),
                Self::UnfinishedCompleteCommands(_, _) =>
                    "unfinished complete commands".to_string(),
                Self::UnfinishedCompleteCommand(_, _) => "unfinished complete command".to_string(),
                Self::UnfinishedList(_, _) => "unfinished list".to_string(),
                Self::UnfinishedAndOrList(_, _) => "unfinished and or list".to_string(),
                Self::UnfinishedPipeline(_, _) => "unfinished pipeline".to_string(),
                Self::UnfinishedPipeSequence(_, _) => "unfinished pipe sequence".to_string(),
                Self::UnfinishedCommand(_) => "unfinished command".to_string(),
                Self::UnfinishedSimpleCommand(_) => "unfinished simple command".to_string(),
                Self::UnfinishedWord(_) => "unfinished word".to_string(),
            }
        )
    }
}

impl std::error::Error for ParseError {}
