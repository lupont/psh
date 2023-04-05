use std::fmt;
use std::io;
use std::path::PathBuf;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    NoHome,
    InvalidHistfile(PathBuf),
    HistoryOutOfBounds,
    UnknownCommand(String),
    Unimplemented(String),
    SyntaxError(String),
    ParseError(ParseError),
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
                Self::UnknownCommand(cmd) => format!("unknown command: {}", cmd),
                Self::Unimplemented(s) => s.to_string(),
                Self::SyntaxError(s) => format!("syntax error: {s}"),
                Self::ParseError(e) => e.to_string(),
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

#[derive(Debug)]
pub enum ParseError {
    InvalidName(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidName(name) => format!("`{name}` is not a valid name"),
            }
        )
    }
}

impl std::error::Error for ParseError {}
