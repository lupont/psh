use std::env;
use std::error;
use std::fmt;
use std::io;
use std::path::PathBuf;

use crate::ast::nodes::*;

pub type Result<T> = std::result::Result<T, Error>;

pub type ParseResult<T> = std::result::Result<T, ParseError<T>>;

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
    ParseError(String),
    CancelledLine,
    Incomplete(String),
    Nix(nix::Error),
    Var(env::VarError),
    NonExistentFile(String),

    #[cfg(feature = "serde")]
    Json(serde_json::Error),
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
                Self::Nix(e) => format!("errno: {e}"),
                Self::Var(e) => e.to_string(),
                Self::NonExistentFile(file) => format!("{file}: no such file"),

                #[cfg(feature = "serde")]
                Self::Json(e) => e.to_string(),
            }
        )
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<nix::Error> for Error {
    fn from(e: nix::Error) -> Self {
        Self::Nix(e)
    }
}

impl<T: fmt::Debug> From<ParseError<T>> for Error {
    fn from(e: ParseError<T>) -> Self {
        Self::ParseError(e.to_string())
    }
}

#[cfg(feature = "serde")]
impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Self::Json(e)
    }
}

impl From<env::VarError> for Error {
    fn from(e: env::VarError) -> Self {
        Self::Var(e)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<T: fmt::Debug> {
    InvalidName(String),
    None,
    Unimplemented(String),
    Unfinished(Option<LeadingWhitespace>, T),
    InvalidSyntaxInCmdSub, //(SyntaxTree),
}

impl<T: fmt::Debug> ParseError<T> {
    /// Casts self to the given type variant.
    ///
    /// # Panics
    ///
    /// This will panic if self is of the `Unfinished` variant.
    /// To support casting this variant, see the `ParseError::cast_with` method.
    pub fn force_cast<U>(self) -> ParseError<U>
    where
        U: fmt::Debug,
    {
        self.cast_with(|_| panic!("tried to force cast a generic variant"))
    }

    /// Casts self to the given type variant. If
    /// self is of `Unfinished` variant, the supplied
    /// function will be used to transform the type
    /// into the new one.
    pub fn cast_with<F, U>(self, f: F) -> ParseError<U>
    where
        U: fmt::Debug,
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Unfinished(ws, t) => ParseError::Unfinished(ws, f(t)),

            Self::InvalidName(name) => ParseError::InvalidName(name),
            Self::None => ParseError::None,
            Self::Unimplemented(thing) => ParseError::Unimplemented(thing),
            Self::InvalidSyntaxInCmdSub => ParseError::InvalidSyntaxInCmdSub,
        }
    }
}

impl<T: fmt::Debug> fmt::Display for ParseError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidName(name) => format!("`{name}` is not a valid name"),
                Self::None => "could not parse this here".to_string(),
                Self::Unimplemented(s) => format!("not yet implemented: {s}"),
                Self::Unfinished(_ws, node) => format!("unfinished {node:?}"),
                Self::InvalidSyntaxInCmdSub => "invalid syntax in command substitution".to_string(),
            }
        )
    }
}

impl<T: fmt::Debug> error::Error for ParseError<T> {}
