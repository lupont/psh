mod cd;
mod colon;
mod exit;

use std::io::Write;

pub use cd::cd;
pub use colon::colon;
pub use exit::exit;

use crate::ast::prelude::Word;
use crate::{Engine, Error, ExitStatus, Result};

use super::expand::remove_quotes;

pub const BUILTINS: &[&str] = &[":", "cd", "exit"];

pub fn execute(
    engine: &mut Engine<impl Write>,
    command: &str,
    args: &[&str],
) -> Result<ExitStatus> {
    match command {
        ":" => colon::colon(),
        "cd" => cd::cd(engine, args),
        "exit" => exit::exit(engine, args),
        c => Err(Error::UnknownBuiltin(c.to_string())),
    }
}

pub fn has(s: &Word) -> bool {
    let name = remove_quotes(&s.name);
    let has = |&s| name == s || name.starts_with(&format!("{s} "));
    for builtin in BUILTINS {
        if has(builtin) {
            return true;
        }
    }
    false
}
