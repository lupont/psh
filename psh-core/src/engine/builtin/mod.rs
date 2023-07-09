mod abbr;
mod cd;
mod colon;
mod exit;
mod unabbr;

use crate::ast::prelude::Word;
use crate::{Engine, Error, ExitStatus, Result};

use super::expand::remove_quotes;

type Builtin = fn(&mut Engine, &[&str]) -> Result<ExitStatus>;

fn get(builtin: &str) -> Option<Builtin> {
    match builtin {
        ":" => Some(colon::execute),
        "abbr" => Some(abbr::execute),
        "cd" => Some(cd::execute),
        "exit" => Some(exit::execute),
        "unabbr" => Some(unabbr::execute),
        _ => None,
    }
}

pub fn execute(engine: &mut Engine, command: &str, args: &[&str]) -> Result<ExitStatus> {
    match get(command) {
        Some(builtin) => builtin(engine, args),
        None => Err(Error::UnknownBuiltin(command.to_string())),
    }
}

pub fn has(s: &Word) -> bool {
    let name = remove_quotes(&s.name);
    get(&name).is_some()
}
