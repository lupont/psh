mod abbr;
mod alias;
mod builtins;
mod cd;
mod colon;
mod exit;
mod unabbr;
mod unalias;

use crate::{Engine, Error, ExitStatus, Result};

type Builtin = fn(&mut Engine, &[&str]) -> Result<ExitStatus>;

pub(crate) const BUILTINS: &[(&str, Builtin)] = &[
    (":", colon::execute),
    ("abbr", abbr::execute),
    ("alias", alias::execute),
    ("builtins", builtins::execute),
    ("cd", cd::execute),
    ("exit", exit::execute),
    ("unabbr", unabbr::execute),
    ("unalias", unalias::execute),
];

fn get(builtin: &str) -> Option<Builtin> {
    for (name, exe) in BUILTINS {
        if name == &builtin {
            return Some(*exe);
        }
    }
    None
}

pub fn execute(engine: &mut Engine, command: &str, args: &[&str]) -> Result<ExitStatus> {
    match get(command) {
        Some(builtin) => builtin(engine, args),
        None => Err(Error::UnknownBuiltin(command.to_string())),
    }
}

pub fn has(s: &str) -> bool {
    get(s).is_some()
}
