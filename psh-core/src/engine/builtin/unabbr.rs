use std::io::Write;

use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: unabbr [ -h | --help ] <key>

Erase an existing abbreviation.

unabbr -h   print this text
unabbr key  remove the abbreviation with key `key`";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.is_empty() || args.contains(&"-h") || args.contains(&"--help") => {
            writeln!(engine.writer, "{}", HELP)?;
            Ok(ExitStatus::from_code(0))
        }

        &[key] => {
            if engine.abbreviations.contains_key(key) {
                engine.abbreviations.remove(key);
                Ok(ExitStatus::from_code(0))
            } else {
                writeln!(engine.writer, "unabbr: {} not found", key)?;
                Ok(ExitStatus::from_code(1))
            }
        }

        _ => {
            writeln!(engine.writer, "unabbr: Too many arguments")?;
            Ok(ExitStatus::from_code(1))
        }
    }
}
