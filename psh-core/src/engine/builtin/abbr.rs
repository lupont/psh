use std::io::Write;

use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: abbr [ <key>=<val> | [-e|--erase] <key> ]

Define, query, or erase existing abbreviations.

`abbr`         prints the current abbreviations
`abbr key`     prints the abbreviation with key `key`
`abbr key=val` defines `key` to expand to `val`
`abbr -e key`  erases the abbreviation with key `key`";

pub fn abbr(engine: &mut Engine<impl Write>, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.contains(&"-h") || args.contains(&"--help") => {
            writeln!(engine.writer, "{}", HELP)?;
            Ok(ExitStatus::from_code(0))
        }

        [] => {
            for (key, val) in &engine.abbreviations {
                writeln!(
                    engine.writer,
                    "abbr {}=\"{}\"",
                    key,
                    val.replace('"', "\\\"")
                )?;
            }
            Ok(ExitStatus::from_code(0))
        }

        &[expr] => {
            if let Some((lhs, rhs)) = expr.split_once('=') {
                engine
                    .abbreviations
                    .insert(lhs.to_string(), rhs.to_string());
                Ok(ExitStatus::from_code(0))
            } else if let Some(val) = engine.abbreviations.get(expr) {
                writeln!(
                    engine.writer,
                    "abbr {}=\"{}\"",
                    expr,
                    val.replace('"', "\\\"")
                )?;
                Ok(ExitStatus::from_code(0))
            } else {
                writeln!(engine.writer, "abbr: {} not found", expr)?;
                Ok(ExitStatus::from_code(1))
            }
        }

        &["-e" | "--erase", key] => {
            if engine.abbreviations.contains_key(key) {
                engine.abbreviations.remove(key);
                Ok(ExitStatus::from_code(0))
            } else {
                writeln!(engine.writer, "abbr: {} not found", key)?;
                Ok(ExitStatus::from_code(1))
            }
        }

        _ => {
            writeln!(engine.writer, "abbr: Too many arguments")?;
            Ok(ExitStatus::from_code(1))
        }
    }
}
