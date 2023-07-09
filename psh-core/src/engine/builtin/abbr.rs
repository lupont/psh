use std::io::Write;

use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: abbr [ -h | --help ] [ <key>=<val> | <key> ]

Define or query existing abbreviations.

abbr -h         print this text
abbr            print the current abbreviations
abbr key        print the abbreviation with key `key`
abbr key=val    define `key` to expand to `val`";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
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

        _ => {
            writeln!(engine.writer, "abbr: Too many arguments")?;
            Ok(ExitStatus::from_code(1))
        }
    }
}
