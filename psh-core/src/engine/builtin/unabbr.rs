use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: unabbr [ -h | --help ] <key>

Erase an existing abbreviation.

unabbr -h   print this text
unabbr key  remove the abbreviation with key `key`";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.is_empty() || args.contains(&"-h") || args.contains(&"--help") => {
            println!("{}", HELP);
            Ok(ExitStatus::from_code(0))
        }

        &[key] => {
            if engine.abbreviations.contains_key(key) {
                engine.abbreviations.remove(key);
                Ok(ExitStatus::from_code(0))
            } else {
                eprintln!("unabbr: {} not found", key);
                Ok(ExitStatus::from_code(1))
            }
        }

        _ => {
            eprintln!("unabbr: Too many arguments");
            Ok(ExitStatus::from_code(1))
        }
    }
}
