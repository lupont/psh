use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: unalias [ -h | --help ] <key>

Erase an existing alias.

unalias -h   print this text
unalias key  remove the alias with key `key`";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.is_empty() || args.contains(&"-h") || args.contains(&"--help") => {
            println!("{}", HELP);
            Ok(ExitStatus::from_code(0))
        }

        &[key] => {
            if engine.aliases.contains_key(key) {
                engine.aliases.remove(key);
                Ok(ExitStatus::from_code(0))
            } else {
                eprintln!("unalias: {} not found", key);
                Ok(ExitStatus::from_code(1))
            }
        }

        _ => {
            eprintln!("unalias: Too many arguments");
            Ok(ExitStatus::from_code(1))
        }
    }
}
