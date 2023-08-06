use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: alias [ -h | --help ] [ <key>=<val> | <key> ]

Define or query existing aliases.

alias -h         print this text
alias            print the current aliases
alias key        print the alias with key `key`
alias key=val    define alias from `key` to `val`";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.contains(&"-h") || args.contains(&"--help") => {
            println!("{}", HELP);
            Ok(ExitStatus::from_code(0))
        }

        [] => {
            for (key, val) in &engine.aliases {
                println!("alias {}=\"{}\"", key, val.replace('"', "\\\""));
            }
            Ok(ExitStatus::from_code(0))
        }

        &[expr] => {
            if let Some((lhs, rhs)) = expr.split_once('=') {
                engine.aliases.insert(lhs.to_string(), rhs.to_string());
                Ok(ExitStatus::from_code(0))
            } else if let Some(val) = engine.aliases.get(expr) {
                println!("alias {}=\"{}\"", expr, val.replace('"', "\\\""));
                Ok(ExitStatus::from_code(0))
            } else {
                eprintln!("alias: {} not found", expr);
                Ok(ExitStatus::from_code(1))
            }
        }

        _ => {
            eprintln!("alias: Too many arguments");
            Ok(ExitStatus::from_code(1))
        }
    }
}
