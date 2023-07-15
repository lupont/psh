use std::io::Write;

use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: builtins [ -h | --help ] [cmd]

Print or query the available builtins.

builtins -h     print this text
builtins cmd    returns with 0 if `cmd` is a builtin, otherwise 1
builtins        print all available builtins";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        &["-h" | "--help"] => {
            writeln!(engine.writer, "{}", HELP)?;
            Ok(ExitStatus::from_code(0))
        }

        &[arg] => {
            let mut rc = 1;
            for (name, _) in super::BUILTINS {
                if name == &arg {
                    rc = 0;
                    break;
                }
            }
            Ok(ExitStatus::from_code(rc))
        }

        [] => {
            for (name, _) in super::BUILTINS {
                writeln!(engine.writer, "{}", name)?;
            }
            Ok(ExitStatus::from_code(0))
        }

        _ => {
            writeln!(engine.writer, "builtins: Too many arguments")?;
            Ok(ExitStatus::from_code(1))
        }
    }
}
