use std::env;
use std::io::Write;
use std::path::PathBuf;

use crate::path;
use crate::{Engine, ExitStatus, Result};

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    let path = match args {
        [] => PathBuf::from(path::home_dir()),

        ["-"] => {
            if let Ok(old_pwd) = env::var("OLDPWD") {
                PathBuf::from(old_pwd)
            } else {
                writeln!(engine.writer, "cd: No previous directory.")?;
                return Ok(ExitStatus::from_code(1));
            }
        }

        [dir] if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

        [dir] if PathBuf::from(dir).exists() => {
            writeln!(engine.writer, "cd: '{}' is not a directory.", dir)?;
            return Ok(ExitStatus::from_code(3));
        }

        [dir] => {
            writeln!(engine.writer, "cd: '{}' does not exist.", dir)?;
            return Ok(ExitStatus::from_code(2));
        }

        _ => {
            writeln!(engine.writer, "cd: Too many arguments")?;
            return Ok(ExitStatus::from_code(1));
        }
    };

    env::set_var("OLDPWD", env::current_dir()?);
    env::set_current_dir(path)?;
    Ok(ExitStatus::from_code(0))
}
