use std::env;
use std::path::PathBuf;

use crate::path;
use crate::{Engine, ExitStatus, Result};

pub fn execute(_: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    let path = match args {
        [] => PathBuf::from(path::home_dir()),

        ["-"] => {
            if let Ok(old_pwd) = env::var("OLDPWD") {
                PathBuf::from(old_pwd)
            } else {
                eprintln!("cd: No previous directory.");
                return Ok(ExitStatus::from_code(1));
            }
        }

        [dir] if PathBuf::from(dir).is_dir() => PathBuf::from(dir),

        [dir] if PathBuf::from(dir).exists() => {
            eprintln!("cd: '{}' is not a directory.", dir);
            return Ok(ExitStatus::from_code(3));
        }

        [dir] => {
            eprintln!("cd: '{}' does not exist.", dir);
            return Ok(ExitStatus::from_code(2));
        }

        _ => {
            eprintln!("cd: Too many arguments");
            return Ok(ExitStatus::from_code(1));
        }
    };

    env::set_var("OLDPWD", env::current_dir()?);
    env::set_current_dir(path)?;
    env::set_var("PWD", env::current_dir()?);
    Ok(ExitStatus::from_code(0))
}
