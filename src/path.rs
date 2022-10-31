use std::env;
use std::fs;
use std::path::PathBuf;

use crate::{Error, Result};

pub(crate) fn home_dir() -> Result<String> {
    env::var("HOME").map_err(|_| Error::NoHome)
}

pub(crate) fn hist_file() -> Result<PathBuf> {
    match env::var("RUSH_HISTFILE") {
        Ok(path) => {
            let path = PathBuf::from(path);
            if path.exists() && !path.is_dir() {
                Ok(path)
            } else {
                Err(Error::InvalidHistfile(path))
            }
        }
        Err(_) => Ok(PathBuf::from(home_dir()?).join(".rush_history")),
    }
}

pub(crate) fn get_cmds_from_path() -> Vec<String> {
    let raw_path = env::var("PATH").unwrap();
    let raw_path = raw_path.split(':');

    let mut cmds = Vec::new();

    for path in raw_path {
        if let Ok(dirs) = fs::read_dir(path) {
            cmds.extend(dirs.map(|d| format!("{}", d.unwrap().path().display())));
        }
    }

    cmds
}
