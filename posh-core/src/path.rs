use std::env;
use std::fs;
use std::path::PathBuf;

use crate::Error;

pub fn home_dir() -> String {
    env::var("HOME").map_err(|_| Error::NoHome).unwrap()
}

pub fn history_file() -> PathBuf {
    match env::var("POSH_HISTORY") {
        Ok(path) => PathBuf::from(path),
        Err(_) => match env::var("XDG_CONFIG_HOME") {
            Ok(config_home) => PathBuf::from(config_home).join("posh").join("history"),
            Err(_) => PathBuf::from(home_dir())
                .join(".config")
                .join("posh")
                .join("history"),
        },
    }
}

pub fn get_cmds_from_path() -> Vec<String> {
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

pub trait Expand: Sized {
    fn expand(self) -> Self;
}

impl Expand for String {
    fn expand(self) -> Self {
        let home = home_dir();
        self.replacen(&home, "~", 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expand_works() {
        let home = home_dir();

        let input = format!("{home}/foo");
        let expanded = input.expand();
        assert_eq!("~/foo", expanded);

        let input = format!("{home}");
        let expanded = input.expand();
        assert_eq!("~", expanded);

        let input = format!("{home}//");
        let expanded = input.expand();
        assert_eq!("~//", expanded);
    }
}
