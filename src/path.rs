use std::env;
use std::fs;

use crate::Error;

pub fn home_dir() -> String {
    env::var("HOME").map_err(|_| Error::NoHome).unwrap()
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
