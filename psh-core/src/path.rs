use std::env;
use std::path::PathBuf;

use crate::engine::util;
use crate::Error;

pub fn home_dir() -> String {
    env::var("HOME").map_err(|_| Error::NoHome).unwrap()
}

fn cfg_file(file_name: &str, var: &str) -> PathBuf {
    if let Ok(path) = env::var(var) {
        return PathBuf::from(path);
    }

    match env::var("XDG_CONFIG_HOME") {
        Ok(cfg_home) => PathBuf::from(cfg_home).join("psh").join(file_name),
        Err(_) => PathBuf::from(home_dir())
            .join(".config")
            .join("psh")
            .join(file_name),
    }
}

pub fn init_file() -> PathBuf {
    cfg_file("init.psh", "PSH_INIT")
}

pub fn history_file() -> PathBuf {
    cfg_file("history", "PSH_HISTORY")
}

pub fn has_relative_command(cmd: impl AsRef<str>) -> bool {
    let cmd = cmd.as_ref();
    cmd.contains('/') && util::is_executable(cmd)
}

pub fn compress_tilde(s: String) -> String {
    let home = home_dir();
    s.replacen(&home, "~", 1)
}

pub fn is_portable_filename(input: impl AsRef<str>) -> bool {
    input
        .as_ref()
        .chars()
        .all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '.' | '-' | '_'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expand_works() {
        let home = home_dir();

        let input = format!("{home}/foo");
        let expanded = compress_tilde(input);
        assert_eq!("~/foo", expanded);

        let input = format!("{home}");
        let expanded = compress_tilde(input);
        assert_eq!("~", expanded);

        let input = format!("{home}//");
        let expanded = compress_tilde(input);
        assert_eq!("~//", expanded);
    }
}
