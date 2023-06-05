use std::env;
use std::fs;
use std::os::unix::prelude::PermissionsExt;
use std::path::PathBuf;

use crate::Error;

pub fn home_dir() -> String {
    env::var("HOME").map_err(|_| Error::NoHome).unwrap()
}

pub fn history_file() -> PathBuf {
    match env::var("PSH_HISTORY") {
        Ok(path) => PathBuf::from(path),
        Err(_) => match env::var("XDG_CONFIG_HOME") {
            Ok(config_home) => PathBuf::from(config_home).join("psh").join("history"),
            Err(_) => PathBuf::from(home_dir())
                .join(".config")
                .join("psh")
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

pub fn has_relative_command(cmd: impl AsRef<str>) -> bool {
    let cmd = cmd.as_ref();

    if !cmd.starts_with('/') && !cmd.starts_with('.') {
        return false;
    }

    match std::fs::metadata(cmd) {
        Ok(metadata) => {
            let mode = metadata.permissions().mode();
            !metadata.is_dir() && mode & 0o111 != 0
        }

        Err(_) => false,
    }
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

pub fn system_has_user(loginname: &str) -> bool {
    std::process::Command::new("id")
        .arg(loginname)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .expect("`id` command not found on system")
        .code()
        .expect("`id` command terminated by signal")
        == 0
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
