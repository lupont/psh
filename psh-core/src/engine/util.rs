use std::env;
use std::os::unix::prelude::PermissionsExt;

use nix::unistd::{fork, ForkResult, Pid};

use crate::{Error, Result};

pub fn spawn_subshell<F>(child_fn: F) -> Result<Pid>
where
    F: FnOnce() -> Result<()>,
{
    match unsafe { fork() } {
        Ok(ForkResult::Parent { child }) => Ok(child),
        Ok(ForkResult::Child) => {
            child_fn()?;
            std::process::exit(0);
        }
        Err(e) => Err(Error::Nix(e)),
    }
}

pub fn is_executable(path: &str) -> bool {
    match std::fs::metadata(path) {
        Ok(metadata) => {
            let mode = metadata.permissions().mode();
            !metadata.is_dir() && mode & 0o111 != 0
        }

        Err(_) => false,
    }
}

pub fn update_shlvl() {
    if let Ok(var) = env::var("SHLVL") {
        if let Ok(shlvl) = var.parse::<u8>() {
            let shlvl = shlvl + 1;
            env::set_var("SHLVL", shlvl.to_string());
        }
    } else {
        env::set_var("SHLVL", "1");
    }
}

pub fn is_root() -> bool {
    nix::unistd::getuid().is_root()
}
