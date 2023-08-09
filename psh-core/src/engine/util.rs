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
