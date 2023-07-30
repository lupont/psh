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
