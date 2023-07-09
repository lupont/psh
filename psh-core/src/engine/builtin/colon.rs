use crate::{Engine, ExitStatus, Result};

pub fn execute(_: &mut Engine, _: &[&str]) -> Result<ExitStatus> {
    Ok(ExitStatus::from_code(0))
}
