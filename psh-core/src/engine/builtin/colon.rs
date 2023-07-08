use crate::{ExitStatus, Result};

pub fn colon() -> Result<ExitStatus> {
    Ok(ExitStatus::from_code(0))
}
