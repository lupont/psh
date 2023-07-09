use std::io::Write;

use crate::{Engine, ExitStatus, Result};

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    let code = match args {
        [code] => {
            if let Ok(code) = code.parse::<i32>() {
                code
            } else {
                writeln!(engine.writer, "invalid integer: '{}'", code)?;
                return Ok(ExitStatus::from_code(1));
            }
        }
        _ => 0,
    };

    std::process::exit(code);
}
