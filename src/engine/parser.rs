use std::io::Write;

use crate::path;
use crate::{Engine, Result};

pub struct Line {
    pub cmd: String,
    pub raw_args: Vec<String>,
    // options, etc. in the future
}

impl Line {
    pub fn parse<W: Write>(engine: &mut Engine<W>, line: String) -> Result<Option<Self>> {
        if line.is_empty() {
            return Ok(None);
        }

        let home = path::home_dir()?;

        engine.history.append(&line)?;

        match line.find(' ') {
            Some(_) => {
                let (cmd, args) = line.split_once(' ').unwrap();
                let args = args
                    .split_ascii_whitespace()
                    .map(|s| s.replace('~', &home))
                    .collect::<Vec<_>>();
                Ok(Some(Self {
                    cmd: cmd.to_string(),
                    raw_args: args,
                }))
            }

            _ => Ok(Some(Self {
                cmd: line,
                raw_args: Default::default(),
            })),
        }
    }

    pub fn raw_args(&self) -> &[String] {
        &self.raw_args
    }
}
