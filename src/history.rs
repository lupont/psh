use std::env;
use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::{Error, Result};

pub struct History {
    path: PathBuf,
    lines: Vec<String>,
    cursor: usize,
}

impl History {
    pub fn init() -> Result<Self> {
        let path = match env::var("RUSH_HISTORY") {
            Ok(path) => PathBuf::from(path),
            Err(_) => match env::var("HOME") {
                Ok(home) => PathBuf::from(home)
                    .join(".config")
                    .join("rush")
                    .join("history"),
                Err(_) => return Err(Error::NoHome),
            },
        };

        if path.metadata().is_err() {
            // FIXME
            fs::create_dir_all(path.parent().expect("could not get parent of history file"))?;
        }

        // To read the amount of lines, and create the file if it does not yet exist.
        let mut file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&path)?;

        let mut buffer = String::new();
        file.read_to_string(&mut buffer)?;
        let lines = buffer
            .trim()
            .split('\n')
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        Ok(Self {
            path,
            cursor: lines.len(),
            lines,
        })
    }

    pub fn reload(&mut self) -> Result<()> {
        if !self.path.exists() {
            self.lines = Default::default();
        } else {
            let contents = std::fs::read_to_string(&self.path)?;
            self.lines = contents
                .trim()
                .split('\n')
                .map(ToString::to_string)
                .collect();
        }
        Ok(())
    }

    pub fn append(&mut self, line: &str) -> Result<()> {
        self.reload()?;

        self.lines.push(line.to_string());
        self.cursor = self.lines.len();

        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&self.path)?;
        file.write_all(self.lines.join("\n").as_bytes())?;
        file.write_all(b"\n")?;

        Ok(())
    }

    pub fn read(&mut self) -> Result<Option<&String>> {
        self.reload()?;

        if self.cursor >= self.lines.len() {
            return Ok(None);
        }

        match self.lines.get(self.cursor) {
            Some(line) => Ok(Some(line)),
            None => Err(Error::HistoryOutOfBounds),
        }
    }

    pub fn prev(&mut self) -> Result<Option<&String>> {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
        self.read()
    }

    pub fn next(&mut self) -> Result<Option<&String>> {
        if self.cursor < self.lines.len() {
            self.cursor += 1;
        }
        self.read()
    }
}
