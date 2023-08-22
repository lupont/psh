use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::path::history_file;
use crate::{Error, Result};

pub struct History {
    pub path: PathBuf,
    lines: Vec<String>,
    cursor: usize,
}

impl History {
    pub fn init() -> Result<Self> {
        let path = history_file();

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

    pub fn clear(&mut self) -> Result<()> {
        fs::OpenOptions::new()
            .write(true)
            .open(&self.path)?
            .set_len(0)?;
        self.reload()
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

    pub fn read_lines(&mut self) -> Result<Vec<String>> {
        self.reload()?;

        let prev_cursor = self.cursor;
        self.cursor = 0;

        let mut vec = Vec::with_capacity(self.lines.len());

        if let Ok(line) = self.read() {
            vec.push(line.clone());
        }

        while let Ok(line) = self.next_entry() {
            vec.push(line.clone());
        }

        self.cursor = prev_cursor;

        Ok(vec)
    }

    pub fn read(&mut self) -> Result<&String> {
        self.reload()?;

        match self.lines.get(self.cursor) {
            Some(line) => Ok(line),
            None => Err(Error::HistoryOutOfBounds),
        }
    }

    pub fn prev_entry(&mut self) -> Result<&String> {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
        self.read()
    }

    // TODO: clippy complained when this was called `next`,
    //       and said to implement Iterator instead. Do that!
    pub fn next_entry(&mut self) -> Result<&String> {
        if self.cursor < self.lines.len() {
            self.cursor += 1;
        }
        self.read()
    }
}
