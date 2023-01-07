use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::path::history_file;
use crate::{Error, Result};

pub trait History {
    fn prev(&mut self) -> Result<Option<&String>>;
    fn next(&mut self) -> Result<Option<&String>>;
    fn read(&mut self) -> Result<Option<&String>>;
    fn read_lines(&mut self) -> Result<Vec<String>>;
    fn append(&mut self, line: &str) -> Result<()>;
    fn reload(&mut self) -> Result<()>;
    fn clear(&mut self) -> Result<()>;
}

pub struct FileHistory {
    pub path: PathBuf,
    lines: Vec<String>,
    cursor: usize,
}

impl FileHistory {
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
}

impl History for FileHistory {
    fn clear(&mut self) -> Result<()> {
        fs::OpenOptions::new()
            .write(true)
            .open(&self.path)?
            .set_len(0)?;
        self.reload()
    }

    fn reload(&mut self) -> Result<()> {
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

    fn append(&mut self, line: &str) -> Result<()> {
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

    fn read_lines(&mut self) -> Result<Vec<String>> {
        self.reload()?;

        let prev_cursor = self.cursor;
        self.cursor = 0;

        let mut vec = Vec::with_capacity(self.lines.len());

        if let Ok(Some(line)) = self.read() {
            vec.push(line.clone());
        }

        while let Ok(Some(line)) = self.next() {
            vec.push(line.clone());
        }

        self.cursor = prev_cursor;

        Ok(vec)
    }

    fn read(&mut self) -> Result<Option<&String>> {
        self.reload()?;

        if self.cursor >= self.lines.len() {
            return Ok(None);
        }

        match self.lines.get(self.cursor) {
            Some(line) => Ok(Some(line)),
            None => Err(Error::HistoryOutOfBounds),
        }
    }

    fn prev(&mut self) -> Result<Option<&String>> {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
        self.read()
    }

    fn next(&mut self) -> Result<Option<&String>> {
        if self.cursor < self.lines.len() {
            self.cursor += 1;
        }
        self.read()
    }
}

pub struct FileHistoryIntoIterator {
    history: FileHistory,
    index: usize,
}

impl Iterator for FileHistoryIntoIterator {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.history.lines.len() - 1 {
            return None;
        }

        let entry = self.history.lines.swap_remove(self.index);
        self.index += 1;
        Some(entry)
    }
}

impl IntoIterator for FileHistory {
    type Item = String;

    type IntoIter = FileHistoryIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            history: self,
            index: 0,
        }
    }
}

pub struct DummyHistory;

impl History for DummyHistory {
    fn prev(&mut self) -> Result<Option<&String>> {
        Ok(None)
    }

    fn next(&mut self) -> Result<Option<&String>> {
        Ok(None)
    }

    fn read(&mut self) -> Result<Option<&String>> {
        Ok(None)
    }

    fn read_lines(&mut self) -> Result<Vec<String>> {
        Ok(vec![])
    }

    fn append(&mut self, _line: &str) -> Result<()> {
        Ok(())
    }

    fn reload(&mut self) -> Result<()> {
        Ok(())
    }

    fn clear(&mut self) -> Result<()> {
        Ok(())
    }
}
