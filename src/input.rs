use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::style;
use crossterm::terminal;
use std::io::Write;

use crate::Result;

pub(crate) fn read_line<W: Write>(stdout: &mut W) -> Result<String> {
    terminal::enable_raw_mode()?;
    let line = sys::read_line(stdout);
    terminal::disable_raw_mode()?;
    line
}

mod sys {
    use super::*;

    pub(super) fn read_line<W: Write>(stdout: &mut W) -> Result<String> {
        let mut line = String::new();
        let mut index = 0;

        while let Event::Key(KeyEvent {
            code, modifiers, ..
        }) = event::read()?
        {
            match code {
                KeyCode::Char('c')
                    if modifiers.contains(KeyModifiers::CONTROL) && !line.is_empty() =>
                {
                    write!(stdout, "\r")?;
                    let (_, y) = cursor::position()?;
                    execute!(stdout, cursor::MoveTo(0, y + 1))?;
                    line.clear();
                    break;
                }
                KeyCode::Enter => {
                    write!(stdout, "\r")?;
                    let (_, y) = cursor::position()?;
                    execute!(stdout, cursor::MoveTo(0, y + 1))?;
                    break;
                }

                KeyCode::Left if index > 0 => {
                    index -= 1;
                    execute!(stdout, cursor::MoveLeft(1))?;
                }

                KeyCode::Right if index < line.len() => {
                    index += 1;
                    execute!(stdout, cursor::MoveRight(1))?;
                }

                KeyCode::Char(c) => {
                    let (x, y) = cursor::position()?;

                    line.insert(index, c);
                    index += 1;

                    execute!(
                        stdout,
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index - 1..]),
                        cursor::MoveTo(x + 1, y),
                    )?;
                }

                KeyCode::Backspace if index > 0 => {
                    let (x, y) = cursor::position()?;

                    index -= 1;
                    line.remove(index);

                    execute!(
                        stdout,
                        cursor::MoveTo(x - 1, y),
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index..]),
                        cursor::MoveTo(x - 1, y),
                    )?;
                }

                _ => {}
            }
        }
        Ok(line)
    }
}
