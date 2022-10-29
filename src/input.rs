use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::style;
use crossterm::terminal;
use std::io::Write;

use crate::Result;
use crate::config::Colors;

pub(crate) fn read_line<W: Write>(stdout: &mut W, cmds: &[String], builtins: &[String]) -> Result<String> {
    terminal::enable_raw_mode()?;
    let line = sys::read_line(stdout, cmds, builtins);
    terminal::disable_raw_mode()?;
    line
}

mod sys {
    use super::*;

    pub(super) fn read_line<W: Write>(stdout: &mut W, cmds: &[String], builtins: &[String]) -> Result<String> {
        let mut line = String::new();
        let mut index = 0;

        let (start_x, start_y) = cursor::position()?;

        while let Event::Key(KeyEvent {
            code, modifiers, ..
        }) = event::read()?
        {
            match code {
                KeyCode::Char('c')
                    if modifiers.contains(KeyModifiers::CONTROL) && !line.is_empty() =>
                {
                    line.clear();
                    write!(stdout, "\r")?;
                    let (_, y) = cursor::position()?;
                    execute!(stdout, cursor::MoveTo(0, y + 1))?;
                    break;
                }
                KeyCode::Enter => {
                    write!(stdout, "\r")?;
                    let (_, y) = cursor::position()?;
                    execute!(stdout, cursor::MoveTo(0, y + 1))?;
                    break;
                }

                KeyCode::Char('l') if modifiers.contains(KeyModifiers::CONTROL) => {
                    execute!(
                        stdout,
                        terminal::Clear(terminal::ClearType::All),
                        cursor::MoveTo(0, 0)
                    )?;
                    break;
                }

                KeyCode::Char('b') if modifiers.contains(KeyModifiers::CONTROL) && index > 0 => {
                    index -= 1;
                    execute!(stdout, cursor::MoveLeft(1))?;
                }

                KeyCode::Left if index > 0 => {
                    index -= 1;
                    execute!(stdout, cursor::MoveLeft(1))?;
                }

                KeyCode::Char('f')
                    if modifiers.contains(KeyModifiers::CONTROL) && index < line.len() =>
                {
                    index += 1;
                    execute!(stdout, cursor::MoveRight(1))?;
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

            let highlight_until_index = line.find(' ').unwrap_or(line.len());
            let cmd = &line[..highlight_until_index];
            let cmd_exists = cmds.iter().any(|s| s.ends_with(&format!("/{}", cmd)));
            let builtin_exists = builtins.iter().any(|s| s == cmd.trim());
            let (x, y) = cursor::position()?;

            let highlight_color = if builtin_exists {
                Colors::VALID_BUILTIN
            } else if cmd_exists {
                Colors::VALID_CMD
            } else {
                Colors::INVALID_CMD
            };

            execute!(
                stdout,
                cursor::MoveTo(start_x, start_y),
                terminal::Clear(terminal::ClearType::UntilNewLine),
                style::SetForegroundColor(highlight_color),
                style::Print(&line[..highlight_until_index]),
                style::SetForegroundColor(style::Color::Reset),
                style::Print(&line[highlight_until_index..]),
                cursor::MoveTo(x, y)
            )?;
        }
        Ok(line)
    }
}
