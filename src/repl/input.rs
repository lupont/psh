use std::io::Write;

use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style::{self, Color};
use crossterm::terminal;

use crate::config::{Colors, ABBREVIATIONS};
use crate::engine::parser::lexer::tokenize;
use crate::engine::parser::Token;
use crate::{Engine, Result};

use super::RawMode;

pub fn read_line<W: Write>(engine: &mut Engine<W>) -> Result<String> {
    let _raw = RawMode::init()?;

    let mut line = String::new();
    let mut index = 0;

    let (start_x, start_y) = cursor::position()?;
    let (_width, height) = terminal::size()?;

    let mut about_to_exit = false;
    let mut cancelled = false;
    let mut cleared = false;

    let mut highlight_abbreviations = true;

    while !about_to_exit {
        let (code, modifiers) = match event::read()? {
            Event::Key(KeyEvent {
                code, modifiers, ..
            }) => (code, modifiers),
            _ => break,
        };

        match (code, modifiers) {
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                if line.is_empty() {
                    continue;
                }

                about_to_exit = true;
                cancelled = true;
            }

            (KeyCode::Enter, _) => {
                if let Some((expanded_line, _)) = expand_abbreviation(&line, true) {
                    line = expanded_line;
                }
                about_to_exit = true;
            }

            (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                if !line.is_empty() {
                    continue;
                }

                execute!(engine.writer, style::Print("\n\r"))?;

                // FIXME: better control flow than this
                std::process::exit(0);
            }

            (KeyCode::Up, _) | (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                line = engine.history.prev()?.cloned().unwrap_or_default();
                index = line.len();

                execute!(
                    engine.writer,
                    cursor::MoveTo(start_x + index as u16, start_y)
                )?;
            }

            (KeyCode::Down, _) | (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                line = engine.history.next()?.cloned().unwrap_or_default();
                index = line.len();

                execute!(
                    engine.writer,
                    cursor::MoveTo(start_x + index as u16, start_y)
                )?;
            }

            (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
                line.clear();
                index = 0;
                execute!(engine.writer, cursor::MoveTo(start_x, start_y))?;
            }

            (KeyCode::Char('w'), KeyModifiers::CONTROL) => {
                if index == 0 {
                    continue;
                }

                let mut space_index = None;
                for i in (0..index).rev() {
                    if let Some(' ') = line.chars().nth(i) {
                        space_index = Some(i);
                        break;
                    }
                }

                if let Some(' ') = line.chars().nth(index - 1) {
                    // FIXME: this should find the previous space
                    space_index = Some(0);
                }

                let space_index = space_index.unwrap_or(0);
                let offset = (index - space_index) as u16;
                line.replace_range(space_index..index, "");
                index = space_index;

                if engine.has_abbreviation(&line) {
                    highlight_abbreviations = true;
                }

                execute!(engine.writer, cursor::MoveLeft(offset))?;
            }

            (KeyCode::Char('l'), KeyModifiers::CONTROL) => {
                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::All),
                    cursor::MoveTo(0, 0)
                )?;
                cleared = true;
                break;
            }

            (KeyCode::Left, _) | (KeyCode::Char('b'), KeyModifiers::CONTROL) if index > 0 => {
                index -= 1;
                execute!(engine.writer, cursor::MoveLeft(1))?;
            }

            (KeyCode::Right, _) | (KeyCode::Char('f'), KeyModifiers::CONTROL)
                if index < line.len() =>
            {
                index += 1;
                execute!(engine.writer, cursor::MoveRight(1))?;
            }

            (KeyCode::Char(' '), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                let (mut x, y) = cursor::position()?;

                if line.find(' ').is_none() {
                    if let Some((expanded_line, diff)) = expand_abbreviation(&line, false) {
                        line = expanded_line;

                        // FIXME: replace with something like `wrapping_add_signed` once
                        //        https://github.com/rust-lang/rust/issues/87840 is in stable
                        if diff >= 0 {
                            x = u16::checked_add(x, diff as u16).unwrap_or(0);
                            index = usize::checked_add(index, diff as usize).unwrap_or(0);
                        } else {
                            x = u16::checked_sub(x, diff.unsigned_abs() as u16).unwrap_or(0);
                            index = usize::checked_sub(index, diff.unsigned_abs()).unwrap_or(0);
                        }
                    }
                }

                line.insert(index, ' ');
                index += 1;

                if engine.has_abbreviation(&line) {
                    highlight_abbreviations = true;
                }

                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&line[index - 1..]),
                    cursor::MoveTo(x + 1, y),
                )?;
            }

            (KeyCode::Char(' '), KeyModifiers::CONTROL) => {
                let (x, y) = cursor::position()?;

                line.insert(index, ' ');
                index += 1;

                highlight_abbreviations = false;

                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&line[index - 1..]),
                    cursor::MoveTo(x + 1, y),
                )?;
            }

            (KeyCode::Char(c), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                let (x, y) = cursor::position()?;

                line.insert(index, c);
                index += 1;

                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&line[index - 1..]),
                    cursor::MoveTo(x + 1, y),
                )?;
            }

            (KeyCode::Backspace, _) if index > 0 => {
                let (x, y) = cursor::position()?;

                index -= 1;
                line.remove(index);

                if engine.has_abbreviation(&line) {
                    highlight_abbreviations = true;
                }

                execute!(
                    engine.writer,
                    cursor::MoveTo(x - 1, y),
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&line[index..]),
                    cursor::MoveTo(x - 1, y),
                )?;
            }

            _ => {}
        }

        let (x, y) = cursor::position()?;
        print(
            engine,
            &line,
            (start_x, start_y),
            (x, y),
            about_to_exit,
            highlight_abbreviations,
        )?;

        if about_to_exit {
            break;
        }
    }

    write!(engine.writer, "\r")?;
    if start_y + 1 >= height {
        execute!(engine.writer, terminal::ScrollUp(1))?;
    }

    if !cleared {
        execute!(engine.writer, cursor::MoveTo(0, start_y + 1))?;
    } else {
        execute!(engine.writer, cursor::MoveTo(0, 0))?;
    }

    // FIXME: should probably return Result<Option<String>> with Ok(None) here?
    if cancelled {
        Ok("".to_string())
    } else {
        Ok(line)
    }
}

fn print<W: Write>(
    engine: &mut Engine<W>,
    line: &String,
    start_pos: (u16, u16),
    pos: (u16, u16),
    about_to_exit: bool,
    highlight_abbreviations: bool,
) -> Result<()> {
    let tokens = tokenize(line, true);

    let mut prev_non_space_token = None;

    let (start_x, start_y) = start_pos;
    let (x, y) = pos;

    queue!(
        engine.writer,
        cursor::MoveTo(start_x, start_y),
        terminal::Clear(terminal::ClearType::UntilNewLine)
    )?;

    for token in &tokens {
        match token {
            Token::Space => queue!(engine.writer, style::Print(" "))?,

            str_token @ (Token::String(s)
            | Token::SingleQuotedString(s)
            | Token::DoubleQuotedString(s)) => {
                match prev_non_space_token {
                    Some(&Token::Pipe | &Token::Semicolon) | None => {
                        let color = if engine.has_builtin(s) {
                            Colors::VALID_BUILTIN
                        } else if engine.has_command(s) {
                            Colors::VALID_CMD
                        } else if engine.has_abbreviation(s) && highlight_abbreviations {
                            Colors::VALID_ABBR
                        } else {
                            Colors::INVALID_CMD
                        };
                        queue!(engine.writer, style::SetForegroundColor(color))?;
                    }

                    _ => match str_token {
                        Token::String(_) => queue!(engine.writer, style::ResetColor)?,
                        Token::SingleQuotedString(_) => {
                            queue!(engine.writer, style::SetForegroundColor(Color::Blue))?
                        }
                        Token::DoubleQuotedString(_) => {
                            queue!(engine.writer, style::SetForegroundColor(Color::DarkGreen))?
                        }
                        _ => unreachable!(),
                    },
                }

                match str_token {
                    Token::String(s) => queue!(engine.writer, style::Print(s))?,
                    Token::SingleQuotedString(_) => {
                        queue!(engine.writer, style::Print(format!("'{s}'")))?
                    }
                    Token::DoubleQuotedString(_) => {
                        queue!(engine.writer, style::Print(format!("\"{s}\"")))?
                    }
                    _ => unreachable!(),
                }
            }

            Token::Pipe => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Yellow),
                style::Print("|")
            )?,

            Token::Semicolon => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Yellow),
                style::Print(";")
            )?,

            Token::RedirectOutput(None, to) => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print(format!(">{to}"))
            )?,

            Token::RedirectOutput(Some(from), to) => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print(format!("{from}>{to}"))
            )?,

            Token::RedirectInput(to) => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print(format!("<{to}"))
            )?,

            Token::And => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print("&&")
            )?,

            Token::Or => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print("||")
            )?,

            Token::Colon => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print(":")
            )?,

            Token::Ampersand => queue!(
                engine.writer,
                style::SetForegroundColor(Color::Red),
                style::Print("&")
            )?,
        }

        if !matches!(&token, &Token::Space) {
            prev_non_space_token = Some(token);
        }
    }
    if about_to_exit {
        queue!(engine.writer, style::ResetColor, style::Print("^C"))?;
    }
    queue!(engine.writer, style::ResetColor, cursor::MoveTo(x, y))?;

    engine.writer.flush()?;

    // execute!(
    //     engine.writer,
    //     cursor::MoveTo(start_x, start_y),
    //     terminal::Clear(terminal::ClearType::UntilNewLine),
    //     style::SetForegroundColor(highlight_color),
    //     style::Print(&line[..highlight_until_index]),
    //     style::ResetColor,
    //     style::Print(&line[highlight_until_index..]),
    //     cursor::MoveTo(x, y),
    // )?;
    Ok(())
}

fn expand_abbreviation<S: AsRef<str>>(line: S, only_if_equal: bool) -> Option<(String, isize)> {
    let line = line.as_ref();
    for (a, b) in ABBREVIATIONS {
        if line == a || (!only_if_equal && line.starts_with(&format!("{a} "))) {
            let diff = b.len() as isize - a.len() as isize;
            return Some((line.replacen(a, b, 1), diff));
        }
    }
    None
}
