mod syntax_highlighting;

use std::collections::HashMap;

use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style;
use crossterm::terminal;

use psh_core::ast::prelude::Word;
use psh_core::engine::expand::Expand;
use psh_core::{parse, Engine, Error, Result};

use crate::config::Colors;
use crate::repl::input::syntax_highlighting::Highlighter;
use crate::repl::RawMode;

use self::syntax_highlighting::Context;

pub fn read_full_command(engine: &mut Engine) -> Result<String> {
    let _raw = RawMode::init()?;

    prompt(engine, false)?;

    let start_pos = cursor::position()?;
    let mut line = read_line(engine, true, start_pos, None)?;

    'outer: while let Err(Error::Incomplete(_)) = parse(&line, false) {
        line.push('\n');

        prompt(engine, true)?;
        match read_line(engine, false, start_pos, Some(&line)) {
            Ok(l) => line += &l,
            Err(Error::CancelledLine) => {
                line = String::new();
                break 'outer;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(line)
}

fn prompt(engine: &mut Engine, ps2: bool) -> Result<()> {
    let prompt = if ps2 {
        engine.get_value_of("PS2").unwrap()
    } else {
        engine.get_value_of("PS1").unwrap()
    };

    let word = Word::new(&prompt, "");
    let word = word.expand(engine);

    queue!(
        engine.writer,
        cursor::MoveToColumn(0),
        style::SetForegroundColor(Colors::PROMPT),
        style::Print(word.to_string()),
        style::ResetColor,
    )?;

    Ok(())
}

struct State {
    /// The current content of the input line.
    line: String,

    /// The current position the user is on the line.
    index: usize,

    /// The initial position of the terminal grid (start of the line, visually).
    start_pos: (u16, u16),

    /// The size of the terminal window.
    size: (u16, u16),

    /// Will be `true` when the user inputs Enter, ^C, etc.
    about_to_exit: bool,

    /// Will be `true` if the user has just entered ^C.
    cancelled: bool,

    /// Will be `true` if the user just cleared the screen via ^L.
    cleared: bool,

    /// Will be `false` if the user inputs '^ ', which will make abbreviations not expand.
    expand_abbreviations: bool,
}

impl State {
    fn pos(&self) -> Result<(u16, u16)> {
        Ok(cursor::position()?)
    }

    fn next_pos(&self) -> cursor::MoveTo {
        let (sx, sy) = self.start_pos;

        let (cx, _) = self.pos().unwrap_or((sx, sy));
        let (width, _) = self.size;

        let mut x = sx + self.index as u16;
        let mut y = sy;

        if cx == width {
            x = sx;
            y = sy + 1;
        }

        cursor::MoveTo(x, y)
    }
}

fn read_line(
    engine: &mut Engine,
    ps1: bool,
    start_pos: (u16, u16),
    old_line: Option<&String>,
) -> Result<String> {
    let _raw = RawMode::init()?;

    let mut state = State {
        line: Default::default(),
        index: 0,
        start_pos: cursor::position()?,
        size: terminal::size()?,
        about_to_exit: false,
        cancelled: false,
        cleared: false,
        expand_abbreviations: true,
    };

    while !state.about_to_exit {
        write_highlighted_ast(engine, &state, start_pos, old_line)?;

        execute!(engine.writer, event::EnableBracketedPaste)?;

        let event = event::read()?;

        if let Event::Paste(s) = &event {
            state.line.insert_str(state.index, s);
            state.index += s.len();

            execute!(
                engine.writer,
                style::Print(&state.line[state.index - 1..]),
                state.next_pos(),
            )?;

            write_highlighted_ast(engine, &state, start_pos, old_line)?;
        }

        execute!(engine.writer, event::DisableBracketedPaste)?;

        let (code, modifiers) = match event {
            Event::Key(KeyEvent {
                code, modifiers, ..
            }) => (code, modifiers),
            _ => continue,
        };

        match (code, modifiers) {
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                if ps1 && state.line.is_empty() {
                    continue;
                }

                state.about_to_exit = true;
                state.cancelled = true;
            }

            (KeyCode::Enter, _) => {
                if state.expand_abbreviations {
                    if let Some((expanded_line, diff)) =
                        expand_abbreviation(&engine.abbreviations, &state.line)
                    {
                        state.line = expanded_line;
                        state.index = state.index.wrapping_add_signed(diff);
                    }
                }
                state.about_to_exit = true;
            }

            (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                if ps1 && state.line.is_empty() {
                    state.about_to_exit = true;
                    state.line = "exit".to_string();
                }
            }

            (KeyCode::Up, _) | (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                state.line = engine.history.prev()?.cloned().unwrap_or_default();
                state.index = state.line.len();

                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Down, _) | (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                state.line = engine.history.next()?.cloned().unwrap_or_default();
                state.index = state.line.len();

                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
                state.line.clear();
                state.index = 0;
                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Char('w'), KeyModifiers::CONTROL) => {
                if state.index == 0 {
                    continue;
                }

                let mut space_index = None;
                for i in (0..state.index).rev() {
                    if let Some(' ') = state.line.chars().nth(i) {
                        space_index = Some(i);
                        break;
                    }
                }

                if let Some(' ') = state.line.chars().nth(state.index - 1) {
                    // FIXME: this should find the previous space
                    space_index = Some(0);
                }

                let space_index = space_index.unwrap_or(0);
                state.line.replace_range(space_index..state.index, "");
                state.index = space_index;

                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Char('l'), KeyModifiers::CONTROL) => {
                let (start_x, _) = state.start_pos;
                execute!(
                    engine.writer,
                    cursor::MoveTo(start_x, 0),
                    terminal::Clear(terminal::ClearType::FromCursorDown),
                )?;
                state.cleared = true;
                break;
            }

            (KeyCode::Left, _) | (KeyCode::Char('b'), KeyModifiers::CONTROL) if state.index > 0 => {
                state.index -= 1;

                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Right, _) | (KeyCode::Char('f'), KeyModifiers::CONTROL)
                if state.index < state.line.len() =>
            {
                state.index += 1;
                execute!(engine.writer, state.next_pos())?;
            }

            (KeyCode::Char(' '), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                state.line.insert(state.index, ' ');
                state.index += 1;

                if state.expand_abbreviations {
                    if let Some((expanded_line, diff)) =
                        expand_abbreviation(&engine.abbreviations, &state.line)
                    {
                        state.line = expanded_line;
                        state.index = state.index.wrapping_add_signed(diff);
                    }
                }

                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&state.line[state.index - 1..]),
                    state.next_pos(),
                )?;
            }

            (KeyCode::Char(' '), KeyModifiers::CONTROL) => {
                state.line.insert(state.index, ' ');
                state.index += 1;
                state.expand_abbreviations = false;

                execute!(
                    engine.writer,
                    terminal::Clear(terminal::ClearType::UntilNewLine),
                    style::Print(&state.line[state.index - 1..]),
                    state.next_pos(),
                )?;
            }

            (KeyCode::Char(c), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                state.line.insert(state.index, c);
                state.index += 1;
                state.expand_abbreviations = c != '|' && c != '&' && c != ';';

                execute!(
                    engine.writer,
                    style::Print(&state.line[state.index - 1..]),
                    state.next_pos(),
                )?;
            }

            (KeyCode::Backspace, _) if state.index > 0 => {
                state.index -= 1;
                state.line.remove(state.index);
                state.expand_abbreviations = true;

                execute!(
                    engine.writer,
                    state.next_pos(),
                    style::Print(&state.line[state.index..]),
                    state.next_pos(),
                )?;
            }

            _ => {}
        }

        if state.about_to_exit {
            break;
        }
    }

    let (_, start_y) = state.start_pos;
    let (_, height) = state.size;
    let next_y = start_y + 1;
    if next_y >= height {
        queue!(engine.writer, terminal::ScrollUp(height - start_y))?;
    }

    if state.cleared {
        execute!(
            engine.writer,
            terminal::Clear(terminal::ClearType::All),
            cursor::MoveTo(0, 0),
        )?;
    } else if !state.line.is_empty() {
        execute!(engine.writer, cursor::MoveTo(0, next_y))?;
    } else {
        execute!(engine.writer, cursor::MoveToRow(next_y))?;
    }

    match (state.cancelled, ps1) {
        (true, false) => Err(Error::CancelledLine),
        (true, true) => Ok("".to_string()),
        (false, _) => Ok(state.line),
    }
}

fn write_highlighted_ast(
    engine: &mut Engine,
    state: &State,
    start_pos: (u16, u16),
    old_line: Option<&String>,
) -> Result<()> {
    let (start_x, start_y) = start_pos;
    let (x, y) = state.pos()?;

    queue!(
        engine.writer,
        cursor::MoveTo(start_x, start_y),
        terminal::Clear(terminal::ClearType::UntilNewLine),
        style::SetForegroundColor(Colors::REDIRECT_INPUT),
    )?;

    let line = if let Some(l) = old_line {
        format!("{l}{}", state.line)
    } else {
        state.line.clone()
    };

    let starting_point = match std::env::var("PS2") {
        Ok(ps2) => ps2.len() as u16,
        _ => start_x,
    };

    let Ok(ast) = psh_core::parse(line, true) else { return Ok(()); };
    ast.write_highlighted(
        engine,
        Context {
            start_x: starting_point,
            abbreviations: state.expand_abbreviations,
        },
    )?;

    if state.cancelled {
        queue!(engine.writer, style::ResetColor, style::Print("^C"))?;
    }

    execute!(engine.writer, style::ResetColor, cursor::MoveTo(x, y))?;

    Ok(())
}

fn expand_abbreviation<S: AsRef<str>>(
    abbreviations: &HashMap<String, String>,
    line: S,
) -> Option<(String, isize)> {
    let line = line.as_ref();
    let mut iter = line.split(' ');
    match iter.next() {
        Some(part) => match abbreviations.get(part) {
            Some(exp) => {
                let diff = exp.len() as isize - part.len() as isize;
                Some((line.replacen(part, exp, 1), diff))
            }
            None => None,
        },
        None => None,
    }
}
