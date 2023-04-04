mod syntax_highlighting;

use std::io::Write;

use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style;
use crossterm::terminal;

use posh_core::{Engine, Result};

use crate::config::{Colors, ABBREVIATIONS};
use crate::repl::input::syntax_highlighting::Highlighter;
use crate::repl::RawMode;

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
    highlight_abbreviations: bool,
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

pub fn read_line<W: Write>(engine: &mut Engine<W>) -> Result<String> {
    let _raw = RawMode::init()?;

    let mut state = State {
        line: String::new(),
        index: 0,
        start_pos: cursor::position()?,
        size: terminal::size()?,
        about_to_exit: false,
        cancelled: false,
        cleared: false,
        highlight_abbreviations: true,
    };

    while !state.about_to_exit {
        print(engine, &state)?;

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

            print(engine, &state)?;
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
                if state.line.is_empty() {
                    continue;
                }

                state.about_to_exit = true;
                state.cancelled = true;
            }

            (KeyCode::Enter, _) => {
                if let Some((expanded_line, _)) = expand_abbreviation(&state.line, true) {
                    state.line = expanded_line;
                }
                state.about_to_exit = true;
            }

            (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                if state.line.is_empty() {
                    state.about_to_exit = true;
                    state.line = "exit".to_string();
                }
            }

            (KeyCode::Up, _) | (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                state.line = engine.history.prev()?.cloned().unwrap_or_default();
                state.index = state.line.len();

                execute!(engine.writer, state.next_pos(),)?;
            }

            (KeyCode::Down, _) | (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                state.line = engine.history.next()?.cloned().unwrap_or_default();
                state.index = state.line.len();

                execute!(engine.writer, state.next_pos(),)?;
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

                if has_abbreviation(&state.line) {
                    state.highlight_abbreviations = true;
                }

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

            (KeyCode::Char(c @ (' ' | '|' | ';')), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                state.line.insert(state.index, c);
                state.index += 1;

                if has_abbreviation(&state.line) {
                    state.highlight_abbreviations = true;
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

                state.highlight_abbreviations = false;

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

                execute!(
                    engine.writer,
                    style::Print(&state.line[state.index - 1..]),
                    state.next_pos(),
                )?;
            }

            (KeyCode::Backspace, _) if state.index > 0 => {
                state.index -= 1;
                state.line.remove(state.index);

                if has_abbreviation(&state.line) {
                    state.highlight_abbreviations = true;
                }

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

    write!(engine.writer, "\r")?;
    let (_, start_y) = state.start_pos;
    let (_, height) = state.size;
    if start_y + 1 >= height {
        execute!(engine.writer, terminal::ScrollUp(1))?;
    }

    if state.cleared {
        execute!(
            engine.writer,
            terminal::Clear(terminal::ClearType::All),
            cursor::MoveTo(0, 0)
        )?;
    } else {
        execute!(engine.writer, cursor::MoveTo(0, start_y + 1))?;
    }

    // FIXME: should probably return Result<Option<String>> with Ok(None) here?
    if state.cancelled {
        Ok("".to_string())
    } else {
        Ok(state.line)
    }
}

fn print<W: Write>(engine: &mut Engine<W>, state: &State) -> Result<()> {
    let (start_x, start_y) = state.start_pos;
    let (x, y) = state.pos()?;

    queue!(
        engine.writer,
        cursor::MoveTo(start_x, start_y),
        terminal::Clear(terminal::ClearType::UntilNewLine),
        style::SetForegroundColor(Colors::REDIRECT_INPUT),
    )?;

    let ast = posh_core::parse(&state.line, true)?;
    ast.write_highlighted(engine)?;
    engine.writer.flush()?;

    if state.cancelled {
        queue!(engine.writer, style::ResetColor, style::Print("^C"))?;
    }

    execute!(engine.writer, style::ResetColor, cursor::MoveTo(x, y))?;

    Ok(())
}

pub fn has_abbreviation(cmd: impl AsRef<str>) -> bool {
    let cmd = cmd.as_ref();
    ABBREVIATIONS.iter().any(|&(a, _)| a == cmd)
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
