use std::io::Write;

use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style;
use crossterm::terminal;
use posh_core::engine::parser::{parse, SyntaxTree};
use posh_core::{Engine, Result};

use self::syntax_highlighting::Highlighter;

use super::RawMode;
use crate::config::{Colors, ABBREVIATIONS};

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

    let ast = parse(&state.line, true)?;
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

mod syntax_highlighting {
    use posh_core::ast::AndOrList;
    use posh_core::ast::CmdPrefix;
    use posh_core::ast::CmdSuffix;
    use posh_core::ast::Command;
    use posh_core::ast::CompleteCommand;
    use posh_core::ast::List;
    use posh_core::ast::LogicalOp;
    use posh_core::ast::Pipeline;
    use posh_core::ast::Redirection;
    use posh_core::ast::Separator;
    use posh_core::ast::SimpleCommand;
    use posh_core::ast::VariableAssignment;
    use posh_core::ast::Word;

    use self::Colors;
    use super::*;

    use crossterm::style::{Print, ResetColor, SetForegroundColor};

    pub trait Highlighter {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()>;
    }

    impl Highlighter for SyntaxTree {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            for cmd in &self.program {
                cmd.write_highlighted(engine)?;
            }

            queue!(
                engine.writer,
                SetForegroundColor(Colors::UNPARSED),
                Print(&self.unparsed),
                ResetColor
            )?;

            engine.writer.flush()?;

            Ok(())
        }
    }

    impl Highlighter for CompleteCommand {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            self.list.write_highlighted(engine)?;

            if let Some(separator) = &self.separator {
                separator.write_highlighted(engine)?;
            }

            if let Some((ws, comment)) = &self.comment {
                queue!(
                    engine.writer,
                    SetForegroundColor(Colors::COMMENT_COLOR),
                    Print(ws),
                    Print("#"),
                    Print(comment),
                    ResetColor
                )?;
            }
            Ok(())
        }
    }

    impl Highlighter for List {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            self.first.write_highlighted(engine)?;

            for (sep, and_or_list) in &self.rest {
                sep.write_highlighted(engine)?;
                and_or_list.write_highlighted(engine)?;
            }

            Ok(())
        }
    }

    impl Highlighter for AndOrList {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            self.first.write_highlighted(engine)?;

            for (op, pipeline) in &self.rest {
                op.write_highlighted(engine)?;
                pipeline.write_highlighted(engine)?;
            }

            Ok(())
        }
    }

    impl Highlighter for Pipeline {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            if let Some(ws) = &self.bang {
                queue!(
                    engine.writer,
                    SetForegroundColor(Colors::BANG_COLOR),
                    Print(format!("{ws}!")),
                    ResetColor
                )?;
            }

            self.first.write_highlighted(engine)?;

            for (ws, cmd) in &self.rest {
                queue!(
                    engine.writer,
                    SetForegroundColor(Colors::PIPE_COLOR),
                    Print(format!("{ws}|")),
                    ResetColor
                )?;
                cmd.write_highlighted(engine)?;
            }

            Ok(())
        }
    }

    impl Highlighter for Command {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            match self {
                Command::Simple(cmd) => cmd.write_highlighted(engine),
                Command::Compound(_) => todo!(),
                Command::FunctionDefinition(_) => todo!(),
            }
        }
    }

    impl Highlighter for SimpleCommand {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            let cmd_color = match &self.name {
                Some(Word { name, .. })
                    if engine.has_command(name)
                        || engine.has_builtin(name)
                        || super::has_abbreviation(name) =>
                {
                    Colors::VALID_CMD_COLOR
                }
                _ => Colors::INVALID_CMD_COLOR,
            };

            for prefix in &self.prefixes {
                prefix.write_highlighted(engine)?;
            }

            if let Some(name) = &self.name {
                queue!(
                    engine.writer,
                    SetForegroundColor(cmd_color),
                    Print(name.to_string()),
                    ResetColor
                )?;
            }

            for suffix in &self.suffixes {
                suffix.write_highlighted(engine)?;
            }

            Ok(())
        }
    }

    impl Highlighter for CmdPrefix {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            match self {
                Self::Redirection(r) => r.write_highlighted(engine),
                Self::Assignment(a) => a.write_highlighted(engine),
            }
        }
    }

    impl Highlighter for CmdSuffix {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            match self {
                Self::Word(w) => Ok(queue!(
                    engine.writer,
                    SetForegroundColor(Colors::TRAILING_WORD_COLOR),
                    Print(w.to_string()),
                    ResetColor
                )?),
                Self::Redirection(r) => r.write_highlighted(engine),
            }
        }
    }

    impl Highlighter for Redirection {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            match self {
                Redirection::Input {
                    file_descriptor,
                    target,
                } => Ok(queue!(
                    engine.writer,
                    SetForegroundColor(Colors::REDIRECTION_FD_COLOR),
                    Print(file_descriptor.to_string()),
                    SetForegroundColor(Colors::REDIRECTION_OP_COLOR),
                    Print("<"),
                    SetForegroundColor(Colors::REDIRECTION_TARGET_COLOR),
                    Print(target.to_string()),
                    ResetColor,
                )?),
                Redirection::Output {
                    file_descriptor,
                    append,
                    target,
                } => Ok(queue!(
                    engine.writer,
                    SetForegroundColor(Colors::REDIRECTION_FD_COLOR),
                    Print(file_descriptor.to_string()),
                    SetForegroundColor(Colors::REDIRECTION_OP_COLOR),
                    Print(if *append { ">>" } else { ">" }),
                    SetForegroundColor(Colors::REDIRECTION_TARGET_COLOR),
                    Print(target.to_string()),
                    ResetColor,
                )?),
                Redirection::HereDocument {
                    file_descriptor,
                    delimiter,
                } => Ok(queue!(
                    engine.writer,
                    SetForegroundColor(Colors::REDIRECTION_FD_COLOR),
                    Print(file_descriptor.to_string()),
                    SetForegroundColor(Colors::REDIRECTION_OP_COLOR),
                    Print("<<"),
                    SetForegroundColor(Colors::REDIRECTION_TARGET_COLOR),
                    Print(delimiter.to_string()),
                    ResetColor,
                )?),
            }
        }
    }

    impl Highlighter for VariableAssignment {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            Ok(queue!(
                engine.writer,
                SetForegroundColor(Colors::ASSIGNMENT_LHS_COLOR),
                Print(self.lhs.to_string()),
                SetForegroundColor(Colors::ASSIGNMENT_OP_COLOR),
                Print("="),
                SetForegroundColor(Colors::ASSIGNMENT_RHS_COLOR),
                Print(match &self.rhs {
                    Some(rhs) => rhs.to_string(),
                    None => "".to_string(),
                }),
                ResetColor,
            )?)
        }
    }

    impl Highlighter for Separator {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            Ok(match self {
                Separator::Sync(ws) => queue!(
                    engine.writer,
                    SetForegroundColor(Colors::SEPARATOR_COLOR),
                    Print(format!("{ws};")),
                    ResetColor
                ),
                Separator::Async(ws) => queue!(
                    engine.writer,
                    SetForegroundColor(Colors::SEPARATOR_COLOR),
                    Print(format!("{ws}&")),
                    ResetColor
                ),
            }?)
        }
    }

    impl Highlighter for LogicalOp {
        fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
            Ok(queue!(
                engine.writer,
                SetForegroundColor(Colors::LOGICAL_OP_COLOR),
                Print(self.to_string()),
                ResetColor
            )?)
        }
    }
}
