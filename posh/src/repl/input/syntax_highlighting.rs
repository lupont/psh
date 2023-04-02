use std::io::Write;

use crossterm::queue;
use crossterm::style::{Print, ResetColor, SetForegroundColor};

use posh_core::ast::prelude::*;
use posh_core::{Engine, Result};

use crate::repl::Colors;

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
            Command::Compound(_, _) => todo!(),
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
            Print(&self.whitespace),
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
