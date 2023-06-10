use std::io::Write;

use crossterm::queue;
use crossterm::style::{Print, ResetColor, SetForegroundColor};

use psh_core::ast::prelude::*;
use psh_core::{Engine, Result};

use crate::repl::Colors;

pub trait Highlighter {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()>;
}

impl Highlighter for SyntaxTree {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.leading.write_highlighted(engine)?;

        if let Some((cmds, linebreak)) = &self.commands {
            cmds.write_highlighted(engine)?;
            linebreak.write_highlighted(engine)?;
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

impl Highlighter for CompleteCommands {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.head.write_highlighted(engine)?;

        for (newlines, cmd) in &self.tail {
            newlines.write_highlighted(engine)?;
            cmd.write_highlighted(engine)?;
        }

        Ok(())
    }
}

impl Highlighter for CompleteCommand {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        match self {
            Self::List(list, None, None) => {
                list.write_highlighted(engine)?;
            }
            Self::List(list, Some(separator_op), None) => {
                list.write_highlighted(engine)?;
                separator_op.write_highlighted(engine)?;
            }
            Self::List(list, None, Some(comment)) => {
                list.write_highlighted(engine)?;
                comment.write_highlighted(engine)?;
            }
            Self::List(list, Some(separator_op), Some(comment)) => {
                list.write_highlighted(engine)?;
                separator_op.write_highlighted(engine)?;
                comment.write_highlighted(engine)?;
            }
            Self::Comment(comment) => {
                comment.write_highlighted(engine)?;
            }
        }

        Ok(())
    }
}

impl Highlighter for List {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.head.write_highlighted(engine)?;

        for (sep, and_or_list) in &self.tail {
            sep.write_highlighted(engine)?;
            and_or_list.write_highlighted(engine)?;
        }

        Ok(())
    }
}

impl Highlighter for AndOrList {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.head.write_highlighted(engine)?;

        for (op, linebreak, pipeline) in &self.tail {
            op.write_highlighted(engine)?;
            linebreak.write_highlighted(engine)?;
            pipeline.write_highlighted(engine)?;
        }

        Ok(())
    }
}

impl Highlighter for Pipeline {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        if let Some(bang) = &self.bang {
            bang.write_highlighted(engine)?;
        }

        self.sequence.write_highlighted(engine)?;

        Ok(())
    }
}

impl Highlighter for PipeSequence {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.head.write_highlighted(engine)?;
        for (pipe, linebreak, cmd) in &self.tail {
            pipe.write_highlighted(engine)?;
            linebreak.write_highlighted(engine)?;
            cmd.write_highlighted(engine)?;
        }
        Ok(())
    }
}

impl Highlighter for Command {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        match self {
            Command::Simple(cmd) => cmd.write_highlighted(engine),
            Command::Compound(cmd, redirections) => {
                cmd.write_highlighted(engine)?;
                for redirection in redirections {
                    redirection.write_highlighted(engine)?;
                }
                Ok(())
            }
            Command::FunctionDefinition(func_def) => func_def.write_highlighted(engine),
        }
    }
}

impl Highlighter for CompoundCommand {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        match self {
            CompoundCommand::Brace(brace_group) => brace_group.write_highlighted(engine),
            CompoundCommand::Subshell(_) => todo!(),
            CompoundCommand::For(_) => todo!(),
            CompoundCommand::Case(_) => todo!(),
            CompoundCommand::If(_) => todo!(),
            CompoundCommand::While(_) => todo!(),
            CompoundCommand::Until(_) => todo!(),
        }
    }
}

impl Highlighter for CompoundList {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.linebreak.write_highlighted(engine)?;
        self.term.write_highlighted(engine)?;
        if let Some(separator) = &self.separator {
            separator.write_highlighted(engine)?;
        }
        Ok(())
    }
}

impl Highlighter for Term {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.head.write_highlighted(engine)?;
        for (sep, and_or) in &self.tail {
            sep.write_highlighted(engine)?;
            and_or.write_highlighted(engine)?;
        }
        Ok(())
    }
}

impl Highlighter for FunctionDefinition {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.name.write_highlighted(engine)?;
        queue!(
            engine.writer,
            SetForegroundColor(Colors::FUNC_DEF_PAREN_COLOR),
            Print(&self.parens),
            ResetColor
        )?;
        self.linebreak.write_highlighted(engine)?;
        self.body.write_highlighted(engine)?;

        Ok(())
    }
}

impl Highlighter for FunctionBody {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        self.command.write_highlighted(engine)?;
        for redirection in &self.redirections {
            redirection.write_highlighted(engine)?;
        }
        Ok(())
    }
}

impl Highlighter for BraceGroup {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::BRACE_GROUP_COLOR),
            Print(&self.lbrace_ws),
            Print('{'),
            ResetColor
        )?;
        self.body.write_highlighted(engine)?;
        queue!(
            engine.writer,
            SetForegroundColor(Colors::BRACE_GROUP_COLOR),
            Print(&self.rbrace_ws),
            Print('}'),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for SimpleCommand {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        let cmd_color = match &self.name {
            Some(word @ Word { name, .. })
                if engine.has_executable(word) || super::has_abbreviation(name) =>
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
            Redirection::File {
                whitespace,
                input_fd,
                ty,
                target,
            } => Ok(queue!(
                engine.writer,
                Print(whitespace),
                SetForegroundColor(Colors::REDIRECTION_FD_COLOR),
                Print(if let Some(fd) = input_fd {
                    fd.to_string()
                } else {
                    String::new()
                }),
                SetForegroundColor(Colors::REDIRECTION_OP_COLOR),
                Print(ty.to_string()),
                SetForegroundColor(Colors::REDIRECTION_TARGET_COLOR),
                Print(target.to_string()),
                ResetColor,
            )?),
            Redirection::Here {
                whitespace,
                input_fd,
                ty,
                end,
                content,
            } => Ok(queue!(
                engine.writer,
                Print(whitespace),
                SetForegroundColor(Colors::REDIRECTION_FD_COLOR),
                Print(if let Some(fd) = input_fd {
                    fd.to_string()
                } else {
                    String::new()
                }),
                SetForegroundColor(Colors::REDIRECTION_OP_COLOR),
                Print(ty.to_string()),
                SetForegroundColor(Colors::REDIRECTION_TARGET_COLOR),
                Print(end.to_string()),
                ResetColor,
                Print(content.to_string()),
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

impl Highlighter for NewlineList {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(engine.writer, Print(&self.whitespace))?;
        Ok(())
    }
}

impl Highlighter for Linebreak {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        if let Some(newlines) = &self.newlines {
            newlines.write_highlighted(engine)?;
        }
        Ok(())
    }
}

impl Highlighter for SeparatorOp {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::SEPARATOR_COLOR),
            Print(self.to_string()),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for Separator {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        match self {
            Separator::Explicit(op, linebreak) => {
                op.write_highlighted(engine)?;
                linebreak.write_highlighted(engine)
            }
            Separator::Implicit(newlines) => newlines.write_highlighted(engine),
        }
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

impl Highlighter for Name {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(engine.writer, Print(self.to_string()))?;
        Ok(())
    }
}

impl Highlighter for Bang {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::BANG_COLOR),
            Print(self.to_string()),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for Comment {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::COMMENT_COLOR),
            Print(self.to_string()),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for Pipe {
    fn write_highlighted(&self, engine: &mut Engine<impl Write>) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::PIPE_COLOR),
            Print(self.to_string()),
            ResetColor,
        )?;
        Ok(())
    }
}