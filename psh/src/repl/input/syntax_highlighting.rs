use std::io::Write;

use crossterm::cursor::{MoveDown, MoveToColumn};
use crossterm::queue;
use crossterm::style::{Print, ResetColor, SetForegroundColor};
use crossterm::terminal::{Clear, ClearType};

use psh_core::ast::prelude::*;
use psh_core::{Engine, Result};

use crate::repl::Colors;

#[derive(Debug, Clone, Copy)]
pub struct Context {
    pub start_x: u16,
    pub abbreviations: bool,
}

pub trait Highlighter {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()>;
}

impl Highlighter for SyntaxTree {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.leading.write_highlighted(engine, context)?;

        if let Some((cmds, linebreak)) = &self.commands {
            cmds.write_highlighted(engine, context)?;
            linebreak.write_highlighted(engine, context)?;
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
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.head.write_highlighted(engine, context)?;

        for (newlines, cmd) in &self.tail {
            newlines.write_highlighted(engine, context)?;
            cmd.write_highlighted(engine, context)?;
        }

        Ok(())
    }
}

impl Highlighter for CompleteCommand {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            Self::List {
                list,
                separator_op: None,
                comment: None,
            } => {
                list.write_highlighted(engine, context)?;
            }
            Self::List {
                list,
                separator_op: Some(separator_op),
                comment: None,
            } => {
                list.write_highlighted(engine, context)?;
                separator_op.write_highlighted(engine, context)?;
            }
            Self::List {
                list,
                separator_op: None,
                comment: Some(comment),
            } => {
                list.write_highlighted(engine, context)?;
                comment.write_highlighted(engine, context)?;
            }
            Self::List {
                list,
                separator_op: Some(separator_op),
                comment: Some(comment),
            } => {
                list.write_highlighted(engine, context)?;
                separator_op.write_highlighted(engine, context)?;
                comment.write_highlighted(engine, context)?;
            }
            Self::Comment { comment } => {
                comment.write_highlighted(engine, context)?;
            }
        }

        Ok(())
    }
}

impl Highlighter for List {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.head.write_highlighted(engine, context)?;

        for (sep, and_or_list) in &self.tail {
            sep.write_highlighted(engine, context)?;
            and_or_list.write_highlighted(engine, context)?;
        }

        Ok(())
    }
}

impl Highlighter for AndOrList {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.head.write_highlighted(engine, context)?;

        for (op, linebreak, pipeline) in &self.tail {
            op.write_highlighted(engine, context)?;
            linebreak.write_highlighted(engine, context)?;
            pipeline.write_highlighted(engine, context)?;
        }

        Ok(())
    }
}

impl Highlighter for Pipeline {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        if let Some(bang) = &self.bang {
            bang.write_highlighted(engine, context)?;
        }

        self.sequence.write_highlighted(engine, context)?;

        Ok(())
    }
}

impl Highlighter for PipeSequence {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.head.write_highlighted(engine, context)?;
        for (pipe, linebreak, cmd) in &self.tail {
            pipe.write_highlighted(engine, context)?;
            linebreak.write_highlighted(engine, context)?;
            cmd.write_highlighted(engine, context)?;
        }
        Ok(())
    }
}

impl Highlighter for Command {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            Command::Simple(cmd) => cmd.write_highlighted(engine, context),
            Command::Compound(cmd, redirections) => {
                cmd.write_highlighted(engine, context)?;
                for redirection in redirections {
                    redirection.write_highlighted(engine, context)?;
                }
                Ok(())
            }
            Command::FunctionDefinition(func_def) => func_def.write_highlighted(engine, context),
        }
    }
}

impl Highlighter for CompoundCommand {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            CompoundCommand::Brace(brace_group) => brace_group.write_highlighted(engine, context),
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
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.linebreak.write_highlighted(engine, context)?;
        self.term.write_highlighted(engine, context)?;
        if let Some(separator) = &self.separator {
            separator.write_highlighted(engine, context)?;
        }
        Ok(())
    }
}

impl Highlighter for Term {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.head.write_highlighted(engine, context)?;
        for (sep, and_or) in &self.tail {
            sep.write_highlighted(engine, context)?;
            and_or.write_highlighted(engine, context)?;
        }
        Ok(())
    }
}

impl Highlighter for FunctionDefinition {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.name.write_highlighted(engine, context)?;
        queue!(
            engine.writer,
            SetForegroundColor(Colors::FUNC_DEF_PAREN_COLOR),
            Print(&self.parens),
            ResetColor
        )?;
        self.linebreak.write_highlighted(engine, context)?;
        self.body.write_highlighted(engine, context)?;

        Ok(())
    }
}

impl Highlighter for FunctionBody {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        self.command.write_highlighted(engine, context)?;
        for redirection in &self.redirections {
            redirection.write_highlighted(engine, context)?;
        }
        Ok(())
    }
}

impl Highlighter for BraceGroup {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::BRACE_GROUP_COLOR),
            Print(&self.lbrace_ws),
            Print('{'),
            ResetColor
        )?;
        self.body.write_highlighted(engine, context)?;
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
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let cmd_color = match &self.name {
            Some(word) => {
                if engine.has_executable(word)
                    || (engine.has_abbreviation(&word.name) && context.abbreviations)
                {
                    Colors::VALID_CMD_COLOR
                } else {
                    Colors::INVALID_CMD_COLOR
                }
            }
            _ => Colors::INVALID_CMD_COLOR,
        };

        for prefix in &self.prefixes {
            prefix.write_highlighted(engine, context)?;
        }

        if let Some(name) = &self.name {
            queue!(engine.writer, SetForegroundColor(cmd_color))?;

            name.write_highlighted(engine, context)?;

            queue!(engine.writer, ResetColor)?;
        }

        for suffix in &self.suffixes {
            suffix.write_highlighted(engine, context)?;
        }

        Ok(())
    }
}

impl Highlighter for CmdPrefix {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            Self::Redirection(r) => r.write_highlighted(engine, context),
            Self::Assignment(a) => a.write_highlighted(engine, context),
        }
    }
}

impl Highlighter for CmdSuffix {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            Self::Word(w) => {
                queue!(
                    engine.writer,
                    SetForegroundColor(Colors::TRAILING_WORD_COLOR),
                )?;

                w.write_highlighted(engine, context)?;

                queue!(engine.writer, ResetColor)?;

                Ok(())
            }
            Self::Redirection(r) => r.write_highlighted(engine, context),
        }
    }
}

impl Highlighter for Redirection {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
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
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
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
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let mut lines = self.whitespace.split('\n').peekable();

        let first = lines.next().unwrap();
        queue!(engine.writer, Clear(ClearType::UntilNewLine), Print(first))?;

        for line in lines {
            queue!(
                engine.writer,
                MoveToColumn(context.start_x),
                MoveDown(1),
                Clear(ClearType::UntilNewLine),
                Print(line)
            )?;
        }

        Ok(())
    }
}

impl Highlighter for Linebreak {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        if let Some(newlines) = &self.newlines {
            newlines.write_highlighted(engine, context)?;
        }
        Ok(())
    }
}

impl Highlighter for SeparatorOp {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
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
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        match self {
            Separator::Explicit(op, linebreak) => {
                op.write_highlighted(engine, context)?;
                linebreak.write_highlighted(engine, context)
            }
            Separator::Implicit(newlines) => newlines.write_highlighted(engine, context),
        }
    }
}

impl Highlighter for LogicalOp {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        Ok(queue!(
            engine.writer,
            SetForegroundColor(Colors::LOGICAL_OP_COLOR),
            Print(self.to_string()),
            ResetColor
        )?)
    }
}

impl Highlighter for Name {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        queue!(engine.writer, Print(self.to_string()))?;
        Ok(())
    }
}

impl Highlighter for Bang {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
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
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
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
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        queue!(
            engine.writer,
            SetForegroundColor(Colors::PIPE_COLOR),
            Print(self.to_string()),
            ResetColor,
        )?;
        Ok(())
    }
}

impl Highlighter for Word {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let s = format!("{}{}", self.whitespace, self.name_with_escaped_newlines);
        let mut lines = s.split('\n').peekable();

        let first = lines.next().unwrap();
        queue!(engine.writer, Clear(ClearType::UntilNewLine), Print(first))?;

        for line in lines {
            queue!(
                engine.writer,
                MoveToColumn(context.start_x),
                MoveDown(1),
                Clear(ClearType::UntilNewLine),
                Print(line)
            )?;
        }

        Ok(())
    }
}
