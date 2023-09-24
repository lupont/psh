use std::collections::HashMap;
use std::io::stdout;

use crossterm::cursor::{MoveDown, MoveToColumn};
use crossterm::style::{Print, ResetColor, SetForegroundColor};
use crossterm::terminal::{Clear, ClearType};
use crossterm::{execute, queue};

use psh_core::ast::nodes::*;
use psh_core::engine::expand::Expand;
use psh_core::{Engine, Result};

use crate::color;

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

        let unparsed_color = color::unparsed(engine);
        queue!(stdout(), SetForegroundColor(unparsed_color))?;
        for c in self.unparsed.chars() {
            if c == '\n' {
                queue!(
                    stdout(),
                    MoveToColumn(context.start_x),
                    MoveDown(1),
                    Clear(ClearType::UntilNewLine)
                )?;
            } else {
                queue!(stdout(), Print(c))?;
            }
        }
        execute!(stdout(), ResetColor)?;

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
        let color = color::normal(engine);
        self.name.write_highlighted(engine, context)?;
        queue!(
            stdout(),
            SetForegroundColor(color),
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
        let separator_color = color::separator(engine);
        queue!(
            stdout(),
            SetForegroundColor(separator_color),
            Print(&self.lbrace_ws),
            Print('{'),
            ResetColor
        )?;
        self.body.write_highlighted(engine, context)?;
        queue!(
            stdout(),
            SetForegroundColor(separator_color),
            Print(&self.rbrace_ws),
            Print('}'),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for SimpleCommand {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        for prefix in &self.prefixes {
            prefix.write_highlighted(engine, context)?;
        }

        if let Some(name) = &self.name {
            let args = name.clone().expand(engine);

            let has_cmd = |cmd| {
                engine.has_executable(cmd, false)
                    || (context.abbreviations && engine.has_abbreviation(cmd))
            };

            let has_cmd_starting_with = |cmd| {
                engine.has_executable(cmd, true)
                    || (context.abbreviations && engine.has_abbreviation(cmd))
            };

            let cmd_color = match args.first() {
                Some(name) if has_cmd(name) => color::valid_cmd(engine),
                Some(name) if has_cmd_starting_with(name) => color::valid_cmd_start(engine),
                _ => color::invalid_cmd(engine),
            };

            queue!(stdout(), SetForegroundColor(cmd_color))?;

            name.write_highlighted(engine, context)?;

            queue!(stdout(), ResetColor)?;
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
                let color = color::normal(engine);
                queue!(stdout(), SetForegroundColor(color))?;

                w.write_highlighted(engine, context)?;

                queue!(stdout(), ResetColor)?;

                Ok(())
            }
            Self::Redirection(r) => r.write_highlighted(engine, context),
        }
    }
}

impl Highlighter for Redirection {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let lhs_color = color::lhs(engine);
        let op_color = color::op(engine);
        let rhs_color = color::rhs(engine);

        match self {
            Redirection::File {
                whitespace,
                input_fd,
                ty,
                target,
            } => {
                queue!(
                    stdout(),
                    Print(whitespace),
                    SetForegroundColor(lhs_color),
                    Print(if let Some(fd) = input_fd {
                        fd.to_string()
                    } else {
                        String::new()
                    }),
                    SetForegroundColor(op_color),
                    Print(ty.to_string()),
                    SetForegroundColor(rhs_color),
                    ResetColor,
                )?;
                target.write_highlighted(engine, context)?;
                queue!(stdout(), ResetColor)?;
                Ok(())
            }
            Redirection::Here {
                whitespace,
                input_fd,
                ty,
                end,
                content,
            } => {
                queue!(
                    stdout(),
                    Print(whitespace),
                    SetForegroundColor(lhs_color),
                    Print(if let Some(fd) = input_fd {
                        fd.to_string()
                    } else {
                        String::new()
                    }),
                    SetForegroundColor(op_color),
                    Print(ty.to_string()),
                    SetForegroundColor(rhs_color),
                    ResetColor,
                    Print(end.to_string())
                )?;
                end.write_highlighted(engine, context)?;
                content.write_highlighted(engine, context)?;
                queue!(stdout(), ResetColor)?;
                Ok(())
            }
        }
    }
}

impl Highlighter for VariableAssignment {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let lhs_color = color::lhs(engine);
        let op_color = color::op(engine);
        let rhs_color = color::rhs(engine);

        queue!(
            stdout(),
            Print(&self.whitespace),
            SetForegroundColor(lhs_color),
            Print(self.lhs.to_string()),
            SetForegroundColor(op_color),
            Print('='),
            SetForegroundColor(rhs_color),
        )?;

        if let Some(rhs) = &self.rhs {
            rhs.write_highlighted(engine, context)?;
        }

        queue!(stdout(), ResetColor)?;
        Ok(())
    }
}

impl Highlighter for NewlineList {
    fn write_highlighted(&self, _: &mut Engine, context: Context) -> Result<()> {
        let mut lines = self.whitespace.split('\n').peekable();

        let first = lines.next().unwrap();
        queue!(stdout(), Clear(ClearType::UntilNewLine), Print(first))?;

        for line in lines {
            queue!(
                stdout(),
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
        let separator_color = color::separator(engine);
        queue!(
            stdout(),
            SetForegroundColor(separator_color),
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
        let separator_color = color::separator(engine);
        Ok(queue!(
            stdout(),
            SetForegroundColor(separator_color),
            Print(self.to_string()),
            ResetColor
        )?)
    }
}

impl Highlighter for Name {
    fn write_highlighted(&self, _: &mut Engine, _: Context) -> Result<()> {
        queue!(stdout(), Print(self.to_string()))?;
        Ok(())
    }
}

impl Highlighter for Bang {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        let separator_color = color::separator(engine);
        queue!(
            stdout(),
            SetForegroundColor(separator_color),
            Print(self.to_string()),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for Comment {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        let color = color::comment(engine);
        queue!(
            stdout(),
            SetForegroundColor(color),
            Print(self.to_string()),
            ResetColor
        )?;
        Ok(())
    }
}

impl Highlighter for Pipe {
    fn write_highlighted(&self, engine: &mut Engine, _: Context) -> Result<()> {
        let color = color::separator(engine);
        queue!(
            stdout(),
            SetForegroundColor(color),
            Print(self.to_string()),
            ResetColor,
        )?;
        Ok(())
    }
}

impl Highlighter for Word {
    fn write_highlighted(&self, engine: &mut Engine, context: Context) -> Result<()> {
        let mut chars = self.name.chars().peekable().enumerate();

        let mut cmd_sub_starts = HashMap::new();
        for exp in &self.expansions {
            if let Expansion::Command {
                range,
                tree,
                finished,
                ..
            } = exp
            {
                cmd_sub_starts.insert(*range.start(), (*range.end(), tree, finished));
            }
        }

        queue!(
            stdout(),
            Clear(ClearType::UntilNewLine),
            Print(&self.whitespace)
        )?;

        let cmd_sub_color = color::cmd_sub(engine);
        while let Some((i, c)) = chars.next() {
            if let Some((end, tree, &finished)) = cmd_sub_starts.get(&i) {
                queue!(
                    stdout(),
                    SetForegroundColor(cmd_sub_color),
                    Print("$("),
                    ResetColor
                )?;
                tree.write_highlighted(engine, context)?;
                if finished {
                    queue!(
                        stdout(),
                        SetForegroundColor(cmd_sub_color),
                        Print(')'),
                        ResetColor
                    )?;
                }
                for _ in i..*end {
                    chars.next();
                }
            } else if c == '\n' {
                queue!(
                    stdout(),
                    MoveToColumn(context.start_x),
                    MoveDown(1),
                    Clear(ClearType::UntilNewLine)
                )?;
            } else {
                queue!(stdout(), Print(c))?;
            }
        }

        Ok(())
    }
}
