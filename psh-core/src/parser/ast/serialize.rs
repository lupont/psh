use serde::ser::SerializeStruct;
use serde::Serialize;

use super::nodes::*;

impl Serialize for SyntaxTree {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("SyntaxTree", 4)?;

        state.serialize_field("leading_linebreak", &self.leading)?;

        if let Some((commands, linebreak)) = &self.commands {
            state.serialize_field("complete_commands", commands)?;
            state.serialize_field("trailing_linebreak", linebreak)?;
        } else {
            state.serialize_field("complete_commands", &None::<CompleteCommands>)?;
            state.serialize_field("trailing_linebreak", &None::<Linebreak>)?;
        }

        if self.unparsed.is_empty() {
            state.serialize_field("unparsed", &None::<String>)?;
        } else {
            state.serialize_field("unparsed", &self.unparsed)?;
        }

        state.end()
    }
}

impl Serialize for CompleteCommands {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Tail<'a> {
            newlines: &'a NewlineList,
            complete_command: &'a CompleteCommand,
        }

        let mut state = serializer.serialize_struct("CompleteCommands", 2)?;

        state.serialize_field("complete_commands_head", &self.head)?;

        state.serialize_field(
            "complete_commands_tail",
            &self
                .tail
                .iter()
                .map(|(n, c)| Tail {
                    newlines: n,
                    complete_command: c,
                })
                .collect::<Vec<_>>(),
        )?;

        state.end()
    }
}

impl Serialize for CompleteCommand {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("CompleteCommand", 3)?;

        match self {
            Self::List {
                list,
                separator_op,
                comment,
            } => {
                state.serialize_field("list", list)?;
                state.serialize_field("separator_op", separator_op)?;
                state.serialize_field("comment", comment)?;
            }
            Self::Comment(comment) => {
                state.serialize_field("list", &None::<List>)?;
                state.serialize_field("separator_op", &None::<SeparatorOp>)?;
                state.serialize_field("comment", comment)?;
            }
        }

        state.end()
    }
}

impl Serialize for List {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Tail<'a> {
            separator_op: &'a SeparatorOp,
            and_or_list: &'a AndOrList,
        }

        let mut state = serializer.serialize_struct("List", 2)?;

        state.serialize_field("list_head", &self.head)?;

        state.serialize_field(
            "list_tail",
            &self
                .tail
                .iter()
                .map(|(op, list)| Tail {
                    separator_op: op,
                    and_or_list: list,
                })
                .collect::<Vec<_>>(),
        )?;

        state.end()
    }
}

impl Serialize for AndOrList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Tail<'a> {
            op: &'a LogicalOp,
            linebreak: &'a Linebreak,
            pipeline: &'a Pipeline,
        }

        let mut state = serializer.serialize_struct("AndOrList", 2)?;

        state.serialize_field("and_or_list_head", &self.head)?;

        state.serialize_field(
            "and_or_list_tail",
            &self
                .tail
                .iter()
                .map(|(op, linebreak, pipeline)| Tail {
                    op,
                    linebreak,
                    pipeline,
                })
                .collect::<Vec<_>>(),
        )?;

        state.end()
    }
}

impl Serialize for Pipeline {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Tail<'a> {
            pipe: &'a Pipe,
            linebreak: &'a Linebreak,
            cmd: &'a Command,
        }

        let mut state = serializer.serialize_struct("Pipeline", 4)?;

        state.serialize_field("bang", &self.bang)?;
        state.serialize_field("pipeline_head", &self.sequence.head)?;
        state.serialize_field(
            "pipeline_tail",
            &self
                .sequence
                .tail
                .iter()
                .map(|(pipe, linebreak, cmd)| Tail {
                    pipe,
                    linebreak,
                    cmd,
                })
                .collect::<Vec<_>>(),
        )?;

        state.end()
    }
}

impl Serialize for Command {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let len = match self {
            Self::Compound(_, _) => 3,
            _ => 2,
        };

        let mut state = serializer.serialize_struct("Command", len)?;

        match self {
            Self::Simple(simple) => {
                state.serialize_field("type", "simple")?;
                state.serialize_field("command", simple)?;
            }
            Self::Compound(compound, redirs) => {
                state.serialize_field("type", "compound")?;
                state.serialize_field("command", compound)?;
                state.serialize_field("redirections", redirs)?;
            }
            Self::FunctionDefinition(func_def) => {
                state.serialize_field("type", "function_definition")?;
                state.serialize_field("command", func_def)?;
            }
        }

        state.end()
    }
}

impl Serialize for CompoundCommand {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("CompoundCommand", 2)?;

        match self {
            CompoundCommand::Brace(brace_group) => {
                state.serialize_field("type", "brace_group")?;
                state.serialize_field("command", brace_group)?;
            }
            CompoundCommand::Subshell(subshell) => {
                state.serialize_field("type", "subshell")?;
                state.serialize_field("command", subshell)?;
            }
            CompoundCommand::For(for_clause) => {
                state.serialize_field("type", "for_clause")?;
                state.serialize_field("command", for_clause)?;
            }
            CompoundCommand::Case(case_clause) => {
                state.serialize_field("type", "case_clause")?;
                state.serialize_field("command", case_clause)?;
            }
            CompoundCommand::If(if_clause) => {
                state.serialize_field("type", "if_clause")?;
                state.serialize_field("command", if_clause)?;
            }
            CompoundCommand::While(while_clause) => {
                state.serialize_field("type", "while_clause")?;
                state.serialize_field("command", while_clause)?;
            }
            CompoundCommand::Until(until_clause) => {
                state.serialize_field("type", "until_clause")?;
                state.serialize_field("command", until_clause)?;
            }
        }

        state.end()
    }
}

impl Serialize for CmdPrefix {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("CmdPrefix", 2)?;

        match self {
            Self::Redirection(r) => {
                state.serialize_field("type", "redirection")?;
                state.serialize_field("redirection", r)?;
            }
            Self::Assignment(a) => {
                state.serialize_field("type", "assignment")?;
                state.serialize_field("assignment", a)?;
            }
        }

        state.end()
    }
}

impl Serialize for CmdSuffix {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("CmdSuffix", 2)?;

        match self {
            Self::Redirection(r) => {
                state.serialize_field("type", "redirection")?;
                state.serialize_field("redirection", r)?;
            }
            Self::Word(w) => {
                state.serialize_field("type", "word")?;
                state.serialize_field("word", w)?;
            }
        }

        state.end()
    }
}

impl Serialize for FileDescriptor {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Stdin => serializer.serialize_i32(0),
            Self::Stdout => serializer.serialize_i32(1),
            Self::Stderr => serializer.serialize_i32(2),
            Self::Other(fd) => serializer.serialize_i32(*fd),
        }
    }
}

impl Serialize for Word {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Word", 3)?;

        state.serialize_field("leading_whitespace", &self.whitespace)?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("expansions", &self.expansions)?;

        state.end()
    }
}

impl Serialize for LogicalOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("LogicalOp", 2)?;

        match self {
            Self::And(ws) => {
                state.serialize_field("leading_whitespace", ws)?;
                state.serialize_field("type", "and")?;
            }
            Self::Or(ws) => {
                state.serialize_field("leading_whitespace", ws)?;
                state.serialize_field("type", "or")?;
            }
        }

        state.end()
    }
}

impl Serialize for NewlineList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.whitespace.is_empty() {
            serializer.serialize_none()
        } else {
            serializer.serialize_str(&self.whitespace)
        }
    }
}

impl Serialize for Linebreak {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(newlines) = &self.newlines {
            newlines.serialize(serializer)
        } else {
            serializer.serialize_none()
        }
    }
}

impl Serialize for SeparatorOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("SeparatorOp", 2)?;

        match self {
            Self::Sync(ws) => {
                state.serialize_field("leading_whitespace", ws)?;
                state.serialize_field("type", "sync")?;
            }
            Self::Async(ws) => {
                state.serialize_field("leading_whitespace", ws)?;
                state.serialize_field("type", "async")?;
            }
        }

        state.end()
    }
}

impl Serialize for Separator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Explicit(op, linebreak) => {
                let mut state = serializer.serialize_struct("Separator", 3)?;
                state.serialize_field("type", "explicit")?;
                state.serialize_field("op", op)?;
                state.serialize_field("linebreak", linebreak)?;
                state.end()
            }
            Self::Implicit(newlines) => {
                let mut state = serializer.serialize_struct("Separator", 2)?;
                state.serialize_field("type", "implicit")?;
                state.serialize_field("newlines", newlines)?;
                state.end()
            }
        }
    }
}

impl Serialize for LeadingWhitespace {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.0.is_empty() {
            serializer.serialize_none()
        } else {
            serializer.serialize_str(&self.0)
        }
    }
}
