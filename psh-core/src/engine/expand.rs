use std::io::Write;

use crate::ast::prelude::*;
use crate::{path, Engine};

pub trait Expand {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self;
}

impl Expand for SyntaxTree {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            leading: self.leading,
            commands: self
                .commands
                .map(|(cmds, linebreak)| (cmds.expand(engine), linebreak)),
            unparsed: self.unparsed,
        }
    }
}

impl Expand for CompleteCommands {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            head: self.head.expand(engine),
            tail: self
                .tail
                .into_iter()
                .map(|(newlines, cmd)| (newlines, cmd.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for CompleteCommand {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        match self {
            Self::List(list, separator_op, comment) => {
                Self::List(list.expand(engine), separator_op, comment)
            }
            Self::Comment(comment) => Self::Comment(comment),
        }
    }
}

impl Expand for List {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            head: self.head.expand(engine),
            tail: self
                .tail
                .into_iter()
                .map(|(sep, and_or)| (sep, and_or.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for AndOrList {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            head: self.head.expand(engine),
            tail: self
                .tail
                .into_iter()
                .map(|(op, linebreak, pipeline)| (op, linebreak, pipeline.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for Pipeline {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            bang: self.bang,
            sequence: self.sequence.expand(engine),
        }
    }
}

impl Expand for PipeSequence {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            head: Box::new(self.head.expand(engine)),
            tail: self
                .tail
                .into_iter()
                .map(|(ws, linebreak, cmd)| (ws, linebreak, cmd.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for Command {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        match self {
            Self::Simple(cmd) => Self::Simple(cmd.expand(engine)),
            Self::Compound(_, _) => todo!(),
            Self::FunctionDefinition(_) => todo!(),
        }
    }
}

impl Expand for SimpleCommand {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            name: self.name.map(|w| w.expand(engine)),
            prefixes: self
                .prefixes
                .into_iter()
                .map(|p| p.expand(engine))
                .collect(),
            suffixes: self
                .suffixes
                .into_iter()
                .map(|s| s.expand(engine))
                .collect(),
        }
    }
}

impl Expand for CmdPrefix {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        match self {
            Self::Redirection(r) => Self::Redirection(r.expand(engine)),
            Self::Assignment(a) => Self::Assignment(a.expand(engine)),
        }
    }
}

impl Expand for CmdSuffix {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        match self {
            Self::Redirection(r) => Self::Redirection(r.expand(engine)),
            Self::Word(w) => Self::Word(w.expand(engine)),
        }
    }
}

impl Expand for Redirection {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        match self {
            Self::File {
                whitespace,
                input_fd,
                ty,
                target,
            } => Self::File {
                whitespace,
                input_fd,
                ty,
                target: target.expand(engine),
            },
            Self::Here {
                whitespace,
                input_fd,
                ty,
                end,
                content,
            } => Self::Here {
                whitespace,
                input_fd,
                ty,
                end: end.expand(engine),
                content: content.expand(engine),
            },
        }
    }
}

impl Expand for VariableAssignment {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            whitespace: self.whitespace,
            lhs: self.lhs,
            rhs: self.rhs.map(|w| w.expand(engine)),
        }
    }
}

impl Expand for Word {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        let tilde_expanded = expand_tilde(self);
        let parameter_expanded = expand_parameters(tilde_expanded, engine);
        // FIXME: command substitution
        // FIXME: arithmetic expression
        // FIXME: field split (should return one "main" word, and a list of trailing words
        // FIXME: pathname expand
        quote_removal(parameter_expanded)
    }
}

fn expand_tilde(mut word: Word) -> Word {
    let Some(index) = word.expansions.iter().position(|e| matches!(e, Expansion::Tilde { .. })) else {
        return word;
    };

    let Expansion::Tilde { range, name } = word.expansions.remove(index) else {
        return word;
    };

    if !name.is_empty() && path::is_portable_filename(&name) && path::system_has_user(&name) {
        // FIXME: the tilde-prefix shall be replaced by a pathname
        //        of the initial working directory associated with
        //        the login name obtained using the getpwnam()
        //        function as defined in the System Interfaces
        //        volume of POSIX.1-2017
        word.name.replace_range(range, &format!("/home/{name}"));
    } else if name.is_empty() {
        word.name.replace_range(range, &path::home_dir());
    }

    word
}

fn expand_parameters(mut word: Word, engine: &mut Engine<impl Write>) -> Word {
    let mut expansion_indices = Vec::new();
    for (i, exp) in word.expansions.iter().enumerate().rev() {
        if matches!(exp, Expansion::Parameter { .. }) {
            expansion_indices.push(i);
        }
    }

    for index in expansion_indices {
        let Expansion::Parameter { range, name } = word.expansions.remove(index) else {
            unreachable!()
        };
        if let Some(val) = engine.get_value_of(&name) {
            word.name.replace_range(range, val);
        } else {
            word.name.replace_range(range, "");
        }
    }

    word
}

fn quote_removal(word: Word) -> Word {
    Word {
        name: remove_quotes(&word.name),
        whitespace: word.whitespace,
        expansions: word.expansions,
    }
}

pub fn remove_quotes(s: &str) -> String {
    let mut name = String::new();
    let mut state = QuoteState::None;
    let mut is_escaped = false;

    for c in s.chars() {
        match (c, state) {
            ('\'', QuoteState::Single) => {
                state = QuoteState::None;
            }
            ('\'', QuoteState::None) => {
                state = QuoteState::Single;
            }
            (c, QuoteState::Single) => name.push(c),

            ('"', QuoteState::Double) if !is_escaped => {
                state = QuoteState::None;
            }
            ('"', QuoteState::None) => {
                state = QuoteState::Double;
            }

            (c, QuoteState::Double) if !is_escaped => {
                name.push(c);
            }

            ('\\', _) if !is_escaped => is_escaped = true,

            (c, _) => {
                name.push(c);
                is_escaped = false;
            }
        }
    }

    name
}