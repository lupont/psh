use std::io::Write;

use crate::ast::prelude::*;
use crate::ast::QuoteState;
use crate::{path, Engine};

pub trait Expand {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self;
}

impl Expand for SyntaxTree {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            program: self.program.into_iter().map(|c| c.expand(engine)).collect(),
            unparsed: self.unparsed,
        }
    }
}

impl Expand for CompleteCommand {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            comment: self.comment,
            separator: self.separator,
            list: self.list.expand(engine),
        }
    }
}

impl Expand for List {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            first: self.first.expand(engine),
            rest: self
                .rest
                .into_iter()
                .map(|(sep, and_or)| (sep, and_or.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for AndOrList {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            first: self.first.expand(engine),
            rest: self
                .rest
                .into_iter()
                .map(|(op, pipeline)| (op, pipeline.expand(engine)))
                .collect(),
        }
    }
}

impl Expand for Pipeline {
    fn expand(self, engine: &mut Engine<impl Write>) -> Self {
        Self {
            bang: self.bang,
            first: self.first.expand(engine),
            rest: self
                .rest
                .into_iter()
                .map(|(ws, cmd)| (ws, cmd.expand(engine)))
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
            Self::Output {
                file_descriptor,
                append,
                target,
            } => Self::Output {
                file_descriptor: file_descriptor.expand(engine),
                append,
                target: target.expand(engine),
            },

            Self::Input {
                file_descriptor,
                target,
            } => Self::Input {
                file_descriptor: file_descriptor.expand(engine),
                target: target.expand(engine),
            },

            Self::HereDocument {
                file_descriptor,
                delimiter,
            } => Self::HereDocument {
                file_descriptor: file_descriptor.expand(engine),
                delimiter: delimiter.expand(engine),
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
        remove_quotes(parameter_expanded)
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

    Word {
        name: word.name,
        whitespace: word.whitespace,
        expansions: word.expansions,
    }
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

fn remove_quotes(word: Word) -> Word {
    let mut s = String::new();
    let mut state = QuoteState::None;
    let mut is_escaped = false;

    for c in word.name.chars() {
        match (c, state) {
            ('\'', QuoteState::Single) => {
                state = QuoteState::None;
            }
            ('\'', QuoteState::None) => {
                state = QuoteState::Single;
            }
            (c, QuoteState::Single) => s.push(c),

            ('"', QuoteState::Double) if !is_escaped => {
                state = QuoteState::None;
            }
            ('"', QuoteState::None) => {
                state = QuoteState::Double;
            }

            (c, QuoteState::Double) if !is_escaped => {
                s.push(c);
            }

            ('\\', _) if !is_escaped => is_escaped = true,

            (c, _) => {
                s.push(c);
                is_escaped = false;
            }
        }
    }

    Word {
        name: s,
        whitespace: word.whitespace,
        expansions: word.expansions,
    }
}
