pub mod reconstruct;

#[cfg(test)]
mod tests;

use std::{iter::Peekable, ops::RangeInclusive};

use super::consumer::Consumer;
use super::semtok::{Keyword, SemanticToken, SemanticTokenizer};
use super::tok::Tokenizer;

use crate::Result;

pub fn parse(input: impl AsRef<str>) -> Result<SyntaxTree> {
    input
        .as_ref()
        .chars()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .parse()
}

pub trait Parser: Iterator<Item = SemanticToken> + std::fmt::Debug {
    fn parse(&mut self) -> Result<SyntaxTree> {
        let mut complete_commands = Vec::new();

        while let Some(cmd) = self.parse_complete_command() {
            complete_commands.push(cmd);
        }

        Ok(SyntaxTree {
            program: complete_commands,
        })
    }

    fn parse_complete_command(&mut self) -> Option<CompleteCommand> {
        let list = match self.parse_list() {
            Some(list) => list,
            None => return None,
        };

        let separator = self.parse_separator();

        Some(CompleteCommand { list, separator })
    }

    fn parse_list(&mut self) -> Option<List>;
    fn parse_and_or_list(&mut self) -> Option<AndOrList> {
        let first = match self.parse_pipeline() {
            Some(pipeline) => pipeline,
            None => return None,
        };

        let mut rest = Vec::new();

        while let Some(thing) = self.parse_logical_op().and_then(|op| {
            // FIXME: this currently allows a list to end with a && or ||
            self.parse_pipeline().map(|c| (op, c))
        }) {
            rest.push(thing);
        }

        Some(AndOrList { first, rest })
    }
    fn parse_pipeline(&mut self) -> Option<Pipeline>;

    fn parse_command(&mut self) -> Option<Command> {
        self.parse_function_definition()
            .map(Command::FunctionDefinition)
            .or_else(|| self.parse_compound_command().map(Command::Compound))
            .or_else(|| self.parse_simple_command().map(Command::Simple))
    }
    fn parse_function_definition(&mut self) -> Option<FunctionDefinition>;
    fn parse_compound_command(&mut self) -> Option<CompoundCommand>;
    fn parse_simple_command(&mut self) -> Option<SimpleCommand>;

    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment>;
    fn parse_redirection(&mut self) -> Option<Redirection>;

    fn parse_word(&mut self, allow_ampersand: bool) -> Option<Word>;
    fn parse_redirection_fd(&mut self) -> Option<Word>;
    fn parse_separator(&mut self) -> Option<Separator>;
    fn parse_logical_op(&mut self) -> Option<LogicalOp>;
    fn parse_pipe(&mut self) -> Option<String>;
    fn parse_bang(&mut self) -> Option<String>;

    fn parse_comment(&mut self) -> Option<(String, String)>;

    fn swallow_whitespace(&mut self) -> LeadingWhitespace;
}

impl<T> Parser for Peekable<T>
where
    T: Iterator<Item = SemanticToken> + Clone + std::fmt::Debug,
{
    fn parse_list(&mut self) -> Option<List> {
        let first = match self.parse_and_or_list() {
            Some(list) => list,
            None => return None,
        };

        let mut rest = Vec::new();
        let mut initial = self.clone();

        while let Some(thing) = self.parse_separator().and_then(|separator| {
            self.parse_and_or_list()
                .map(|a| (separator, a))
                .or_else(|| {
                    *self = initial.clone();
                    None
                })
        }) {
            rest.push(thing);
            initial = self.clone();
        }

        let comment = self.parse_comment();

        Some(List {
            first,
            rest,
            comment,
        })
    }

    fn parse_pipeline(&mut self) -> Option<Pipeline> {
        let initial = self.clone();

        let bang = self.parse_bang();

        let first = match self.parse_command() {
            Some(cmd) => cmd,
            None => {
                *self = initial;
                return None;
            }
        };

        let mut rest = Vec::new();

        while let Some((ws, cmd)) = self
            .parse_pipe()
            .and_then(|ws| self.parse_command().map(|cmd| (ws, cmd)))
        {
            rest.push((ws, cmd));
        }

        Some(Pipeline { bang, first, rest })
    }

    fn parse_function_definition(&mut self) -> Option<FunctionDefinition> {
        None
    }

    fn parse_compound_command(&mut self) -> Option<CompoundCommand> {
        None
    }

    fn parse_simple_command(&mut self) -> Option<SimpleCommand> {
        let initial = self.clone();

        let mut prefixes = Vec::new();
        let mut suffixes = Vec::new();

        while let Some(thing) = self
            .parse_variable_assignment()
            .map(SimpleCommandMeta::Assignment)
            .or_else(|| self.parse_redirection().map(SimpleCommandMeta::Redirection))
        {
            match thing {
                a @ SimpleCommandMeta::Assignment(_) => prefixes.push(a),
                r @ SimpleCommandMeta::Redirection(_) => prefixes.push(r),
                SimpleCommandMeta::Word(_) => unreachable!(),
            }
        }

        // FIXME: we need to support variable assignments without command execution,
        //        e.g. just ^foo=bar$
        let name = match self.parse_word(false) {
            Some(word) => word,
            None => {
                *self = initial;
                return None;
            }
        };

        while let Some(thing) = self
            .parse_redirection()
            .map(SimpleCommandMeta::Redirection)
            .or_else(|| self.parse_word(false).map(SimpleCommandMeta::Word))
        {
            match thing {
                r @ SimpleCommandMeta::Redirection(_) => suffixes.push(r),
                w @ SimpleCommandMeta::Word(_) => suffixes.push(w),
                SimpleCommandMeta::Assignment(_) => unreachable!(),
            }
        }

        Some(SimpleCommand {
            name,
            prefixes,
            suffixes,
        })
    }

    fn parse_separator(&mut self) -> Option<Separator> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::SyncSeparator)
            .or_else(|| self.consume_single(SemanticToken::AsyncSeparator))
            .map(|t| match t {
                SemanticToken::SyncSeparator => Separator::Sync(ws),
                SemanticToken::AsyncSeparator => Separator::Async(ws),
                _ => unreachable!(),
            })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_logical_op(&mut self) -> Option<LogicalOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::And)
            .or_else(|| self.consume_single(SemanticToken::Or))
            .map(|t| match t {
                SemanticToken::And => LogicalOp::And(ws),
                SemanticToken::Or => LogicalOp::Or(ws),
                _ => unreachable!(),
            })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_pipe(&mut self) -> Option<String> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::Pipe)
            .map(|_| ws)
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_bang(&mut self) -> Option<String> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::Keyword(Keyword::Bang))
            .map(|_| ws)
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_comment(&mut self) -> Option<(String, String)> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if let Some(SemanticToken::Comment(comment)) = self.next() {
            Some((ws, comment))
        } else {
            *self = initial;
            None
        }
    }

    fn swallow_whitespace(&mut self) -> LeadingWhitespace {
        let mut s = LeadingWhitespace::new();
        while let Some(SemanticToken::Whitespace(c)) = self.peek() {
            s.push(*c);
            self.next();
        }
        s
    }

    fn parse_redirection(&mut self) -> Option<Redirection> {
        let initial = self.clone();

        let fd = self.parse_redirection_fd().unwrap_or_else(|| {
            let ws = self.swallow_whitespace();
            Word::new("", ws)
        });

        match self
            .consume_single(SemanticToken::RedirectOutput)
            .or_else(|| self.consume_single(SemanticToken::RedirectInput))
        {
            Some(SemanticToken::RedirectInput) => {
                let is_here_doc = self.consume_single(SemanticToken::RedirectInput).is_some();

                match self.parse_word(false) {
                    Some(target) if is_here_doc => Some(Redirection::HereDocument {
                        file_descriptor: fd,
                        delimiter: target,
                    }),

                    Some(target) => Some(Redirection::Input {
                        file_descriptor: fd,
                        target,
                    }),

                    None => {
                        *self = initial;
                        None
                    }
                }
            }

            Some(SemanticToken::RedirectOutput) => {
                let append = self.consume_single(SemanticToken::RedirectOutput).is_some();

                let target = match self.parse_word(true) {
                    Some(word) => word,
                    None => {
                        *self = initial;
                        return None;
                    }
                };

                Some(Redirection::Output {
                    file_descriptor: fd,
                    append,
                    target,
                })
            }

            _ => {
                *self = initial;
                None
            }
        }
    }

    // allow_ampersand should almost always be `false`. the one case
    // it needs to be `true` is when parsing redirection targets
    fn parse_word(&mut self, allow_ampersand: bool) -> Option<Word> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        let mut s = String::new();
        if allow_ampersand {
            if let Some(SemanticToken::AsyncSeparator) = self.peek() {
                self.next();
                s.push('&');
            }
        }

        match self.peek() {
            Some(SemanticToken::Word(xs)) => {
                s.push_str(xs);
                let word = Word::new(&s, ws);
                self.next();
                Some(word)
            }
            _ => {
                *self = initial;
                None
            }
        }
    }

    fn parse_redirection_fd(&mut self) -> Option<Word> {
        let ws = self.swallow_whitespace();

        match self.peek() {
            Some(SemanticToken::RedirectOutput | SemanticToken::RedirectInput) => {
                Some(Word::new("", ws))
            }

            Some(SemanticToken::Word(word)) if word.len() == 1 => match word.chars().next() {
                Some('0'..='9') => {
                    let word = Word::new(word, ws);
                    self.next();
                    Some(word)
                }
                _ => None,
            },

            _ => None,
        }
    }

    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment> {
        let initial = self.clone();

        if let Some(Word {
            name, whitespace, ..
        }) = self.parse_word(false)
        {
            if let Some((lhs, rhs)) = name.split_once('=') {
                let var_assg = VariableAssignment::new(
                    Word::new(lhs, whitespace),
                    if rhs.is_empty() {
                        None
                    } else {
                        Some(Word::new(rhs, ""))
                    },
                );
                return Some(var_assg);
            }
        }

        *self = initial;
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    pub program: Vec<CompleteCommand>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompleteCommand {
    pub list: List,
    pub separator: Option<Separator>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct List {
    pub first: AndOrList,

    pub comment: Option<(String, String)>,

    // As noted semantically by having it be the first part of
    // the tuple, each `Separator` here ends the previous
    // `AndOrList`.
    pub rest: Vec<(Separator, AndOrList)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AndOrList {
    pub first: Pipeline,

    // As noted semantically by having it be the first part of
    // the tuple, each `LogicalOp` here operates on the previous
    // `Pipeline` and it's tuple partner.
    pub rest: Vec<(LogicalOp, Pipeline)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pipeline {
    pub bang: Option<String>,
    pub first: Command,
    pub rest: Vec<(String, Command)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Simple(SimpleCommand),
    Compound(CompoundCommand),
    FunctionDefinition(FunctionDefinition),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    pub name: Word,
    pub prefixes: Vec<SimpleCommandMeta>,
    pub suffixes: Vec<SimpleCommandMeta>,
}

impl SimpleCommand {
    pub fn name(&self) -> &String {
        &self.name.name
    }

    pub fn args(&self) -> impl Iterator<Item = &String> {
        self.suffixes
            .iter()
            .filter_map(|m| match m {
                SimpleCommandMeta::Word(w) => Some(w),
                _ => None,
            })
            .map(|w| &w.name)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SimpleCommandMeta {
    Word(Word),
    Redirection(Redirection),
    Assignment(VariableAssignment),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Redirection {
    Output {
        file_descriptor: Word,
        append: bool,
        target: Word,
    },

    Input {
        file_descriptor: Word,
        target: Word,
    },

    HereDocument {
        file_descriptor: Word,
        delimiter: Word,
    },
}

impl Redirection {
    pub fn new_output(fd: Word, target: Word, append: bool) -> Self {
        Self::Output {
            file_descriptor: fd,
            append,
            target,
        }
    }

    pub fn new_input(fd: Word, target: Word) -> Self {
        Self::Input {
            file_descriptor: fd,
            target,
        }
    }

    pub fn new_here_doc(fd: Word, delimiter: Word) -> Self {
        Self::HereDocument {
            file_descriptor: fd,
            delimiter,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableAssignment {
    pub lhs: Word,
    pub rhs: Option<Word>,
}

impl VariableAssignment {
    pub fn new(lhs: Word, rhs: Option<Word>) -> Self {
        Self { lhs, rhs }
    }
}

pub type LeadingWhitespace = String;

#[derive(Debug, PartialEq, Eq)]
pub struct Word {
    pub raw: String,
    pub name: String,
    pub whitespace: LeadingWhitespace,
    pub expansions: Vec<Expansion>,
}

impl Word {
    pub fn new(input: &str, whitespace: impl Into<LeadingWhitespace>) -> Self {
        let quote_removed = Self::do_quote_removal(input);
        let expanded = Self::expand(quote_removed);

        // FIXME: get rid of .to_string() twice, probably requires API change
        Self {
            raw: input.to_string(),
            name: expanded.to_string(),
            whitespace: whitespace.into(),
            expansions: Default::default(),
        }
    }

    fn do_quote_removal(input: &str) -> &str {
        // TODO: do quote removal
        input
    }

    // TODO: expanding should probably result in multiple `Word`s
    // fn expand(self) -> Vec<Self> {
    //     vec![self]
    // }

    fn expand(input: &str) -> &str {
        // TODO: expand
        input
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expansion {
    Tilde {
        index: usize,
    },

    Glob {
        range: RangeInclusive<usize>,
        recursive: bool,
        pattern: String,
    },

    Brace {
        range: RangeInclusive<usize>,
        pattern: String,
    },

    Parameter {
        range: RangeInclusive<usize>,
        name: String,
    },

    Command {
        range: RangeInclusive<usize>,
        tree: SyntaxTree,
    },

    Arithmetic {
        range: RangeInclusive<usize>,
        expression: Word,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundCommand {
    //  true: (cmd; cmd)
    //  false: { cmd; cmd; }
    pub subshell: bool,
    pub first: Box<Command>,
    pub rest: Vec<Command>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: String,
    pub commands: CompoundCommand,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    Sync(LeadingWhitespace),
    Async(LeadingWhitespace),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And(LeadingWhitespace),
    Or(LeadingWhitespace),
}
