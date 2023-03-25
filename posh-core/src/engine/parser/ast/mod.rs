pub mod reconstruct;

#[cfg(test)]
mod tests;

use std::borrow::Cow;
use std::{iter::Peekable, ops::RangeInclusive};

use super::consumer::Consumer;
use super::semtok::{ReservedWord, SemanticToken, SemanticTokenizer};
use super::tok::Tokenizer;

use crate::path;
use crate::{Error, Result};

pub fn parse(input: impl AsRef<str>, allow_errors: bool) -> Result<SyntaxTree> {
    match input
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
    {
        Ok(ast) => Ok(ast),
        Err(ast) if allow_errors => Ok(ast),
        Err(ast) => Err(Error::SyntaxError(format!(
            "could not parse the following: `{}`",
            ast.unparsed.trim_start(),
        ))),
    }
}

pub trait Parser: Iterator<Item = SemanticToken> + std::fmt::Debug + Sized {
    fn parse(&mut self) -> std::result::Result<SyntaxTree, SyntaxTree> {
        let mut complete_commands = Vec::new();

        while let Some(cmd) = self.parse_complete_command() {
            complete_commands.push(cmd);
        }

        let mut unparsed = String::new();
        for token in self.by_ref() {
            unparsed.push_str(&token.to_string());
        }

        let ok = unparsed.is_empty() || unparsed.chars().all(|c| c.is_ascii_whitespace());

        let ast = SyntaxTree {
            program: complete_commands,
            unparsed,
        };

        if ok {
            Ok(ast)
        } else {
            Err(ast)
        }
    }

    fn parse_complete_command(&mut self) -> Option<CompleteCommand> {
        let Some(list) = self.parse_list() else {
            return None;
        };

        let separator = self.parse_separator();

        let comment = self.parse_comment();

        Some(CompleteCommand {
            list,
            separator,
            comment,
        })
    }

    fn parse_list(&mut self) -> Option<List>;
    fn parse_and_or_list(&mut self) -> Option<AndOrList>;
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
        let Some(first) = self.parse_and_or_list() else {
            return None;
        };

        let mut rest = Vec::new();
        let mut initial = self.clone();

        while let Some(thing) = self
            .parse_separator()
            .and_then(|s| self.parse_and_or_list().map(|a| (s, a)))
            .or_else(|| {
                *self = initial.clone();
                None
            })
        {
            rest.push(thing);
            initial = self.clone();
        }

        Some(List { first, rest })
    }

    fn parse_and_or_list(&mut self) -> Option<AndOrList> {
        let Some(first) = self.parse_pipeline() else {
            return None;
        };

        let mut rest = Vec::new();

        let mut initial = self.clone();

        while let Some(thing) = self
            .parse_logical_op()
            .and_then(|op| self.parse_pipeline().map(|c| (op, c)))
            .or_else(|| {
                *self = initial.clone();
                None
            })
        {
            rest.push(thing);
            initial = self.clone();
        }

        Some(AndOrList { first, rest })
    }

    fn parse_pipeline(&mut self) -> Option<Pipeline> {
        let mut initial = self.clone();

        let bang = self.parse_bang();

        let Some(first) = self.parse_command() else {
            *self = initial;
            return None;
        };

        let mut rest = Vec::new();

        initial = self.clone();

        while let Some((ws, cmd)) = self
            .parse_pipe()
            .and_then(|ws| self.parse_command().map(|cmd| (ws, cmd)))
            .or_else(|| {
                *self = initial;
                None
            })
        {
            rest.push((ws, cmd));
            initial = self.clone();
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
            .map(CmdPrefix::Assignment)
            .or_else(|| self.parse_redirection().map(CmdPrefix::Redirection))
        {
            match thing {
                a @ CmdPrefix::Assignment(_) => prefixes.push(a),
                r @ CmdPrefix::Redirection(_) => prefixes.push(r),
            }
        }

        let name = self.parse_word(false);

        while let Some(thing) = self
            .parse_redirection()
            .map(CmdSuffix::Redirection)
            .or_else(|| self.parse_word(false).map(CmdSuffix::Word))
        {
            match thing {
                r @ CmdSuffix::Redirection(_) => suffixes.push(r),
                w @ CmdSuffix::Word(_) => suffixes.push(w),
            }
        }

        if name.is_none() && prefixes.is_empty() && suffixes.is_empty() {
            *self = initial;
            None
        } else {
            Some(SimpleCommand {
                name,
                prefixes,
                suffixes,
            })
        }
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
        self.consume_single(SemanticToken::Reserved(ReservedWord::Bang))
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
            Some(SemanticToken::Reserved(word)) => {
                s.push_str(&word.to_string());
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
            whitespace, raw, ..
        }) = self.parse_word(false)
        {
            if let Some((lhs, rhs)) = raw.split_once('=') {
                if is_name(lhs) {
                    let var_assg = VariableAssignment::new(
                        lhs,
                        if rhs.is_empty() {
                            None
                        } else {
                            Some(Word::new(rhs, ""))
                        },
                        whitespace,
                    );
                    return Some(var_assg);
                }
            }
        }

        *self = initial;
        None
    }
}

fn is_name(input: impl AsRef<str>) -> bool {
    let mut input = input.as_ref().chars().peekable();
    match input.peek() {
        Some('0'..='9') => false,
        None => false,
        _ => input.all(|c| c == '_' || matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z')),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    pub program: Vec<CompleteCommand>,
    pub unparsed: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompleteCommand {
    pub list: List,
    pub separator: Option<Separator>,
    pub comment: Option<(LeadingWhitespace, String)>,
}

impl CompleteCommand {
    pub fn list_with_separator(&self) -> Vec<(&AndOrList, Separator)> {
        let mut items = Vec::new();

        let final_separator = match &self.separator {
            Some(separator) => separator.clone(),
            None => Default::default(),
        };

        if self.list.rest.is_empty() {
            items.push((&self.list.first, final_separator));
        } else {
            let mut prev_list = &self.list.first;

            for (sep, and_or_list) in &self.list.rest {
                items.push((prev_list, sep.clone()));
                prev_list = and_or_list;
            }

            if let Some((_, and_or_list)) = self.list.rest.last() {
                items.push((and_or_list, final_separator));
            }
        }

        items
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct List {
    pub first: AndOrList,
    pub rest: Vec<(Separator, AndOrList)>,
}

// foo
//
// foo;
//
// foo &
//
// foo && bar
//
// foo && bar &
//
// foo & bar ; baz && quux ;

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
    pub bang: Option<LeadingWhitespace>,
    pub first: Command,
    pub rest: Vec<(LeadingWhitespace, Command)>,
}

impl Pipeline {
    /// Always at least one in length, since this joins self.first and self.rest.
    pub fn pipeline(&self) -> Vec<&Command> {
        let mut v = vec![&self.first];
        for (_, cmd) in &self.rest {
            v.push(cmd);
        }
        v
    }

    pub fn has_bang(&self) -> bool {
        self.bang.is_some()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Simple(SimpleCommand),
    Compound(CompoundCommand),
    FunctionDefinition(FunctionDefinition),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    pub name: Option<Word>,
    pub prefixes: Vec<CmdPrefix>,
    pub suffixes: Vec<CmdSuffix>,
}

impl SimpleCommand {
    pub fn name(&self) -> Option<&String> {
        if let Some(word) = &self.name {
            Some(&word.name)
        } else {
            None
        }
    }

    pub fn args(&self) -> impl Iterator<Item = &String> {
        self.suffixes
            .iter()
            .filter_map(|m| match m {
                CmdSuffix::Word(w) => Some(w),
                _ => None,
            })
            .map(|w| &w.name)
    }

    pub fn assignments(&self) -> impl Iterator<Item = &VariableAssignment> {
        self.prefixes.iter().filter_map(|m| match m {
            CmdPrefix::Assignment(a) => Some(a),
            _ => None,
        })
    }

    pub fn redirections(&self) -> impl Iterator<Item = &Redirection> {
        self.prefixes
            .iter()
            .filter_map(|m| match m {
                CmdPrefix::Redirection(r) => Some(r),
                _ => None,
            })
            .chain(self.suffixes.iter().filter_map(|m| match m {
                CmdSuffix::Redirection(r) => Some(r),
                _ => None,
            }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CmdPrefix {
    Redirection(Redirection),
    Assignment(VariableAssignment),
}

#[derive(Debug, PartialEq, Eq)]
pub enum CmdSuffix {
    Redirection(Redirection),
    Word(Word),
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
    pub lhs: String,
    pub rhs: Option<Word>,
    pub whitespace: LeadingWhitespace,
}

impl VariableAssignment {
    pub fn new(lhs: &str, rhs: Option<Word>, whitespace: impl Into<LeadingWhitespace>) -> Self {
        Self {
            lhs: lhs.to_string(),
            rhs,
            whitespace: whitespace.into(),
        }
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
        let raw = input.to_string();
        let expanded = Self::expand(input);
        let quote_removed = Self::do_quote_removal(&expanded);

        Self {
            raw,
            name: quote_removed,
            whitespace: whitespace.into(),
            expansions: Default::default(),
        }
    }

    fn do_quote_removal(input: &str) -> String {
        #[derive(PartialEq, Clone, Copy)]
        enum State {
            InSingleQuote,
            InDoubleQuote,
            None,
        }

        let mut s = String::new();
        let mut state = State::None;
        let mut is_escaped = false;

        for c in input.chars() {
            match (c, state) {
                ('\'', State::InSingleQuote) => {
                    state = State::None;
                }
                ('\'', State::None) => {
                    state = State::InSingleQuote;
                }
                (c, State::InSingleQuote) => s.push(c),

                ('"', State::InDoubleQuote) if !is_escaped => {
                    state = State::None;
                }
                ('"', State::None) => {
                    state = State::InDoubleQuote;
                }

                (c, State::InDoubleQuote) if !is_escaped => {
                    s.push(c);
                }

                ('\\', _) if !is_escaped => is_escaped = true,

                (c, _) => {
                    s.push(c);
                    is_escaped = false;
                }
            }
        }

        s
    }

    fn expand(input: &str) -> String {
        // TODO: expand
        Self::expand_tilde(input).to_string()
    }

    fn expand_tilde(input: &str) -> Cow<str> {
        if !matches!(input.chars().next(), Some('~')) {
            return Cow::Borrowed(input);
        }

        let slash_index = match Self::find_unquoted(input, '/') {
            Some(index) => index,
            None => input.len(),
        };

        let pre = &input[..slash_index];

        let expanded = if pre.len() > 1 && path::is_portable_filename(&pre[1..slash_index]) {
            // FIXME: the tilde-prefix shall be replaced by a pathname
            //        of the initial working directory associated with
            //        the login name obtained using the getpwnam()
            //        function as defined in the System Interfaces
            //        volume of POSIX.1-2017
            format!("/home/{}", &pre[1..slash_index])
        } else if pre.len() > 1 {
            return Cow::Borrowed(input);
        } else {
            path::home_dir()
        };

        Cow::Owned(input.replacen(pre, &expanded, 1))
    }

    fn find_unquoted(haystack: &str, needle: char) -> Option<usize> {
        #[derive(PartialEq, Clone, Copy)]
        enum State {
            InSingleQuote,
            InDoubleQuote,
            None,
        }

        let mut state = State::None;
        let mut is_escaped = false;

        let mut found = None;

        for (i, c) in haystack.chars().enumerate() {
            match (c, state) {
                ('\'', State::InSingleQuote) => {
                    state = State::None;
                }
                ('\'', State::None) => {
                    state = State::InSingleQuote;
                }

                ('"', State::InDoubleQuote) if !is_escaped => {
                    state = State::None;
                }
                ('"', State::None) => {
                    state = State::InDoubleQuote;
                }

                ('\\', _) if !is_escaped => is_escaped = true,

                (c, _) => {
                    if state == State::None && c == needle {
                        found = Some(i);
                    }
                    is_escaped = false;
                }
            }
        }

        found
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
pub struct CompoundList {
    pub term: Term,
    pub separator: Option<Separator>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Term {
    pub first: AndOrList,
    pub rest: Vec<(Separator, AndOrList)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundCommand {
    //  true: (cmd; cmd)
    //  false: { cmd; cmd; }
    pub subshell: bool,
    pub first: Box<Command>,
    pub rest: Vec<Command>,
}

/// brace_group  : Lbrace compound_list Rbrace
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct BraceGroup {
    pub list: CompoundList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: String,
    pub commands: CompoundCommand,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Separator {
    Sync(LeadingWhitespace),
    Async(LeadingWhitespace),
}

impl Separator {
    pub fn is_sync(&self) -> bool {
        match self {
            Self::Sync(_) => true,
            Self::Async(_) => false,
        }
    }

    pub fn is_async(&self) -> bool {
        !self.is_sync()
    }
}

impl Default for Separator {
    fn default() -> Self {
        Self::Sync(Default::default())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And(LeadingWhitespace),
    Or(LeadingWhitespace),
}
