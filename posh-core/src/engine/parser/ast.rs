use std::{iter::Peekable, ops::RangeInclusive};

use super::consumer::Consumer;
use super::semtok::{Keyword, SemanticToken, SemanticTokenizer};
use super::tok::Tokenizer;

use crate::Result;

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    pub program: Vec<CompleteCommand>,
}

pub fn parse<A>(input: A) -> Result<SyntaxTree>
where
    A: AsRef<str>,
{
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

        while let Some(thing) = self.parse_complete_command() {
            complete_commands.push(thing);
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

    fn swallow_whitespace(&mut self) -> String;
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

        Some(List { first, rest })
    }

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

    fn parse_pipeline(&mut self) -> Option<Pipeline> {
        let initial = self.clone();

        let negate = self
            .consume_single(SemanticToken::Keyword(Keyword::Bang))
            .is_some();

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

        Some(Pipeline {
            negate,
            first,
            rest,
        })
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
            println!("prefix: {:?}", thing);
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
                println!("NONE");
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

    fn swallow_whitespace(&mut self) -> String {
        let mut s = String::new();
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
                return Some(Word::new("", ws));
            }

            Some(SemanticToken::Word(word)) if word.len() == 1 => match word.chars().nth(0) {
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
pub struct CompleteCommand {
    pub list: List,
    pub separator: Option<Separator>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct List {
    pub first: AndOrList,
    pub rest: Vec<(Separator, AndOrList)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AndOrList {
    pub first: Pipeline,
    pub rest: Vec<(LogicalOp, Pipeline)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pipeline {
    negate: bool,
    pub first: Command,
    rest: Vec<(String, Command)>,
}

impl Pipeline {
    pub fn all(&self) -> &[Command] {
        todo!()
        // let mut copy = &self.rest[..];
        // copy.insert(0, self.first);
        // copy
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
    name: Word,
    prefixes: Vec<SimpleCommandMeta>,
    suffixes: Vec<SimpleCommandMeta>,
}

impl SimpleCommand {
    pub fn name(&self) -> &String {
        &self.name.name
    }

    pub fn args<'a>(&'a self) -> impl Iterator<Item = &'a String> {
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
    lhs: Word,
    rhs: Option<Word>,
}

impl VariableAssignment {
    pub fn new(lhs: Word, rhs: Option<Word>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Word {
    raw: String,
    name: String,
    whitespace: String,
    expansions: Vec<Expansion>,
}

impl Word {
    pub fn new(input: &str, whitespace: impl ToString) -> Self {
        let quote_removed = Self::do_quote_removal(input);
        let expanded = Self::expand(quote_removed);

        Self {
            raw: input.to_string(),
            name: expanded.to_string(),
            whitespace: whitespace.to_string(),
            expansions: Default::default(),
        }
    }

    fn do_quote_removal(input: &str) -> &str {
        // TODO: do quote removal
        input
    }

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
    subshell: bool,
    first: Box<Command>,
    rest: Vec<Command>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition {
    name: String,
    commands: CompoundCommand,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    Sync(String),
    Async(String),
}

mod reconstruct {
    use super::AndOrList;
    use super::Command;
    use super::CompleteCommand;
    use super::List;
    use super::Pipeline;
    use super::Redirection;
    use super::SimpleCommand;
    use super::SimpleCommandMeta;
    use super::SyntaxTree;
    use super::VariableAssignment;
    use super::Word;

    impl ToString for SyntaxTree {
        fn to_string(&self) -> String {
            // FIXME: save amount of new lines?
            self.program
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        }
    }

    impl ToString for CompleteCommand {
        fn to_string(&self) -> String {
            let mut s = self.list.to_string();
            if let Some(sep) = &self.separator {
                s.push_str(&sep.to_string());
            }
            s
        }
    }

    impl ToString for List {
        fn to_string(&self) -> String {
            let mut list = self.first.to_string();
            for (sep, and_or_list) in &self.rest {
                list.push_str(&sep.to_string());
                list.push_str(&and_or_list.to_string());
            }
            list
        }
    }

    impl ToString for AndOrList {
        fn to_string(&self) -> String {
            let mut list = self.first.to_string();
            for (op, pipeline) in &self.rest {
                list.push_str(&op.to_string());
                list.push_str(&pipeline.to_string());
            }
            list
        }
    }

    impl ToString for Pipeline {
        fn to_string(&self) -> String {
            let mut pipeline = self.first.to_string();

            for (ws, cmd) in &self.rest {
                pipeline.push_str(ws);
                pipeline.push('|');
                pipeline.push_str(&cmd.to_string());
            }

            if self.negate {
                pipeline.insert(0, '!');
            }

            pipeline
        }
    }

    impl ToString for Command {
        fn to_string(&self) -> String {
            match self {
                Command::Simple(s) => s.to_string(),
                Command::Compound(c) => c.to_string(),
                Command::FunctionDefinition(f) => f.to_string(),
            }
        }
    }

    impl ToString for SimpleCommand {
        fn to_string(&self) -> String {
            let mut s = String::new();
            for prefix in &self.prefixes {
                s += &prefix.to_string();
            }
            s += &self.name.to_string();
            for suffix in &self.suffixes {
                s += &suffix.to_string();
            }
            s
        }
    }

    impl ToString for SimpleCommandMeta {
        fn to_string(&self) -> String {
            match self {
                Self::Word(w) => w.to_string(),
                Self::Redirection(r) => r.to_string(),
                Self::Assignment(a) => a.to_string(),
            }
        }
    }

    impl ToString for Redirection {
        fn to_string(&self) -> String {
            match self {
                Redirection::Input {
                    file_descriptor,
                    target,
                } => format!("{}<{}", file_descriptor.to_string(), target.to_string()),
                Redirection::Output {
                    file_descriptor,
                    append,
                    target,
                } => format!(
                    "{}>{}{}",
                    file_descriptor.to_string(),
                    if *append {
                        ">".to_string()
                    } else {
                        "".to_string()
                    },
                    target.to_string()
                ),
                Redirection::HereDocument {
                    file_descriptor,
                    delimiter,
                } => format!("{}<<{}", file_descriptor.to_string(), delimiter.to_string()),
            }
        }
    }

    impl ToString for VariableAssignment {
        fn to_string(&self) -> String {
            format!(
                "{}={}",
                self.lhs.to_string(),
                match &self.rhs {
                    Some(rhs) => rhs.to_string(),
                    None => "".to_string(),
                }
            )
        }
    }

    impl ToString for Word {
        fn to_string(&self) -> String {
            format!("{}{}", self.whitespace, self.raw)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And(String),
    Or(String),
}

impl ToString for CompoundCommand {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for FunctionDefinition {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for Separator {
    fn to_string(&self) -> String {
        match self {
            Separator::Sync(ws) => format!("{ws};"),
            Separator::Async(ws) => format!("{ws}&"),
        }
    }
}

impl ToString for LogicalOp {
    fn to_string(&self) -> String {
        match self {
            LogicalOp::And(ws) => format!("{ws}&&"),
            LogicalOp::Or(ws) => format!("{ws}||"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::semtok::SemanticTokenizer;
    use super::super::tok::Tokenizer;
    use super::*;

    fn parse(
        input: &str,
    ) -> Peekable<impl Iterator<Item = SemanticToken> + Clone + std::fmt::Debug> {
        input
            .chars()
            .peekable()
            .tokenize()
            .into_iter()
            .peekable()
            .tokenize()
            .into_iter()
            .peekable()
    }

    #[test]
    fn parse_variable_assignment() {
        let mut tokens = parse("  foo='bar baz'");
        let actual = tokens.parse_variable_assignment();
        let expected =
            VariableAssignment::new(Word::new("foo", "  "), Some(Word::new("'bar baz'", "")));
        assert_eq!(Some(expected), actual);

        let mut tokens = parse(" foo=bar\\ baz");
        let actual = tokens.parse_variable_assignment();
        let expected =
            VariableAssignment::new(Word::new("foo", " "), Some(Word::new("bar\\ baz", "")));
        assert_eq!(Some(expected), actual);

        let mut tokens = parse(r#"foo="bar baz""#);
        let actual = tokens.parse_variable_assignment();
        let expected =
            VariableAssignment::new(Word::new("foo", ""), Some(Word::new("\"bar baz\"", "")));
        assert_eq!(Some(expected), actual);

        let mut tokens = parse("foo=");
        let actual = tokens.parse_variable_assignment();
        let expected = VariableAssignment::new(Word::new("foo", ""), None);
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn parse_redirect_output() {
        for (item, ws) in vec!["  2>/dev/null", "  2> /dev/null"]
            .iter()
            .zip(["", " "])
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected =
                Redirection::new_output(Word::new("2", "  "), Word::new("/dev/null", ws), false);
            assert_eq!(Some(expected), actual);
        }

        for (item, ws) in vec![" >>'foo bar baz'", " >> 'foo bar baz'"]
            .iter()
            .zip(["", " "])
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected =
                Redirection::new_output(Word::new("", " "), Word::new("'foo bar baz'", ws), true);
            assert_eq!(Some(expected), actual);
        }

        for (item, ws) in vec!["2>>'foo bar baz'", "2>> 'foo bar baz'"]
            .iter()
            .zip(["", " "])
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected =
                Redirection::new_output(Word::new("2", ""), Word::new("'foo bar baz'", ws), true);
            assert_eq!(Some(expected), actual);
        }

        let mut tokens = parse(">><");
        let actual = tokens.parse_redirection();
        assert!(actual.is_none());
        assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());
        assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());
        assert_eq!(Some(SemanticToken::RedirectInput), tokens.next());
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_redirect_input_and_here_document() {
        for (item, ws) in vec![r#"  2<"foo.txt""#, r#"  2< "foo.txt""#]
            .iter()
            .zip(["", " "].iter())
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected =
                Redirection::new_input(Word::new("2", "  "), Word::new(r#""foo.txt""#, ws));
            assert_eq!(Some(expected), actual);
        }

        for (item, ws) in vec![r#" 2<<"EOF""#, r#" 2<< "EOF""#]
            .iter()
            .zip(["", " "].iter())
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected =
                Redirection::new_here_doc(Word::new("2", " "), Word::new(r#""EOF""#, ws));
            assert_eq!(Some(expected), actual);
        }

        for (item, ws) in vec![r#"<<"EOF""#, r#"<< "EOF""#]
            .iter()
            .zip(["", " "].iter())
        {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::new_here_doc(Word::new("", ""), Word::new(r#""EOF""#, ws));
            assert_eq!(Some(expected), actual);
        }
    }

    #[test]
    fn parse_simple_command() {
        let mut tokens = parse("echo");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Word::new("echo", ""),
            prefixes: Vec::new(),
            suffixes: Vec::new(),
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("   echo foo bar baz");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Word::new("echo", "   "),
            prefixes: Vec::new(),
            suffixes: vec![
                SimpleCommandMeta::Word(Word::new("foo", " ")),
                SimpleCommandMeta::Word(Word::new("bar", " ")),
                SimpleCommandMeta::Word(Word::new("baz", " ")),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens =
            parse("foo='bar baz' 3>foo bar=yo echo 4</dev/null foo 2>> stderr.log bar baz");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Word::new("echo", " "),
            prefixes: vec![
                SimpleCommandMeta::Assignment(VariableAssignment::new(
                    Word::new("foo", ""),
                    Some(Word::new("'bar baz'", "")),
                )),
                SimpleCommandMeta::Redirection(Redirection::new_output(
                    Word::new("3", " "),
                    Word::new("foo", ""),
                    false,
                )),
                SimpleCommandMeta::Assignment(VariableAssignment::new(
                    Word::new("bar", " "),
                    Some(Word::new("yo", "")),
                )),
            ],
            suffixes: vec![
                SimpleCommandMeta::Redirection(Redirection::new_input(
                    Word::new("4", " "),
                    Word::new("/dev/null", ""),
                )),
                SimpleCommandMeta::Word(Word::new("foo", " ")),
                SimpleCommandMeta::Redirection(Redirection::new_output(
                    Word::new("2", " "),
                    Word::new("stderr.log", " "),
                    true,
                )),
                SimpleCommandMeta::Word(Word::new("bar", " ")),
                SimpleCommandMeta::Word(Word::new("baz", " ")),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("foo=bar echo bar=baz");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Word::new("echo", " "),
            prefixes: vec![SimpleCommandMeta::Assignment(VariableAssignment::new(
                Word::new("foo", ""),
                Some(Word::new("bar", "")),
            ))],
            suffixes: vec![SimpleCommandMeta::Word(Word::new("bar=baz", " "))],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_simple_pipeline() {
        let mut tokens = parse("echo foo 2>/dev/null|rev 2< file | cat");
        let actual = tokens.parse_pipeline();

        let expected = Pipeline {
            negate: false,

            first: Command::Simple(SimpleCommand {
                name: Word::new("echo", ""),
                prefixes: Vec::new(),
                suffixes: vec![
                    SimpleCommandMeta::Word(Word::new("foo", " ")),
                    SimpleCommandMeta::Redirection(Redirection::new_output(
                        Word::new("2", " "),
                        Word::new("/dev/null", ""),
                        false,
                    )),
                ],
            }),

            rest: vec![
                (
                    "".to_string(),
                    Command::Simple(SimpleCommand {
                        name: Word::new("rev", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Redirection(Redirection::new_input(
                            Word::new("2", " "),
                            Word::new("file", " "),
                        ))],
                    }),
                ),
                (
                    " ".to_string(),
                    Command::Simple(SimpleCommand {
                        name: Word::new("cat", " "),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    }),
                ),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_simple_and_or_list() {
        let mut tokens = parse("foo && bar | rev || baz");
        let actual = tokens.parse_and_or_list();

        let expected = AndOrList {
            first: Pipeline {
                first: Command::Simple(SimpleCommand {
                    name: Word::new("foo", ""),
                    prefixes: Vec::new(),
                    suffixes: Vec::new(),
                }),
                negate: false,
                rest: Vec::new(),
            },
            rest: vec![
                (
                    LogicalOp::And(" ".to_string()),
                    Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("bar", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: vec![(
                            " ".to_string(),
                            Command::Simple(SimpleCommand {
                                name: Word::new("rev", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                        )],
                    },
                ),
                (
                    LogicalOp::Or(" ".to_string()),
                    Pipeline {
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("baz", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        negate: false,
                        rest: Vec::new(),
                    },
                ),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_simple_list() {
        let mut tokens = parse("true && foo || bar & baz; quux | rev");
        let actual = tokens.parse_list();

        let expected = List {
            first: AndOrList {
                first: Pipeline {
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("true", ""),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    }),
                    rest: Vec::new(),
                    negate: false,
                },
                rest: vec![
                    (
                        LogicalOp::And(" ".to_string()),
                        Pipeline {
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("foo", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: Vec::new(),
                            negate: false,
                        },
                    ),
                    (
                        LogicalOp::Or(" ".to_string()),
                        Pipeline {
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("bar", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: Vec::new(),
                            negate: false,
                        },
                    ),
                ],
            },
            rest: vec![
                (
                    Separator::Async(" ".to_string()),
                    AndOrList {
                        first: Pipeline {
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("baz", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: Vec::new(),
                            negate: false,
                        },
                        rest: Vec::new(),
                    },
                ),
                (
                    Separator::Sync("".to_string()),
                    AndOrList {
                        first: Pipeline {
                            negate: false,
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("quux", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: vec![(
                                " ".to_string(),
                                Command::Simple(SimpleCommand {
                                    name: Word::new("rev", " "),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                }),
                            )],
                        },
                        rest: Vec::new(),
                    },
                ),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_complete_command() {
        let mut tokens = parse("echo foo");
        let actual = tokens.parse_complete_command();

        let expected = CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", ""),
                            prefixes: Vec::new(),
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            separator: None,
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo foo ;");
        let actual = tokens.parse_complete_command();

        let expected = CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", ""),
                            prefixes: Vec::new(),
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            separator: Some(Separator::Sync(" ".to_string())),
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo foo&");
        let actual = tokens.parse_complete_command();

        let expected = CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", ""),
                            prefixes: Vec::new(),
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            separator: Some(Separator::Async("".to_string())),
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo foo& true ;");
        let actual = tokens.parse_complete_command();

        let expected = CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", ""),
                            prefixes: Vec::new(),
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
                rest: vec![(
                    Separator::Async("".to_string()),
                    AndOrList {
                        first: Pipeline {
                            negate: false,
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("true", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: Vec::new(),
                        },
                        rest: Vec::new(),
                    },
                )],
            },
            separator: Some(Separator::Sync(" ".to_string())),
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo foo;true&");
        let actual = tokens.parse_complete_command();

        let expected = CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        negate: false,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", ""),
                            prefixes: Vec::new(),
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
                rest: vec![(
                    Separator::Sync("".to_string()),
                    AndOrList {
                        first: Pipeline {
                            negate: false,
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("true", ""),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                            rest: Vec::new(),
                        },
                        rest: Vec::new(),
                    },
                )],
            },
            separator: Some(Separator::Async("".to_string())),
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    #[test]
    fn ast() {
        let mut tokens = parse("2>&1 echo foo | rev&& exit ||die; sleep 3s  &");
        let ast = tokens.parse();

        let expected = SyntaxTree {
            program: vec![CompleteCommand {
                list: List {
                    first: AndOrList {
                        first: Pipeline {
                            negate: false,
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("echo", " "),
                                prefixes: vec![SimpleCommandMeta::Redirection(
                                    Redirection::new_output(
                                        Word::new("2", ""),
                                        Word::new("&1", ""),
                                        false,
                                    ),
                                )],
                                suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                            }),
                            rest: vec![(
                                " ".to_string(),
                                Command::Simple(SimpleCommand {
                                    name: Word::new("rev", " "),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                }),
                            )],
                        },
                        rest: vec![
                            (
                                LogicalOp::And("".to_string()),
                                Pipeline {
                                    negate: false,
                                    first: Command::Simple(SimpleCommand {
                                        name: Word::new("exit", " "),
                                        prefixes: Vec::new(),
                                        suffixes: Vec::new(),
                                    }),
                                    rest: Vec::new(),
                                },
                            ),
                            (
                                LogicalOp::Or(" ".to_string()),
                                Pipeline {
                                    negate: false,
                                    first: Command::Simple(SimpleCommand {
                                        name: Word::new("die", ""),
                                        prefixes: Vec::new(),
                                        suffixes: Vec::new(),
                                    }),
                                    rest: Vec::new(),
                                },
                            ),
                        ],
                    },
                    rest: vec![(
                        Separator::Sync("".to_string()),
                        AndOrList {
                            first: Pipeline {
                                negate: false,
                                first: Command::Simple(SimpleCommand {
                                    name: Word::new("sleep", " "),
                                    prefixes: Vec::new(),
                                    suffixes: vec![SimpleCommandMeta::Word(Word::new("3s", " "))],
                                }),
                                rest: Vec::new(),
                            },
                            rest: Vec::new(),
                        },
                    )],
                },
                separator: Some(Separator::Async("  ".to_string())),
            }],
        };
        assert_eq!(expected, ast.unwrap());
        assert!(tokens.next().is_none());
    }

    #[test]
    fn parse_word() {
        let mut tokens = parse("  echo");
        let actual = tokens.parse_word(false);
        let expected = Word::new("echo", "  ");
        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse(" 	'echo yo'");
        let actual = tokens.parse_word(false);
        let expected = Word::new("'echo yo'", " 	");
        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse(r#" "echo yo""#);
        let actual = tokens.parse_word(false);
        let expected = Word::new(r#""echo yo""#, " ");
        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse(" echo\\ yo");
        let actual = tokens.parse_word(false);
        let expected = Word::new("echo\\ yo", " ");
        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo");
        let actual = tokens.parse_word(false);
        let expected = Word::new("echo", "");
        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());

        let mut tokens = parse("echo foo");
        let actual = tokens.parse_word(false);
        let expected = Word::new("echo", "");
        assert_eq!(Some(expected), actual);
        assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
        assert_eq!(Some(SemanticToken::Word("foo".to_string())), tokens.next());
        assert!(tokens.next().is_none());

        let mut tokens = parse(">foo");
        let actual = tokens.parse_word(false);
        assert!(actual.is_none());
        assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());

        let mut tokens = parse("  >foo");
        let actual = tokens.parse_word(false);
        assert!(actual.is_none());
        assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
        assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    }

    #[test]
    fn parse_redirection_fd() {
        let mut tokens = parse(" >");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(Some(Word::new("", " ")), actual);

        let mut tokens = parse(">>");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(Some(Word::new("", "")), actual);

        let mut tokens = parse(" <");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(Some(Word::new("", " ")), actual);

        let mut tokens = parse("2>");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(Some(Word::new("2", "")), actual);

        let mut tokens = parse(" 2");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(Some(Word::new("2", " ")), actual);

        let mut tokens = parse("a");
        let actual = tokens.parse_redirection_fd();
        assert_eq!(None, actual);
    }

    #[test]
    fn syntax_tree_back_to_string() {
        let input = "   foo='bar  baz'\\ quux  echo yo hello	2< file && true|cat> foo; hello";
        let mut tokens = parse(input);
        let actual = tokens.parse().unwrap();

        assert_eq!(input.to_string(), actual.to_string());
    }
}
