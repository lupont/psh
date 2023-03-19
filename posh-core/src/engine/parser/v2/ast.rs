use std::{iter::Peekable, ops::RangeInclusive};

use super::consumer::Consumer;
use super::semtok::{Keyword, SemanticToken};

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    commands: Vec<Command>,
}

pub trait Parser: Iterator<Item = SemanticToken> {
    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment>;
    fn parse_redirection(&mut self) -> Option<Redirection>;
    fn parse_word(&mut self) -> Option<Word>;

    fn parse_and_or_list(&mut self) -> Option<AndOrList>;

    fn parse_simple_command(&mut self) -> Option<SimpleCommand>;
    fn parse_pipeline(&mut self) -> Option<Pipeline>;

    fn parse_command(&mut self) -> Option<Command> {
        self.parse_pipeline()
            .map(Command::Pipeline)
            .or_else(|| self.parse_simple_command().map(Command::Simple))
    }

    // NOTE: I really dislike this, but this is the simplest way
    //       I could think of to avoid stack overflows when
    //       parsing a command in a pipeline
    fn parse_command_except_pipeline(&mut self) -> Option<Command> {
        self.parse_simple_command().map(Command::Simple)
    }

    fn swallow_whitespace(&mut self);

    fn parse(&mut self) -> Option<SyntaxTree>;
}

impl<T> Parser for Peekable<T>
where
    T: Iterator<Item = SemanticToken> + Clone + std::fmt::Debug,
{
    fn parse(&mut self) -> Option<SyntaxTree> {
        todo!()
    }

    fn parse_and_or_list(&mut self) -> Option<AndOrList> {
        let first = match self.parse_command() {
            Some(cmd) => cmd,
            None => return None,
        };

        let mut rest = Vec::new();

        self.swallow_whitespace();

        while let Some(thing) = self
            .consume_if(|t| t == &SemanticToken::And || t == &SemanticToken::Or)
            .map(|t| match t {
                SemanticToken::And => LogicalOp::And,
                SemanticToken::Or => LogicalOp::Or,
                _ => unreachable!(),
            })
            .and_then(|op| {
                self.swallow_whitespace();
                self.parse_command().map(|c| (op, c))
            })
        {
            rest.push(thing);
        }

        Some(AndOrList { first, rest })
    }

    fn parse_pipeline(&mut self) -> Option<Pipeline> {
        let initial = self.clone();

        let negate = self
            .consume_single(SemanticToken::Keyword(Keyword::Not))
            .is_some();
        self.swallow_whitespace();

        let first = match self.parse_command_except_pipeline() {
            Some(cmd) => cmd,
            None => {
                *self = initial;
                return None;
            }
        };

        self.swallow_whitespace();

        let mut rest = Vec::new();

        while let Some(cmd) = self.consume_single(SemanticToken::Pipe).and_then(|_| {
            self.swallow_whitespace();
            self.parse_command_except_pipeline()
        }) {
            rest.push(cmd);
        }

        // NOTE: not exactly on-spec, but without this, a simple command
        //       gets parsed as a pipeline
        if rest.is_empty() {
            *self = initial;
            return None;
        }

        Some(Pipeline {
            negate,
            first: Box::new(first),
            rest,
        })
    }

    fn parse_simple_command(&mut self) -> Option<SimpleCommand> {
        let initial = self.clone();

        let mut redirections = Vec::new();
        let mut assignments = Vec::new();
        let mut words = Vec::new();

        enum Valid {
            A(VariableAssignment),
            R(Redirection),
            W(Word),
        }

        while let Some(thing) = self
            .parse_variable_assignment()
            .map(Valid::A)
            .or_else(|| self.parse_redirection().map(Valid::R))
        {
            match thing {
                Valid::A(v) => assignments.push(v),
                Valid::R(r) => redirections.push(r),
                Valid::W(_) => unreachable!(),
            }
            self.swallow_whitespace();
        }

        self.swallow_whitespace();

        let name = match self.parse_word() {
            Some(word) => word,
            None => {
                *self = initial;
                return None;
            }
        };

        self.swallow_whitespace();

        while let Some(thing) = self
            .parse_redirection()
            .map(Valid::R)
            .or_else(|| self.parse_word().map(Valid::W))
        {
            match thing {
                Valid::R(r) => redirections.push(r),
                Valid::W(w) => words.push(w),
                Valid::A(_) => unreachable!(),
            }
            self.swallow_whitespace();
        }

        Some(SimpleCommand {
            redirections,
            assignments,
            name,
            words,
        })
    }

    fn swallow_whitespace(&mut self) {
        while let Some(SemanticToken::Whitespace(_)) = self.peek() {
            self.next();
        }
    }

    fn parse_redirection(&mut self) -> Option<Redirection> {
        let initial = self.clone();

        let fd = match self.peek() {
            Some(SemanticToken::Word(s)) => {
                let word = Word::new(s);
                self.next();
                word
            }
            _ => Word::new("0"),
        };

        match self
            .consume_single(SemanticToken::RedirectOutput)
            .or_else(|| self.consume_single(SemanticToken::RedirectInput))
        {
            Some(SemanticToken::RedirectInput) => {
                let is_here_doc = self.consume_single(SemanticToken::RedirectInput).is_some();

                self.swallow_whitespace();

                match self.peek() {
                    Some(SemanticToken::Word(s)) => {
                        let target = Word::new(s);
                        self.next();
                        Some(if is_here_doc {
                            Redirection::HereDocument {
                                file_descriptor: fd,
                                delimiter: target,
                            }
                        } else {
                            Redirection::Input {
                                file_descriptor: fd,
                                target,
                            }
                        })
                    }

                    _ => {
                        *self = initial;
                        None
                    }
                }
            }

            Some(SemanticToken::RedirectOutput) => {
                let append = self.consume_single(SemanticToken::RedirectOutput).is_some();

                self.swallow_whitespace();

                let target = match self.consume_if(|t| matches!(t, SemanticToken::Word(_))) {
                    Some(SemanticToken::Word(s)) => s,
                    Some(_) => unreachable!(),
                    None => {
                        *self = initial;
                        return None;
                    }
                };

                Some(Redirection::Output {
                    file_descriptor: fd,
                    append,
                    target: Word::new(&target),
                })
            }

            _ => {
                *self = initial;
                None
            }
        }
    }

    fn parse_word(&mut self) -> Option<Word> {
        match self.peek() {
            Some(SemanticToken::Word(s)) => {
                let word = Word::new(s);
                self.next();
                Some(word)
            }
            _ => None,
        }
    }

    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment> {
        if let Some(SemanticToken::Word(s)) = self.peek() {
            if let Some((lhs, rhs)) = s.split_once('=') {
                let var_assg =
                    VariableAssignment::new(lhs, if rhs.is_empty() { None } else { Some(rhs) });
                self.next();
                return Some(var_assg);
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Simple(SimpleCommand),
    Pipeline(Pipeline),
    List(List),
    CompoundCommand(CompoundCommand),
    FunctionDefinition(FunctionDefinition),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    name: Word,
    words: Vec<Word>,
    redirections: Vec<Redirection>,
    assignments: Vec<VariableAssignment>,
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
    pub fn new_output(fd: &str, target: &str, append: bool) -> Self {
        Self::Output {
            file_descriptor: Word::new(fd),
            append,
            target: Word::new(target),
        }
    }

    pub fn new_input(fd: &str, target: &str) -> Self {
        Self::Input {
            file_descriptor: Word::new(fd),
            target: Word::new(target),
        }
    }

    pub fn new_here_doc(fd: &str, delimiter: &str) -> Self {
        Self::HereDocument {
            file_descriptor: Word::new(fd),
            delimiter: Word::new(delimiter),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableAssignment {
    lhs: Word,
    rhs: Option<Word>,
}

impl VariableAssignment {
    pub fn new(lhs: &str, rhs: Option<&str>) -> Self {
        Self {
            lhs: Word::new(lhs),
            rhs: rhs.map(Word::new),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Word {
    name: String,
    expansions: Vec<Expansion>,
}

impl Word {
    pub fn new(input: &str) -> Self {
        let input = Self::do_quote_removal(input);
        let input = Self::expand(input);

        Self {
            name: input.to_string(),
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
pub struct Pipeline {
    negate: bool,
    first: Box<Command>,
    rest: Vec<Command>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct List {
    first: Box<AndOrList>,
    rest: Vec<(Separator, AndOrList)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AndOrList {
    first: Command,
    rest: Vec<(LogicalOp, Command)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    Sync,
    Async,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

#[cfg(test)]
mod tests {
    use super::super::semtok::SemanticTokenizer;
    use super::super::tokenizer::Tokenizer;
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
        let mut tokens = parse("foo='bar baz'");
        let actual = tokens.parse_variable_assignment();
        let expected = VariableAssignment::new("foo", Some("'bar baz'"));
        assert_eq!(Some(expected), actual);

        let mut tokens = parse("foo=bar\\ baz");
        let actual = tokens.parse_variable_assignment();
        let expected = VariableAssignment::new("foo", Some("bar\\ baz"));
        assert_eq!(Some(expected), actual);

        let mut tokens = parse(r#"foo="bar baz""#);
        let actual = tokens.parse_variable_assignment();
        let expected = VariableAssignment::new("foo", Some("\"bar baz\""));
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn parse_redirect_output() {
        for item in vec!["2>/dev/null", "2> /dev/null"] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::Output {
                file_descriptor: Word::new("2"),
                append: false,
                target: Word::new("/dev/null"),
            };
            assert_eq!(Some(expected), actual);
        }

        for item in vec![">>'foo bar baz'", ">> 'foo bar baz'"] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::Output {
                file_descriptor: Word::new("0"),
                append: true,
                target: Word::new("'foo bar baz'"),
            };
            assert_eq!(Some(expected), actual);
        }

        for item in vec!["2>>'foo bar baz'", "2>> 'foo bar baz'"] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::Output {
                file_descriptor: Word::new("2"),
                append: true,
                target: Word::new("'foo bar baz'"),
            };
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
        for item in vec![r#"2<"foo.txt""#, r#"2< "foo.txt""#] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::Input {
                file_descriptor: Word::new("2"),
                target: Word::new(r#""foo.txt""#),
            };
            assert_eq!(Some(expected), actual);
        }

        for item in vec![r#"2<<"EOF""#, r#"2<< "EOF""#] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::HereDocument {
                file_descriptor: Word::new("2"),
                delimiter: Word::new(r#""EOF""#),
            };
            assert_eq!(Some(expected), actual);
        }
    }

    #[test]
    fn parse_simple_command() {
        let mut tokens =
            parse("foo='bar baz' 3>foo bar=yo echo 4</dev/null foo 2>> stderr.log bar baz");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Word::new("echo"),
            words: vec![Word::new("foo"), Word::new("bar"), Word::new("baz")],
            redirections: vec![
                Redirection::new_output("3", "foo", false),
                Redirection::new_input("4", "/dev/null"),
                Redirection::new_output("2", "stderr.log", true),
            ],
            assignments: vec![
                VariableAssignment::new("foo", Some("'bar baz'")),
                VariableAssignment::new("bar", Some("yo")),
            ],
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

            first: Box::new(Command::Simple(SimpleCommand {
                name: Word::new("echo"),
                words: vec![Word::new("foo")],
                redirections: vec![Redirection::new_output("2", "/dev/null", false)],
                assignments: Vec::new(),
            })),

            rest: vec![
                Command::Simple(SimpleCommand {
                    name: Word::new("rev"),
                    words: Vec::new(),
                    redirections: vec![Redirection::new_input("2", "file")],
                    assignments: Vec::new(),
                }),
                Command::Simple(SimpleCommand {
                    name: Word::new("cat"),
                    words: Vec::new(),
                    redirections: Vec::new(),
                    assignments: Vec::new(),
                }),
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
            first: Command::Simple(SimpleCommand {
                name: Word::new("foo"),
                words: Vec::new(),
                redirections: Vec::new(),
                assignments: Vec::new(),
            }),
            rest: vec![
                (
                    LogicalOp::And,
                    Command::Pipeline(Pipeline {
                        negate: false,
                        first: Box::new(Command::Simple(SimpleCommand {
                            name: Word::new("bar"),
                            words: Vec::new(),
                            redirections: Vec::new(),
                            assignments: Vec::new(),
                        })),
                        rest: vec![Command::Simple(SimpleCommand {
                            name: Word::new("rev"),
                            words: Vec::new(),
                            redirections: Vec::new(),
                            assignments: Vec::new(),
                        })],
                    }),
                ),
                (
                    LogicalOp::Or,
                    Command::Simple(SimpleCommand {
                        name: Word::new("baz"),
                        words: Vec::new(),
                        redirections: Vec::new(),
                        assignments: Vec::new(),
                    }),
                ),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }
}
