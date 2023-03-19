use std::{iter::Peekable, ops::RangeInclusive};

use super::consumer::Consumer;
use super::semtok::SemanticToken;

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    commands: Vec<Command>,
}

pub trait Parser: Iterator<Item = SemanticToken> {
    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment>;
    fn parse_redirection(&mut self) -> Option<Redirection>;
    fn parse_word(&mut self) -> Option<Word>;

    fn parse_simple_command(&mut self) -> Option<SimpleCommand>;

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

    fn parse_simple_command(&mut self) -> Option<SimpleCommand> {
        let initial = self.clone();

        let mut redirections = Vec::new();
        let mut variable_assignments = Vec::new();
        let mut words = Vec::<Word>::new();

        enum Valid {
            R(Redirection),
            V(VariableAssignment),
        }

        while let Some(thing) = self
            .parse_variable_assignment()
            .map(Valid::V)
            .or_else(|| self.parse_redirection().map(Valid::R))
        {
            match thing {
                Valid::R(r) => redirections.push(r),
                Valid::V(v) => variable_assignments.push(v),
            }
            self.swallow_whitespace();
        }

        self.swallow_whitespace();

        match self.parse_word() {
            None => {
                *self = initial;
                None
            }

            Some(word) => {
                let name = word;

                enum Valid {
                    R(Redirection),
                    W(Word),
                }

                self.swallow_whitespace();

                while let Some(thing) = self
                    .parse_redirection()
                    .map(Valid::R)
                    .or_else(|| self.parse_word().map(Valid::W))
                {
                    match thing {
                        Valid::R(redirection) => redirections.push(redirection),
                        Valid::W(word) => words.push(word),
                    }
                    self.swallow_whitespace();
                }

                Some(SimpleCommand {
                    redirections,
                    assignments: variable_assignments,
                    name,
                    words,
                })
            }
        }
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
                Some(var_assg)
            } else {
                None
            }
        } else {
            None
        }
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
    redirections: Vec<Redirection>,
    assignments: Vec<VariableAssignment>,
    name: Word,
    words: Vec<Word>,
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
    rest: Vec<(Op, Command)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    Sync,
    Async,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
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
        for item in vec!["2<\"foo.txt\"", "2< \"foo.txt\""] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::Input {
                file_descriptor: Word::new("2"),
                target: Word::new("\"foo.txt\""),
            };
            assert_eq!(Some(expected), actual);
        }

        for item in vec![r#"2<<"EOF""#, r#"2<< "EOF""#] {
            let mut tokens = parse(item);
            let actual = tokens.parse_redirection();
            let expected = Redirection::HereDocument {
                file_descriptor: Word::new("2"),
                delimiter: Word::new("\"EOF\""),
            };
            assert_eq!(Some(expected), actual);
        }
    }

    fn output_redirection(fd: &str, append: bool, target: &str) -> Redirection {
        Redirection::Output {
            file_descriptor: Word::new(fd),
            append,
            target: Word::new(target),
        }
    }

    fn input_redirection(fd: &str, target: &str) -> Redirection {
        Redirection::Input {
            file_descriptor: Word::new(fd),
            target: Word::new(target),
        }
    }

    fn here_doc(fd: &str, delimiter: &str) -> Redirection {
        Redirection::HereDocument {
            file_descriptor: Word::new(fd),
            delimiter: Word::new(delimiter),
        }
    }

    #[test]
    fn parse_simple_command() {
        let mut tokens = parse("foo='bar baz' 3>foo bar=yo echo 4</dev/null foo 2>> stderr.log bar baz");
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            redirections: vec![
                output_redirection("3", false, "foo"),
                input_redirection("4", "/dev/null"),
                output_redirection("2", true, "stderr.log"),
            ],
            assignments: vec![
                VariableAssignment::new("foo", Some("'bar baz'")),
                VariableAssignment::new("bar", Some("yo")),
            ],
            name: Word::new("echo"),
            words: vec![
                Word::new("foo"),
                Word::new("bar"),
                Word::new("baz"),
            ],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }
}
