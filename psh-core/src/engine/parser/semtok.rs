use std::iter::Peekable;

use crate::engine::parser::consumer::Consumer;
use crate::engine::parser::tok::{Token, Tokenizer};

pub fn lex(input: impl AsRef<str>) -> Vec<SemanticToken> {
    input
        .as_ref()
        .chars()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .tokenize()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticToken {
    Word(String),
    Reserved(ReservedWord),
    Whitespace(char),
    CmdSubStart,
    ArithmeticStart,
    DoubleQuote,
    SingleQuote,
    Equals,
    Dollar,
    Backslash,
    And,
    Or,
    SyncSeparator,
    AsyncSeparator,
    Pipe,
    RedirectInput,
    RedirectOutput,
    LParen,
    RParen,
    Comment(String),
}

impl ToString for SemanticToken {
    fn to_string(&self) -> String {
        match self {
            Self::Word(word) => word.to_string(),
            Self::Reserved(reserved_word) => reserved_word.as_ref().to_string(),
            Self::Whitespace(ws) => ws.to_string(),
            Self::CmdSubStart => "$(".to_string(),
            Self::ArithmeticStart => "$((".to_string(),
            Self::DoubleQuote => "\"".to_string(),
            Self::SingleQuote => "'".to_string(),
            Self::Equals => "=".to_string(),
            Self::Dollar => "$".to_string(),
            Self::Backslash => "\\".to_string(),
            Self::And => "&&".to_string(),
            Self::Or => "||".to_string(),
            Self::SyncSeparator => ";".to_string(),
            Self::AsyncSeparator => "&".to_string(),
            Self::Pipe => "|".to_string(),
            Self::RedirectInput => "<".to_string(),
            Self::RedirectOutput => ">".to_string(),
            Self::LParen => "(".to_string(),
            Self::RParen => ")".to_string(),
            Self::Comment(comment) => format!("#{comment}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReservedWord {
    Bang,
    LBrace,
    RBrace,
    Case,
    Do,
    Done,
    Elif,
    Else,
    Esac,
    Fi,
    For,
    If,
    In,
    Then,
    Until,
    While,
}

impl AsRef<str> for ReservedWord {
    fn as_ref(&self) -> &str {
        match self {
            ReservedWord::Bang => "!",
            ReservedWord::LBrace => "{",
            ReservedWord::RBrace => "}",
            ReservedWord::Case => "case",
            ReservedWord::Do => "do",
            ReservedWord::Done => "done",
            ReservedWord::Elif => "elif",
            ReservedWord::Else => "else",
            ReservedWord::Esac => "esac",
            ReservedWord::Fi => "fi",
            ReservedWord::For => "for",
            ReservedWord::If => "if",
            ReservedWord::In => "in",
            ReservedWord::Then => "then",
            ReservedWord::Until => "until",
            ReservedWord::While => "while",
        }
    }
}

pub trait SemanticTokenizer: Iterator<Item = Token> {
    fn parse_and(&mut self) -> Option<SemanticToken>;
    fn parse_or(&mut self) -> Option<SemanticToken>;
    fn parse_sync_separator(&mut self) -> Option<SemanticToken>;
    fn parse_async_separator(&mut self) -> Option<SemanticToken>;
    fn parse_pipe(&mut self) -> Option<SemanticToken>;
    fn parse_redirect_input(&mut self) -> Option<SemanticToken>;
    fn parse_redirect_output(&mut self) -> Option<SemanticToken>;
    fn parse_lparen(&mut self) -> Option<SemanticToken>;
    fn parse_rparen(&mut self) -> Option<SemanticToken>;
    fn parse_comment(&mut self) -> Option<SemanticToken>;
    fn parse_whitespace(&mut self) -> Option<SemanticToken>;
    fn parse_cmd_sub_start(&mut self) -> Option<SemanticToken>;
    fn parse_arithmetic_start(&mut self) -> Option<SemanticToken>;
    fn parse_dollar(&mut self) -> Option<SemanticToken>;
    fn parse_backslash(&mut self) -> Option<SemanticToken>;
    fn parse_double_quote(&mut self) -> Option<SemanticToken>;
    fn parse_single_quote(&mut self) -> Option<SemanticToken>;
    fn parse_equals(&mut self) -> Option<SemanticToken>;
    fn parse_word(&mut self) -> Option<SemanticToken>;
    fn parse_reserved_word(&mut self) -> Option<SemanticToken>;

    fn parse(&mut self) -> Option<SemanticToken> {
        self.parse_whitespace()
            .or_else(|| self.parse_and())
            .or_else(|| self.parse_or())
            .or_else(|| self.parse_async_separator())
            .or_else(|| self.parse_sync_separator())
            .or_else(|| self.parse_pipe())
            .or_else(|| self.parse_redirect_input())
            .or_else(|| self.parse_redirect_output())
            .or_else(|| self.parse_lparen())
            .or_else(|| self.parse_rparen())
            .or_else(|| self.parse_comment())
            .or_else(|| self.parse_reserved_word())
            .or_else(|| self.parse_arithmetic_start())
            .or_else(|| self.parse_cmd_sub_start())
            .or_else(|| self.parse_dollar())
            .or_else(|| self.parse_backslash())
            .or_else(|| self.parse_double_quote())
            .or_else(|| self.parse_single_quote())
            .or_else(|| self.parse_equals())
            .or_else(|| self.parse_word())
    }

    fn tokenize(&mut self) -> Vec<SemanticToken> {
        let mut tokens = Vec::new();

        while let Some(token) = self.parse() {
            tokens.push(token);
        }

        tokens
    }
}

impl<T> SemanticTokenizer for Peekable<T>
where
    T: Iterator<Item = Token> + Clone + std::fmt::Debug,
{
    fn parse_sync_separator(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Semicolon)
            .map(|_| SemanticToken::SyncSeparator)
    }

    fn parse_async_separator(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Ampersand)
            .map(|_| SemanticToken::AsyncSeparator)
    }

    fn parse_pipe(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Pipe)
            .map(|_| SemanticToken::Pipe)
    }

    fn parse_redirect_input(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::LAngle)
            .map(|_| SemanticToken::RedirectInput)
    }

    fn parse_redirect_output(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::RAngle)
            .map(|_| SemanticToken::RedirectOutput)
    }

    fn parse_lparen(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::LParen)
            .map(|_| SemanticToken::LParen)
    }

    fn parse_rparen(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::RParen)
            .map(|_| SemanticToken::RParen)
    }

    fn parse_comment(&mut self) -> Option<SemanticToken> {
        if self.consume_single(Token::Pound).is_some() {
            let comment = if let Some(tokens) =
                self.consume_until(|t| matches!(t, Token::Whitespace('\n')))
            {
                tokens
                    .iter()
                    .map(|t| t.to_str())
                    .collect::<Vec<_>>()
                    .join("")
            } else {
                "".to_string()
            };
            return Some(SemanticToken::Comment(comment));
        }
        None
    }

    fn parse_whitespace(&mut self) -> Option<SemanticToken> {
        self.consume_if(|t| matches!(t, Token::Whitespace(_)))
            .map(|t| match t {
                Token::Whitespace(c) => SemanticToken::Whitespace(c),
                _ => unreachable!(),
            })
    }

    fn parse_arithmetic_start(&mut self) -> Option<SemanticToken> {
        self.consume_multiple([Token::Dollar, Token::LParen, Token::LParen])
            .map(|_| SemanticToken::ArithmeticStart)
    }

    fn parse_cmd_sub_start(&mut self) -> Option<SemanticToken> {
        self.consume_multiple([Token::Dollar, Token::LParen])
            .map(|_| SemanticToken::CmdSubStart)
    }

    fn parse_backslash(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Backslash)
            .map(|_| SemanticToken::Backslash)
    }

    fn parse_dollar(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Dollar)
            .map(|_| SemanticToken::Dollar)
    }

    fn parse_double_quote(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::DoubleQuote)
            .map(|_| SemanticToken::DoubleQuote)
    }

    fn parse_single_quote(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::SingleQuote)
            .map(|_| SemanticToken::SingleQuote)
    }

    fn parse_equals(&mut self) -> Option<SemanticToken> {
        self.consume_single(Token::Equals)
            .map(|_| SemanticToken::Equals)
    }

    fn parse_word(&mut self) -> Option<SemanticToken> {
        if !matches!(self.peek(), Some(Token::Backtick | Token::Word(_))) {
            return None;
        }

        let mut word = String::new();

        while let Some(token) = self.peek() {
            match token {
                Token::Word(s) => {
                    word.push_str(s);
                    self.next();
                }

                _ => break,
            }
        }

        if word.is_empty() {
            None
        } else {
            Some(SemanticToken::Word(word))
        }
    }

    fn parse_and(&mut self) -> Option<SemanticToken> {
        self.consume_multiple([Token::Ampersand, Token::Ampersand])
            .map(|_| SemanticToken::And)
    }

    fn parse_or(&mut self) -> Option<SemanticToken> {
        self.consume_multiple([Token::Pipe, Token::Pipe])
            .map(|_| SemanticToken::Or)
    }

    fn parse_reserved_word(&mut self) -> Option<SemanticToken> {
        let mut consume_reserved_word = |s: &str, reserved_word| {
            self.consume_single(Token::Word(s.to_string()))
                .map(|_| SemanticToken::Reserved(reserved_word))
        };

        consume_reserved_word("!", ReservedWord::Bang)
            .or_else(|| consume_reserved_word("{", ReservedWord::LBrace))
            .or_else(|| consume_reserved_word("}", ReservedWord::RBrace))
            .or_else(|| consume_reserved_word("case", ReservedWord::Case))
            .or_else(|| consume_reserved_word("do", ReservedWord::Do))
            .or_else(|| consume_reserved_word("done", ReservedWord::Done))
            .or_else(|| consume_reserved_word("elif", ReservedWord::Elif))
            .or_else(|| consume_reserved_word("else", ReservedWord::Else))
            .or_else(|| consume_reserved_word("esac", ReservedWord::Esac))
            .or_else(|| consume_reserved_word("fi", ReservedWord::Fi))
            .or_else(|| consume_reserved_word("for", ReservedWord::For))
            .or_else(|| consume_reserved_word("if", ReservedWord::If))
            .or_else(|| consume_reserved_word("in", ReservedWord::In))
            .or_else(|| consume_reserved_word("then", ReservedWord::Then))
            .or_else(|| consume_reserved_word("until", ReservedWord::Until))
            .or_else(|| consume_reserved_word("while", ReservedWord::While))
    }
}

#[cfg(test)]
mod tests {
    use super::super::tok::Tokenizer;
    use super::SemanticToken::*;
    use super::*;

    fn test_tokenize(input: &str, expected: Vec<SemanticToken>) {
        let tokens = lex(input);
        assert_eq!(expected, tokens);
    }

    #[test]
    fn parse_basic() {
        let mut input = vec![Token::Pipe, Token::Pipe].into_iter().peekable();
        assert_eq!(Some(Or), input.parse());
        assert!(input.next().is_none());

        let mut input = vec![Token::Whitespace(' ')].into_iter().peekable();
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert!(input.next().is_none());

        let mut input = vec![Token::Pipe, Token::Pipe, Token::Pipe]
            .into_iter()
            .peekable();
        assert_eq!(Some(Or), input.parse());
        assert_eq!(Some(Pipe), input.parse());
        assert!(input.next().is_none());
    }

    #[test]
    fn parse_reserved_word() {
        let all_reserved_words = vec![
            ("!", super::ReservedWord::Bang),
            ("{", super::ReservedWord::LBrace),
            ("}", super::ReservedWord::RBrace),
            ("case", super::ReservedWord::Case),
            ("do", super::ReservedWord::Do),
            ("done", super::ReservedWord::Done),
            ("elif", super::ReservedWord::Elif),
            ("else", super::ReservedWord::Else),
            ("esac", super::ReservedWord::Esac),
            ("fi", super::ReservedWord::Fi),
            ("for", super::ReservedWord::For),
            ("if", super::ReservedWord::If),
            ("in", super::ReservedWord::In),
            ("then", super::ReservedWord::Then),
            ("until", super::ReservedWord::Until),
            ("while", super::ReservedWord::While),
        ];

        for (literal, reserved_word) in all_reserved_words {
            let unquoted_reserved_word = literal.to_string();
            let mut input = vec![Token::Word(unquoted_reserved_word)]
                .into_iter()
                .peekable();
            assert_eq!(Some(Reserved(reserved_word)), input.parse());

            let single_quoted_reserved_word = format!("'{literal}'");
            let mut input = vec![Token::Word(single_quoted_reserved_word)]
                .into_iter()
                .peekable();
            let parsed = input.parse();
            assert!(!matches!(parsed, Some(Reserved(_))));
            assert!(matches!(parsed, Some(Word(_))));

            let double_quoted_reserved_word = format!(r#""{literal}""#);
            let mut input = vec![Token::Word(double_quoted_reserved_word)]
                .into_iter()
                .peekable();
            let parsed = input.parse();
            assert!(!matches!(parsed, Some(Reserved(_))));
            assert!(matches!(parsed, Some(Word(_))));
        }
    }

    #[test]
    fn parse_comment() {
        let mut input = "#this is a comment".chars().peekable();
        let mut tokens = input.tokenize().into_iter().peekable();
        let parsed = tokens.parse();
        assert_eq!(
            Some(SemanticToken::Comment("this is a comment".to_string())),
            parsed
        );
    }

    #[test]
    fn parse_word() {
        let mut input = vec![Token::Word("foo".to_string())].into_iter().peekable();

        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert!(input.next().is_none());

        let mut input = vec![
            Token::SingleQuote,
            Token::Word("foo".to_string()),
            Token::SingleQuote,
        ]
        .into_iter()
        .peekable();

        assert_eq!(Some(SingleQuote), input.parse());
        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert_eq!(Some(SingleQuote), input.parse());
        assert!(input.next().is_none());

        let mut input = vec![
            Token::Word("foo".to_string()),
            Token::SingleQuote,
            Token::Word("bar".to_string()),
            Token::Whitespace(' '),
            Token::Word("baz".to_string()),
            Token::SingleQuote,
            Token::Word("quux".to_string()),
            Token::Backslash,
            Token::Whitespace(' '),
            Token::Word("yo".to_string()),
        ]
        .into_iter()
        .peekable();

        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert_eq!(Some(SingleQuote), input.parse());
        assert_eq!(Some(Word("bar".to_string())), input.parse());
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert_eq!(Some(Word("baz".to_string())), input.parse());
        assert_eq!(Some(SingleQuote), input.parse());
        assert_eq!(Some(Word("quux".to_string())), input.parse());
        assert_eq!(Some(Backslash), input.parse());
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert_eq!(Some(Word("yo".to_string())), input.parse());
        assert!(input.next().is_none());

        let input = "$(echo foo)".chars().peekable().tokenize();
        let mut input = input.into_iter().peekable(); //.parse();
        assert_eq!(Some(CmdSubStart), input.parse());
        assert_eq!(Some(Word("echo".to_string())), input.parse());
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert_eq!(Some(RParen), input.parse());
        // let expected = CmdSubStart;
        // assert_eq!(Some(expected), actual);

        let input = "$(".chars().peekable().tokenize();
        let actual = input.into_iter().peekable().parse();
        let expected = CmdSubStart;
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn tokenize_basic() {
        test_tokenize(
            r#"foo="bar" <file echo $bar 2>> /dev/null"#,
            vec![
                Word("foo".to_string()),
                Equals,
                DoubleQuote,
                Word("bar".to_string()),
                DoubleQuote,
                Whitespace(' '),
                RedirectInput,
                Word("file".to_string()),
                Whitespace(' '),
                Word("echo".to_string()),
                Whitespace(' '),
                Dollar,
                Word("bar".to_string()),
                Whitespace(' '),
                Word("2".to_string()),
                RedirectOutput,
                RedirectOutput,
                Whitespace(' '),
                Word("/dev/null".to_string()),
            ],
        );
    }

    #[test]
    fn tokenize_with_cmd_substitution() {
        test_tokenize(
            "echo $(b $(c $(( d ))) ) separate",
            vec![
                Word("echo".to_string()),
                Whitespace(' '),
                CmdSubStart,
                Word("b".to_string()),
                Whitespace(' '),
                CmdSubStart,
                Word("c".to_string()),
                Whitespace(' '),
                ArithmeticStart,
                Whitespace(' '),
                Word("d".to_string()),
                Whitespace(' '),
                RParen,
                RParen,
                RParen,
                Whitespace(' '),
                RParen,
                Whitespace(' '),
                Word("separate".to_string()),
            ],
        );
    }

    #[test]
    fn tokenize_with_trailing_reserved_word() {
        test_tokenize(
            "echo {",
            vec![
                Word("echo".to_string()),
                Whitespace(' '),
                Reserved(ReservedWord::LBrace),
            ],
        );
    }
}
