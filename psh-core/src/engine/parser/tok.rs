use std::{borrow::Cow, iter::Peekable};

use crate::engine::parser::consumer::Consumer;

pub fn lex(input: impl AsRef<str>) -> Vec<Token> {
    input.as_ref().chars().peekable().tokenize()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    Reserved(ReservedWord),
    Whitespace(char),
    CmdSubStart,
    ArithmeticStart,
    DoubleQuote,
    SingleQuote,
    Equals,
    Dollar,
    QuestionMark,
    Tilde,
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
    Pound,
}

impl Token {
    pub fn as_str(&self) -> Cow<'_, str> {
        use Cow::*;
        match self {
            Self::Word(word) => Borrowed(word.as_ref()),
            Self::Reserved(reserved_word) => Borrowed(reserved_word.as_ref()),
            Self::Whitespace(ws) => Owned(ws.to_string()),
            Self::CmdSubStart => Borrowed("$("),
            Self::ArithmeticStart => Borrowed("$(("),
            Self::DoubleQuote => Borrowed("\""),
            Self::SingleQuote => Borrowed("'"),
            Self::Equals => Borrowed("="),
            Self::Dollar => Borrowed("$"),
            Self::QuestionMark => Borrowed("?"),
            Self::Tilde => Borrowed("~"),
            Self::Backslash => Borrowed("\\"),
            Self::And => Borrowed("&&"),
            Self::Or => Borrowed("||"),
            Self::SyncSeparator => Borrowed(";"),
            Self::AsyncSeparator => Borrowed("&"),
            Self::Pipe => Borrowed("|"),
            Self::RedirectInput => Borrowed("<"),
            Self::RedirectOutput => Borrowed(">"),
            Self::LParen => Borrowed("("),
            Self::RParen => Borrowed(")"),
            Self::Pound => Borrowed("#"),
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

pub trait Tokenizer: Iterator<Item = char> {
    fn parse_and(&mut self) -> Option<Token>;
    fn parse_or(&mut self) -> Option<Token>;
    fn parse_sync_separator(&mut self) -> Option<Token>;
    fn parse_async_separator(&mut self) -> Option<Token>;
    fn parse_pipe(&mut self) -> Option<Token>;
    fn parse_redirect_input(&mut self) -> Option<Token>;
    fn parse_redirect_output(&mut self) -> Option<Token>;
    fn parse_lparen(&mut self) -> Option<Token>;
    fn parse_rparen(&mut self) -> Option<Token>;
    fn parse_pound(&mut self) -> Option<Token>;
    fn parse_whitespace(&mut self) -> Option<Token>;
    fn parse_cmd_sub_start(&mut self) -> Option<Token>;
    fn parse_arithmetic_start(&mut self) -> Option<Token>;
    fn parse_dollar(&mut self) -> Option<Token>;
    fn parse_question_mark(&mut self) -> Option<Token>;
    fn parse_tilde(&mut self) -> Option<Token>;
    fn parse_backslash(&mut self) -> Option<Token>;
    fn parse_double_quote(&mut self) -> Option<Token>;
    fn parse_single_quote(&mut self) -> Option<Token>;
    fn parse_equals(&mut self) -> Option<Token>;
    fn parse_word(&mut self) -> Option<Token>;
    fn parse_reserved_word(&mut self) -> Option<Token>;

    fn parse(&mut self) -> Option<Token> {
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
            .or_else(|| self.parse_pound())
            .or_else(|| self.parse_reserved_word())
            .or_else(|| self.parse_arithmetic_start())
            .or_else(|| self.parse_cmd_sub_start())
            .or_else(|| self.parse_dollar())
            .or_else(|| self.parse_question_mark())
            .or_else(|| self.parse_tilde())
            .or_else(|| self.parse_backslash())
            .or_else(|| self.parse_double_quote())
            .or_else(|| self.parse_single_quote())
            .or_else(|| self.parse_equals())
            .or_else(|| self.parse_word())
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.parse() {
            tokens.push(token);
        }

        tokens
    }
}

impl<T> Tokenizer for Peekable<T>
where
    T: Iterator<Item = char> + Clone,
{
    fn parse_sync_separator(&mut self) -> Option<Token> {
        self.consume_single(';').map(|_| Token::SyncSeparator)
    }

    fn parse_async_separator(&mut self) -> Option<Token> {
        self.consume_single('&').map(|_| Token::AsyncSeparator)
    }

    fn parse_pipe(&mut self) -> Option<Token> {
        self.consume_single('|').map(|_| Token::Pipe)
    }

    fn parse_redirect_input(&mut self) -> Option<Token> {
        self.consume_single('<').map(|_| Token::RedirectInput)
    }

    fn parse_redirect_output(&mut self) -> Option<Token> {
        self.consume_single('>').map(|_| Token::RedirectOutput)
    }

    fn parse_lparen(&mut self) -> Option<Token> {
        self.consume_single('(').map(|_| Token::LParen)
    }

    fn parse_rparen(&mut self) -> Option<Token> {
        self.consume_single(')').map(|_| Token::RParen)
    }

    fn parse_pound(&mut self) -> Option<Token> {
        self.consume_single('#').map(|_| Token::Pound)
    }

    fn parse_whitespace(&mut self) -> Option<Token> {
        self.consume_if(|c| c.is_whitespace())
            .map(Token::Whitespace)
    }

    fn parse_arithmetic_start(&mut self) -> Option<Token> {
        self.consume_multiple(['$', '(', '('])
            .map(|_| Token::ArithmeticStart)
    }

    fn parse_cmd_sub_start(&mut self) -> Option<Token> {
        self.consume_multiple(['$', '('])
            .map(|_| Token::CmdSubStart)
    }

    fn parse_backslash(&mut self) -> Option<Token> {
        self.consume_single('\\').map(|_| Token::Backslash)
    }

    fn parse_dollar(&mut self) -> Option<Token> {
        self.consume_single('$').map(|_| Token::Dollar)
    }

    fn parse_question_mark(&mut self) -> Option<Token> {
        self.consume_single('?').map(|_| Token::QuestionMark)
    }

    fn parse_tilde(&mut self) -> Option<Token> {
        self.consume_single('~').map(|_| Token::Tilde)
    }

    fn parse_double_quote(&mut self) -> Option<Token> {
        self.consume_single('"').map(|_| Token::DoubleQuote)
    }

    fn parse_single_quote(&mut self) -> Option<Token> {
        self.consume_single('\'').map(|_| Token::SingleQuote)
    }

    fn parse_equals(&mut self) -> Option<Token> {
        self.consume_single('=').map(|_| Token::Equals)
    }

    fn parse_word(&mut self) -> Option<Token> {
        let mut word = String::new();

        while let Some(c) = self.peek() {
            if is_separator(*c) {
                break;
            }
            word.push(self.next().unwrap());
        }

        if word.is_empty() {
            None
        } else {
            Some(Token::Word(word))
        }
    }

    fn parse_and(&mut self) -> Option<Token> {
        self.consume_multiple(['&', '&']).map(|_| Token::And)
    }

    fn parse_or(&mut self) -> Option<Token> {
        self.consume_multiple(['|', '|']).map(|_| Token::Or)
    }

    fn parse_reserved_word(&mut self) -> Option<Token> {
        let mut consume_reserved_word = |s: &str, reserved_word| {
            let initial = self.clone();
            let consumed = self.consume_multiple(s.chars());

            if let Some(c) = self.peek() {
                if !is_separator(*c) {
                    *self = initial;
                    return None;
                }
            }

            consumed.map(|_| Token::Reserved(reserved_word))
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

fn is_separator(c: char) -> bool {
    c.is_whitespace()
        || matches!(
            c,
            '#' | '`' | '=' | '\'' | '"' | '>' | '<' | ';' | '&' | '|' | '(' | ')' | '\\' | '$'
        )
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    fn test_tokenize(input: &str, expected: Vec<Token>) {
        let tokens = lex(input);
        assert_eq!(expected, tokens);
    }

    #[test]
    fn parse_basic() {
        let mut input = vec!['|', '|'].into_iter().peekable();
        assert_eq!(Some(Or), input.parse());
        assert!(input.next().is_none());

        let mut input = vec![' '].into_iter().peekable();
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert!(input.next().is_none());

        let mut input = vec!['|', '|', '|'].into_iter().peekable();
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
            let mut input = unquoted_reserved_word.chars().peekable();
            assert_eq!(Some(Reserved(reserved_word)), input.parse());

            let single_quoted_reserved_word = format!("'{literal}'");
            let mut input = single_quoted_reserved_word.chars().peekable();
            let parsed = input.parse();
            assert!(!matches!(parsed, Some(Reserved(_))));
            assert!(matches!(parsed, Some(SingleQuote)));

            let double_quoted_reserved_word = format!(r#""{literal}""#);
            let mut input = double_quoted_reserved_word.chars().peekable();
            let parsed = input.parse();
            assert!(!matches!(parsed, Some(Reserved(_))));
            assert!(matches!(parsed, Some(DoubleQuote)));
        }
    }

    #[test]
    fn parse_comment() {
        let mut input = "#this is a comment".chars().peekable();
        let parsed = input.parse();
        assert_eq!(Some(Token::Pound), parsed);
    }

    #[test]
    fn parse_word() {
        let mut input = "foo".chars().peekable();

        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert!(input.next().is_none());

        let mut input = "'foo'".chars().peekable();

        assert_eq!(Some(SingleQuote), input.parse());
        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert_eq!(Some(SingleQuote), input.parse());
        assert!(input.next().is_none());

        let mut input = "foo'bar baz'quux\\ yo".chars().peekable();

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

        let mut input = "$(echo foo)".chars().peekable();
        assert_eq!(Some(CmdSubStart), input.parse());
        assert_eq!(Some(Word("echo".to_string())), input.parse());
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert_eq!(Some(Word("foo".to_string())), input.parse());
        assert_eq!(Some(RParen), input.parse());

        let mut input = "$(".chars().peekable();
        let expected = CmdSubStart;
        assert_eq!(Some(expected), input.parse());
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
