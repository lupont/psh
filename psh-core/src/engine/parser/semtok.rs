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
            SemanticToken::Word(word) => word.to_string(),
            SemanticToken::Reserved(reserved_word) => reserved_word.to_string(),
            SemanticToken::Whitespace(ws) => ws.to_string(),
            SemanticToken::And => "&&".to_string(),
            SemanticToken::Or => "||".to_string(),
            SemanticToken::SyncSeparator => ";".to_string(),
            SemanticToken::AsyncSeparator => "&".to_string(),
            SemanticToken::Pipe => "|".to_string(),
            SemanticToken::RedirectInput => "<".to_string(),
            SemanticToken::RedirectOutput => ">".to_string(),
            SemanticToken::LParen => "(".to_string(),
            SemanticToken::RParen => ")".to_string(),
            SemanticToken::Comment(comment) => format!("#{comment}"),
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

impl ToString for ReservedWord {
    fn to_string(&self) -> String {
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
        .to_string()
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
    fn parse_word(&mut self) -> Option<SemanticToken>;
    fn parse_reserved_word(&mut self) -> Option<SemanticToken>;

    fn parse_single_quoted_string(&mut self) -> Option<String>;
    fn parse_double_quoted_string(&mut self) -> Option<String>;

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

    fn parse_single_quoted_string(&mut self) -> Option<String> {
        if !matches!(self.peek(), Some(Token::SingleQuote)) {
            return None;
        }

        let mut word = String::new();

        let quote = self.next().unwrap();
        word.push_str(&quote.to_str());

        while self.peek().is_some() {
            let next = self.next().unwrap();
            word.push_str(&next.to_str());
            if let Token::SingleQuote = next {
                break;
            }
        }

        if word.is_empty() {
            None
        } else {
            Some(word)
        }
    }

    fn parse_double_quoted_string(&mut self) -> Option<String> {
        if !matches!(self.peek(), Some(Token::DoubleQuote)) {
            return None;
        }

        let mut word = String::new();

        let quote = self.next().unwrap();
        word.push_str(&quote.to_str());

        let mut is_escaped = false;

        while let Some(token) = self.peek() {
            match token {
                Token::Backslash => {
                    let slash = self.next().unwrap();
                    word.push_str(&slash.to_str());
                    if let Some(Token::Whitespace('\n')) = self.peek() {
                        self.next();
                        word.push('\n');
                    } else {
                        is_escaped ^= true;
                    }
                }

                Token::DoubleQuote if is_escaped => {
                    let quote = self.next().unwrap();
                    word.push_str(&quote.to_str());
                    is_escaped = false;
                }

                Token::DoubleQuote if !is_escaped => {
                    let quote = self.next().unwrap();
                    word.push_str(&quote.to_str());
                    break;
                }

                _ => {
                    let inner = self.next().unwrap();
                    word.push_str(&inner.to_str());
                }
            }
        }

        Some(word)
    }

    fn parse_word(&mut self) -> Option<SemanticToken> {
        if !matches!(
            self.peek(),
            Some(
                Token::Backslash
                    | Token::Backtick
                    | Token::Dollar
                    | Token::Equals
                    | Token::Word(_)
                    | Token::SingleQuote
                    | Token::DoubleQuote
            )
        ) {
            return None;
        }

        let initial = self.clone();

        let mut word = String::new();

        let mut is_escaped = false;
        let mut nested_level = Vec::new();

        enum SubExprType {
            CmdSub,
            Arithmetic,
        }

        while let Some(token) = self.peek() {
            match token {
                t if is_escaped => {
                    word.push_str(&t.to_str());
                    self.next();
                    is_escaped = false;
                }

                Token::Backslash => {
                    let slash = self.next().unwrap();
                    word.push_str(&slash.to_str());
                    if let Some(Token::Whitespace('\n')) = self.peek() {
                        self.next();
                        word.push('\n');
                    } else {
                        is_escaped ^= true;
                    }
                }

                Token::Equals => {
                    word.push('=');
                    self.next();
                }

                Token::Dollar => {
                    word.push('$');
                    self.next();

                    if let Some(Token::LParen) = self.peek() {
                        self.next();
                        word.push('(');

                        if let Some(Token::LParen) = self.peek() {
                            self.next();
                            word.push('(');
                            nested_level.push(SubExprType::Arithmetic);
                        } else {
                            nested_level.push(SubExprType::CmdSub);
                        }
                    }
                }

                Token::RParen => {
                    word.push(')');
                    self.next();

                    if let Some(SubExprType::Arithmetic) = nested_level.pop() {
                        if let Some(Token::RParen) = self.peek() {
                            word.push(')');
                            self.next();
                        }
                    }
                }

                Token::SingleQuote => {
                    if let Some(s) = self.parse_single_quoted_string() {
                        word.push_str(&s);
                    } else {
                        *self = initial;
                        return None;
                    }
                }

                Token::DoubleQuote => {
                    if let Some(s) = self.parse_double_quoted_string() {
                        word.push_str(&s);
                    } else {
                        *self = initial;
                        return None;
                    }
                }

                Token::Whitespace(c) if is_escaped || !nested_level.is_empty() => {
                    word.push(*c);
                    self.next();
                    is_escaped = false;
                }

                Token::Word(s) => {
                    word.push_str(s);
                    self.next();
                }

                token if !nested_level.is_empty() => {
                    word.push_str(&token.to_str());
                    self.next();
                }

                Token::Pound if !word.is_empty() => {
                    word.push_str(&token.to_str());
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
        self.consume_multiple(vec![Token::Ampersand, Token::Ampersand])
            .map(|_| SemanticToken::And)
    }

    fn parse_or(&mut self) -> Option<SemanticToken> {
        self.consume_multiple(vec![Token::Pipe, Token::Pipe])
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
    fn parse_double_quoted_string() {
        let mut input = r#""foo bar \"baz \\ quux\"""#.chars().peekable();
        let mut tokens = input.tokenize().into_iter().peekable();
        let parsed = tokens.parse();
        let expected = SemanticToken::Word(r#""foo bar \"baz \\ quux\"""#.to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#""$(echo "foo" $(("1")))""#.chars().peekable();
        let mut tokens = input.tokenize().into_iter().peekable();
        let parsed = tokens.parse();
        let expected = SemanticToken::Word(r#""$(echo "foo" $(("1")))""#.to_string());
        assert_eq!(Some(expected), parsed);
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

        assert_eq!(Some(Word("'foo'".to_string())), input.parse());
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

        assert_eq!(
            Some(Word("foo'bar baz'quux\\ yo".to_string())),
            input.parse()
        );
        assert!(input.next().is_none());

        let input = "$(echo foo)".chars().peekable().tokenize();
        let actual = input.into_iter().peekable().parse();
        let expected = Word("$(echo foo)".to_string());
        assert_eq!(Some(expected), actual);

        let input = r#"$(echo "$(echo foo\ bar $(( 1))))' 2'"#.chars().peekable().tokenize();
        let actual = input.into_iter().peekable().parse();
        let expected = Word(r#"$(echo "$(echo foo\ bar $(( 1))))' 2'"#.to_string());
        assert_eq!(Some(expected), actual);

        let input = r#"$(echo '))  a')"#.chars().peekable().tokenize();
        let actual = input.into_iter().peekable().parse();
        let expected = Word(r#"$(echo '))  a')"#.to_string());
        assert_eq!(Some(expected), actual);

        let input = r#"$(echo \$\(\-\a)"#.chars().peekable().tokenize();
        let actual = input.into_iter().peekable().parse();
        let expected = Word(r#"$(echo \$\(\-\a)"#.to_string());
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn tokenize_basic() {
        test_tokenize(
            r#"foo="bar" <file echo $bar 2>> /dev/null"#,
            vec![
                Word(r#"foo="bar""#.to_string()),
                Whitespace(' '),
                RedirectInput,
                Word("file".to_string()),
                Whitespace(' '),
                Word("echo".to_string()),
                Whitespace(' '),
                Word("$bar".to_string()),
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
                Word("$(b $(c $(( d ))) )".to_string()),
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
