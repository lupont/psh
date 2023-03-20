use std::iter::Peekable;

use super::consumer::Consumer;
use super::tok::{Token, Tokenizer};

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
    Keyword(Keyword),
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
            SemanticToken::Keyword(keyword) => keyword.to_string(),
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
pub enum Keyword {
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

impl ToString for Keyword {
    fn to_string(&self) -> String {
        match self {
            Keyword::Bang => "!",
            Keyword::LBrace => "{",
            Keyword::RBrace => "}",
            Keyword::Case => "case",
            Keyword::Do => "do",
            Keyword::Done => "done",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::Esac => "esac",
            Keyword::Fi => "fi",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::In => "in",
            Keyword::Then => "then",
            Keyword::Until => "until",
            Keyword::While => "while",
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
    fn parse_keyword(&mut self) -> Option<SemanticToken>;

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
            .or_else(|| self.parse_comment())
            .or_else(|| self.parse_keyword())
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

        while let Some(token) = self.peek() {
            match token {
                Token::SingleQuote => {
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
                    is_escaped ^= true;
                    let slash = self.next().unwrap();
                    word.push_str(&slash.to_str());
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

        if word.is_empty() {
            None
        } else {
            Some(word)
        }
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

        while let Some(token) = self.peek() {
            match token {
                Token::Equals => {
                    word.push('=');
                    self.next();
                }

                Token::Backslash => {
                    word.push('\\');
                    self.next();
                    is_escaped ^= true;
                }

                Token::Dollar => {
                    word.push('$');
                    self.next();
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

                Token::Whitespace(c) if is_escaped => {
                    word.push(*c);
                    self.next();
                    is_escaped = false;
                }

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
        self.consume_multiple(vec![Token::Ampersand, Token::Ampersand])
            .map(|_| SemanticToken::And)
    }

    fn parse_or(&mut self) -> Option<SemanticToken> {
        self.consume_multiple(vec![Token::Pipe, Token::Pipe])
            .map(|_| SemanticToken::Or)
    }

    fn parse_keyword(&mut self) -> Option<SemanticToken> {
        let mut consume_keyword = |s: &str, keyword| {
            self.consume_single(Token::Word(s.to_string()))
                .map(|_| SemanticToken::Keyword(keyword))
        };

        consume_keyword("!", Keyword::Bang)
            .or_else(|| consume_keyword("{", Keyword::LBrace))
            .or_else(|| consume_keyword("}", Keyword::RBrace))
            .or_else(|| consume_keyword("case", Keyword::Case))
            .or_else(|| consume_keyword("do", Keyword::Do))
            .or_else(|| consume_keyword("done", Keyword::Done))
            .or_else(|| consume_keyword("elif", Keyword::Elif))
            .or_else(|| consume_keyword("else", Keyword::Else))
            .or_else(|| consume_keyword("esac", Keyword::Esac))
            .or_else(|| consume_keyword("fi", Keyword::Fi))
            .or_else(|| consume_keyword("for", Keyword::For))
            .or_else(|| consume_keyword("if", Keyword::If))
            .or_else(|| consume_keyword("in", Keyword::In))
            .or_else(|| consume_keyword("then", Keyword::Then))
            .or_else(|| consume_keyword("until", Keyword::Until))
            .or_else(|| consume_keyword("while", Keyword::While))
    }
}

#[cfg(test)]
mod tests {
    use super::super::tok::Tokenizer;
    use super::SemanticToken::*;
    use super::*;

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
    fn parse_keyword() {
        let test_data = vec![
            ("!", super::Keyword::Bang),
            ("{", super::Keyword::LBrace),
            ("}", super::Keyword::RBrace),
            ("case", super::Keyword::Case),
            ("do", super::Keyword::Do),
            ("done", super::Keyword::Done),
            ("elif", super::Keyword::Elif),
            ("else", super::Keyword::Else),
            ("esac", super::Keyword::Esac),
            ("fi", super::Keyword::Fi),
            ("for", super::Keyword::For),
            ("if", super::Keyword::If),
            ("in", super::Keyword::In),
            ("then", super::Keyword::Then),
            ("until", super::Keyword::Until),
            ("while", super::Keyword::While),
        ];

        for (literal, keyword) in test_data {
            let mut input = vec![Token::Word(literal.to_string())]
                .into_iter()
                .peekable();
            assert_eq!(Some(Keyword(keyword)), input.parse());
        }
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
    }

    #[test]
    fn tokenize_basic() {
        let mut input = r#"foo="bar" <file echo $bar 2>> /dev/null"#.chars().peekable();
        let tokens = input.tokenize();

        let semantic_tokens = tokens.into_iter().peekable().tokenize();

        let expected = vec![
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
        ];

        assert_eq!(expected, semantic_tokens);
    }

    #[test]
    fn parse_double_quoted_string() {
        let mut input = r#""foo bar \"baz \\ quux\"""#.chars().peekable();
        let mut tokens = input.tokenize().into_iter().peekable();
        let parsed = tokens.parse();

        let expected = SemanticToken::Word(r#""foo bar \"baz \\ quux\"""#.to_string());

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
}
