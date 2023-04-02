use std::borrow::Cow;
use std::iter::Peekable;
use std::str::Chars;

use crate::engine::parser::consumer::Consumer;

pub fn tokenize(input: impl AsRef<str>) -> Vec<Token> {
    input.as_ref().chars().peekable().tokenize()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Word(String),
    Whitespace(char),
    SingleQuote,
    DoubleQuote,
    Semicolon,
    Ampersand,
    Equals,
    Dollar,
    Pound,
    Pipe,
    LAngle,
    RAngle,
    LParen,
    RParen,
    Backtick,
    Backslash,
}

impl Token {
    pub fn to_str(&self) -> Cow<str> {
        match self {
            Token::Word(s) => Cow::Borrowed(s),
            Token::Whitespace(c) => Cow::Owned(format!("{c}")),
            Token::SingleQuote => Cow::Borrowed("'"),
            Token::DoubleQuote => Cow::Borrowed("\""),
            Token::Semicolon => Cow::Borrowed(";"),
            Token::Ampersand => Cow::Borrowed("&"),
            Token::Equals => Cow::Borrowed("="),
            Token::Dollar => Cow::Borrowed("$"),
            Token::Pound => Cow::Borrowed("#"),
            Token::Pipe => Cow::Borrowed("|"),
            Token::LAngle => Cow::Borrowed("<"),
            Token::RAngle => Cow::Borrowed(">"),
            Token::LParen => Cow::Borrowed("("),
            Token::RParen => Cow::Borrowed(")"),
            Token::Backtick => Cow::Borrowed("`"),
            Token::Backslash => Cow::Borrowed("\\"),
        }
    }
}

pub trait Tokenizer: Iterator<Item = char> {
    fn parse_single_quote(&mut self) -> Option<Token>;
    fn parse_double_quote(&mut self) -> Option<Token>;
    fn parse_semicolon(&mut self) -> Option<Token>;
    fn parse_ampersand(&mut self) -> Option<Token>;
    fn parse_equals(&mut self) -> Option<Token>;
    fn parse_dollar(&mut self) -> Option<Token>;
    fn parse_pound(&mut self) -> Option<Token>;
    fn parse_pipe(&mut self) -> Option<Token>;
    fn parse_langle(&mut self) -> Option<Token>;
    fn parse_rangle(&mut self) -> Option<Token>;
    fn parse_lparen(&mut self) -> Option<Token>;
    fn parse_rparen(&mut self) -> Option<Token>;
    fn parse_backtick(&mut self) -> Option<Token>;
    fn parse_backslash(&mut self) -> Option<Token>;
    fn parse_whitespace(&mut self) -> Option<Token>;
    fn parse_word(&mut self) -> Option<Token>;

    fn parse(&mut self) -> Option<Token> {
        self.parse_single_quote()
            .or_else(|| self.parse_double_quote())
            .or_else(|| self.parse_semicolon())
            .or_else(|| self.parse_ampersand())
            .or_else(|| self.parse_equals())
            .or_else(|| self.parse_dollar())
            .or_else(|| self.parse_pound())
            .or_else(|| self.parse_pipe())
            .or_else(|| self.parse_langle())
            .or_else(|| self.parse_rangle())
            .or_else(|| self.parse_lparen())
            .or_else(|| self.parse_rparen())
            .or_else(|| self.parse_backtick())
            .or_else(|| self.parse_backslash())
            .or_else(|| self.parse_whitespace())
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

impl Tokenizer for Peekable<Chars<'_>> {
    fn parse_single_quote(&mut self) -> Option<Token> {
        self.consume_single('\'').map(|_| Token::SingleQuote)
    }

    fn parse_double_quote(&mut self) -> Option<Token> {
        self.consume_single('"').map(|_| Token::DoubleQuote)
    }

    fn parse_semicolon(&mut self) -> Option<Token> {
        self.consume_single(';').map(|_| Token::Semicolon)
    }

    fn parse_ampersand(&mut self) -> Option<Token> {
        self.consume_single('&').map(|_| Token::Ampersand)
    }

    fn parse_equals(&mut self) -> Option<Token> {
        self.consume_single('=').map(|_| Token::Equals)
    }

    fn parse_dollar(&mut self) -> Option<Token> {
        self.consume_single('$').map(|_| Token::Dollar)
    }

    fn parse_pound(&mut self) -> Option<Token> {
        self.consume_single('#').map(|_| Token::Pound)
    }

    fn parse_pipe(&mut self) -> Option<Token> {
        self.consume_single('|').map(|_| Token::Pipe)
    }

    fn parse_langle(&mut self) -> Option<Token> {
        self.consume_single('<').map(|_| Token::LAngle)
    }

    fn parse_rangle(&mut self) -> Option<Token> {
        self.consume_single('>').map(|_| Token::RAngle)
    }

    fn parse_lparen(&mut self) -> Option<Token> {
        self.consume_single('(').map(|_| Token::LParen)
    }

    fn parse_rparen(&mut self) -> Option<Token> {
        self.consume_single(')').map(|_| Token::RParen)
    }

    fn parse_backtick(&mut self) -> Option<Token> {
        self.consume_single('`').map(|_| Token::Backtick)
    }

    fn parse_backslash(&mut self) -> Option<Token> {
        let backslash = self.consume_single('\\').map(|_| Token::Backslash);
        if let Some('\n') = self.peek() {
            self.next();
            None
        } else {
            backslash
        }
    }

    fn parse_whitespace(&mut self) -> Option<Token> {
        self.consume_if(|c| c.is_whitespace())
            .map(Token::Whitespace)
    }

    fn parse_word(&mut self) -> Option<Token> {
        self.consume_until(|c| is_separator(*c))
            .map(|v| Token::Word(v.iter().collect::<String>()))
    }
}

fn is_separator(c: char) -> bool {
    c.is_whitespace()
        || matches!(
            c,
            '#' | '`' | '=' | '\'' | '"' | '>' | '<' | ';' | '&' | '|' | '(' | ')' | '\\'
        )
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn parse() {
        let mut input = "'".chars().peekable();
        assert_eq!(Some(SingleQuote), input.parse());

        let mut input = "\"".chars().peekable();
        assert_eq!(Some(DoubleQuote), input.parse());

        let mut input = ";".chars().peekable();
        assert_eq!(Some(Semicolon), input.parse());

        let mut input = "&".chars().peekable();
        assert_eq!(Some(Ampersand), input.parse());

        let mut input = "=".chars().peekable();
        assert_eq!(Some(Equals), input.parse());

        let mut input = "|".chars().peekable();
        assert_eq!(Some(Pipe), input.parse());

        let mut input = "<".chars().peekable();
        assert_eq!(Some(LAngle), input.parse());

        let mut input = ">".chars().peekable();
        assert_eq!(Some(RAngle), input.parse());

        let mut input = "(".chars().peekable();
        assert_eq!(Some(LParen), input.parse());

        let mut input = ")".chars().peekable();
        assert_eq!(Some(RParen), input.parse());

        let mut input = "\\".chars().peekable();
        assert_eq!(Some(Backslash), input.parse());

        let mut input = " 	".chars().peekable();
        assert_eq!(Some(Whitespace(' ')), input.parse());
        assert_eq!(Some(Whitespace('\t')), input.parse());

        let mut input = "echo".chars().peekable();
        assert_eq!(Some(Word("echo".to_string())), input.parse());
    }

    #[test]
    fn parse_multiple() {
        let mut input = "echo 'foo'".chars().peekable();
        assert_eq!(Some(Token::Word("echo".to_string())), input.parse());
        assert_eq!(Some(Token::Whitespace(' ')), input.parse());
        assert_eq!(Some(Token::SingleQuote), input.parse());
        assert_eq!(Some(Token::Word("foo".to_string())), input.parse());
        assert_eq!(Some(Token::SingleQuote), input.parse());
        assert_eq!(None, input.parse());
    }

    #[test]
    fn tokenize() {
        let mut input = r#"foo="bar" <file echo $bar 2>> /dev/null"#.chars().peekable();

        let tokens = input.tokenize();

        let expected = vec![
            Word("foo".to_string()),
            Equals,
            DoubleQuote,
            Word("bar".to_string()),
            DoubleQuote,
            Whitespace(' '),
            LAngle,
            Word("file".to_string()),
            Whitespace(' '),
            Word("echo".to_string()),
            Whitespace(' '),
            Dollar,
            Word("bar".to_string()),
            Whitespace(' '),
            Word("2".to_string()),
            RAngle,
            RAngle,
            Whitespace(' '),
            Word("/dev/null".to_string()),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn tokenize_cmd_sub() {
        let mut input = r#"echo "$(echo "$(cat "`echo file`")" | rev)" #remove this"#
            .chars()
            .peekable();

        let tokens = input.tokenize();

        let expected = vec![
            Word("echo".to_string()),
            Whitespace(' '),
            DoubleQuote,
            Dollar,
            LParen,
            Word("echo".to_string()),
            Whitespace(' '),
            DoubleQuote,
            Dollar,
            LParen,
            Word("cat".to_string()),
            Whitespace(' '),
            DoubleQuote,
            Backtick,
            Word("echo".to_string()),
            Whitespace(' '),
            Word("file".to_string()),
            Backtick,
            DoubleQuote,
            RParen,
            DoubleQuote,
            Whitespace(' '),
            Pipe,
            Whitespace(' '),
            Word("rev".to_string()),
            RParen,
            DoubleQuote,
            Whitespace(' '),
            Pound,
            Word("remove".to_string()),
            Whitespace(' '),
            Word("this".to_string()),
        ];

        assert_eq!(expected, tokens);
    }
}
