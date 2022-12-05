use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    String(String),
    SingleQuotedString(String),
    DoubleQuotedString(String),

    // LParen,
    // RParen,

    // LBrace,
    // RBrace,
    RedirectOutput(Option<String>, String),
    RedirectInput(String),
    Pipe,

    And,
    Or,

    Colon,
    Semicolon,
    Ampersand,

    Space,
}

pub fn lex(input: impl AsRef<str>) -> Vec<Token> {
    tokenize(input, false)
}

pub fn tokenize(input: impl AsRef<str>, include_whitespace: bool) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = input.as_ref().chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            ' ' if include_whitespace => tokens.push(Token::Space),
            ' ' => {}
            // '(' => tokens.push(Token::LParen),
            // ')' => tokens.push(Token::RParen),
            // '{' => tokens.push(Token::LBrace),
            // '}' => tokens.push(Token::RBrace),
            '<' => {
                if let Some(token) = try_lex_redirect_input(&mut chars) {
                    tokens.push(token);
                }
            }

            '>' => {
                if let Some(token) = try_lex_redirect_output(&mut chars, None) {
                    tokens.push(token);
                }
            }

            '|' => {
                if let Some(&'|') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Or);
                } else {
                    tokens.push(Token::Pipe);
                }
            }

            '"' => {
                let s = advance_until(&mut chars, '"', true);
                tokens.push(Token::DoubleQuotedString(s));
            }

            '\'' => {
                let s = advance_until(&mut chars, '\'', false);
                tokens.push(Token::SingleQuotedString(s));
            }

            ':' => tokens.push(Token::Colon),
            ';' => tokens.push(Token::Semicolon),

            '&' => match chars.peek() {
                Some(&'&') => {
                    chars.next();
                    tokens.push(Token::And);
                }

                _ => {
                    tokens.push(Token::Ampersand);
                }
            },

            c if c.is_ascii_digit() => {
                let mut fd = String::from(c);
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        fd.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                let token = match chars.peek() {
                    Some(&'>') => {
                        chars.next();
                        try_lex_redirect_output(&mut chars, Some(fd))
                    }

                    Some(&'<') => {
                        chars.next();
                        try_lex_redirect_input(&mut chars)
                    }

                    _ => Some(try_lex_string(&mut chars, Some(fd), false)),
                };

                if let Some(token) = token {
                    tokens.push(token);
                }
            }

            c => {
                let token = try_lex_string(&mut chars, Some(c), false);
                tokens.push(token);
            }
        }
    }

    tokens
}

fn advance_until(chars: &mut Peekable<Chars>, end: char, check_nested_level: bool) -> String {
    let mut s = String::new();

    let mut is_escaped = false;
    let mut nested_level = 0;

    while let Some(&next) = chars.peek() {
        if !is_escaped && (nested_level == 0 || !check_nested_level) && next == end {
            chars.next();
            break;
        }

        if !is_escaped && next == '\\' {
            is_escaped = true;
            chars.next();
            continue;
        }

        let next = chars.next().unwrap();
        s.push(next);
        is_escaped = false;

        if next == '$' {
            if let Some(&'(') = chars.peek() {
                nested_level += 1;
            }
        } else if next == ')' {
            nested_level -= 1;
        }
    }
    s
}

fn try_lex_redirect_input(chars: &mut Peekable<Chars>) -> Option<Token> {
    if let Some(&' ') = chars.peek() {
        chars.next();
    }

    if let Token::String(s) = try_lex_string(chars, None::<char>, false) {
        let token = Token::RedirectInput(s);
        Some(token)
    } else {
        None
    }
}

fn try_lex_redirect_output(chars: &mut Peekable<Chars>, dest: Option<String>) -> Option<Token> {
    if let Some(&' ') = chars.peek() {
        chars.next();
    }

    if let Token::String(s) = try_lex_string(chars, None::<char>, true) {
        let token = Token::RedirectOutput(dest, s);
        Some(token)
    } else {
        None
    }
}

fn try_lex_string(
    chars: &mut Peekable<Chars>,
    start: Option<impl ToString>,
    allow_ampersand: bool,
) -> Token {
    let mut s = match start {
        Some(s) => s.to_string(),
        None => String::new(),
    };

    while let Some(&next) = chars.peek() {
        if "<> ;|".contains(next) {
            break;
        }

        if !allow_ampersand && next == '&' {
            break;
        }

        s.push(next);
        chars.next();
    }

    Token::String(s)
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn lex_basic() {
        let input = "echo 'foo bar' |lolcat".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                String("echo".into()),
                SingleQuotedString("foo bar".into()),
                Pipe,
                String("lolcat".into()),
            ],
            tokens
        );
    }

    #[test]
    fn lex_basic_2() {
        let input = "FOO= ls \"foo\" 2>/dev/null;".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                // Assignment("FOO".into(), "".into()),
                String("FOO=".into()),
                String("ls".into()),
                DoubleQuotedString("foo".into()),
                RedirectOutput(Some("2".into()), "/dev/null".into()),
                Semicolon,
            ],
            tokens
        );
    }

    #[test]
    fn lex_command_with_prefixes() {
        let input = "LC_ALL=en-US 2>&1 ls".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                // Assignment("LC_ALL".into(), "en-US".into()),
                String("LC_ALL=en-US".into()),
                RedirectOutput(Some("2".into()), "&1".into()),
                String("ls".into()),
            ],
            tokens,
        );
    }

    #[test]
    fn lex_multiple_commands_and_substitution() {
        let input = "groups \"$(whoami)\" 2>&1; sleep 3 &; :".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                String("groups".into()),
                DoubleQuotedString("$(whoami)".into()),
                RedirectOutput(Some("2".into()), "&1".into()),
                Semicolon,
                String("sleep".into()),
                String("3".into()),
                Ampersand,
                Semicolon,
                Colon,
            ],
            tokens
        );
    }

    #[test]
    fn lex_strange_redirection() {
        let input = "cat<foo.txt 2>/dev/null".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                String("cat".into()),
                RedirectInput("foo.txt".into()),
                RedirectOutput(Some("2".into()), "/dev/null".to_string()),
            ],
            tokens,
        );
    }

    #[test]
    fn lex_num() {
        let input = "echo 123 2> foo.txt".to_string();
        let tokens = lex(input);

        assert_eq!(
            vec![
                String("echo".into()),
                String("123".into()),
                RedirectOutput(Some("2".into()), "foo.txt".into()),
            ],
            tokens,
        );
    }

    #[test]
    fn lex_misc() {
        let input = "echo foo".to_string();
        let tokens = lex(input);
        assert_eq!(vec![String("echo".into()), String("foo".into())], tokens);

        let input = "echo 'foo bar'".to_string();
        let tokens = lex(input);
        assert_eq!(
            vec![String("echo".into()), SingleQuotedString("foo bar".into())],
            tokens
        );

        let input = r#"echo "foo bar""#.to_string();
        let tokens = lex(input);
        assert_eq!(
            vec![String("echo".into()), DoubleQuotedString("foo bar".into())],
            tokens
        );

        let input = r#"echo 'it\'s time to "foo bar"'"#.to_string();
        let tokens = lex(input);
        assert_eq!(
            vec![
                String("echo".into()),
                SingleQuotedString(r#"it's time to "foo bar""#.into())
            ],
            tokens
        );

        let input = "echo ''".to_string();
        let tokens = lex(input);
        assert_eq!(
            vec![String("echo".into()), SingleQuotedString("".into())],
            tokens
        );

        let input = "echo \"\"".to_string();
        let tokens = lex(input);
        assert_eq!(
            vec![String("echo".into()), DoubleQuotedString("".into())],
            tokens
        );

        let input = "PATH=\"\" ls".to_string();
        let tokens = lex(input);
        assert_eq!(
            // vec![Assignment("PATH".into(), "".into()), String("ls".into())],
            vec![String("PATH=\"\"".into()), String("ls".into())],
            tokens,
        );

        // let input = "PATH='' ls".to_string();
        // let tokens = lex(input);
        // assert_eq!(
        //     vec![Assignment("PATH".into(), "".into()), String("ls".into())],
        //     tokens,
        // );

        // let input = "PATH= ls".to_string();
        // let tokens = lex(input);
        // assert_eq!(
        //     vec![Assignment("PATH".into(), "".into()), String("ls".into())],
        //     tokens,
        // );
    }
}
