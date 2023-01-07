use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    String(String),
    SingleQuotedString(String, bool),
    DoubleQuotedString(String, bool),

    RedirectOutput(Option<String>, String, Option<String>, bool),
    RedirectInput(String),
    Pipe,

    And,
    Or,

    Semicolon,
    Ampersand,

    Space,
}

impl Token {
    pub fn try_get_assignment(&self) -> Option<(String, Option<String>)> {
        match self {
            Token::String(s) => match s.split_once('=') {
                Some((a, b)) if b.is_empty() => Some((a.to_string(), None)),
                Some((a, b)) => Some((a.to_string(), Some(b.to_string()))),
                None => None,
            },
            _ => None,
        }
    }
}

pub fn lex(input: impl AsRef<str>, include_whitespace: bool) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = input.as_ref().chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            ' ' if include_whitespace => tokens.push(Token::Space),
            ' ' => {}

            '<' => {
                if let Some(token) = try_lex_redirect_input(&mut chars) {
                    tokens.push(token);
                }
            }

            '>' => {
                let append = chars.peek() == Some(&'>');
                if let Some(token) = try_lex_redirect_output(&mut chars, None, append) {
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

            '"' => match advance_until(&mut chars, '"', true) {
                Ok(s) => tokens.push(Token::DoubleQuotedString(s, true)),
                Err(s) => {
                    // FIXME: this is a syntax error, but is needed for syntax highlighting
                    tokens.push(Token::DoubleQuotedString(s, false));
                }
            },

            '\'' => match advance_until(&mut chars, '\'', false) {
                Ok(s) => tokens.push(Token::SingleQuotedString(s, true)),
                Err(s) => {
                    // FIXME: this is a syntax error, but is needed for syntax highlighting
                    tokens.push(Token::SingleQuotedString(s, false));
                }
            },

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
                        let append = chars.peek() == Some(&'>');
                        try_lex_redirect_output(&mut chars, Some(fd), append)
                    }

                    Some(&'<') => {
                        chars.next();
                        try_lex_redirect_input(&mut chars)
                    }

                    _ => Some(lex_string(&mut chars, Some(fd), false)),
                };

                if let Some(token) = token {
                    tokens.push(token);
                }
            }

            c => {
                let token = lex_string(&mut chars, Some(c), false);
                tokens.push(token);
            }
        }
    }

    tokens
}

fn advance_until(
    chars: &mut Peekable<Chars>,
    end: char,
    check_nested_level: bool,
) -> Result<String, String> {
    let mut s = String::new();

    let mut is_escaped = false;
    let mut nested_level = 0;
    let mut finished = false;

    while let Some(&next) = chars.peek() {
        if !is_escaped && (nested_level == 0 || !check_nested_level) && next == end {
            chars.next();
            finished = true;
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

    if finished {
        Ok(s)
    } else {
        Err(s)
    }
}

fn try_lex_redirect_input(chars: &mut Peekable<Chars>) -> Option<Token> {
    if let Some(&' ') = chars.peek() {
        chars.next();
    }

    if let Token::String(s) = lex_string(chars, None::<char>, false) {
        let token = Token::RedirectInput(s);
        Some(token)
    } else {
        None
    }
}

fn try_lex_redirect_output(
    chars: &mut Peekable<Chars>,
    dest: Option<String>,
    append: bool,
) -> Option<Token> {
    if append && chars.peek() == Some(&'>') {
        chars.next();
    }

    let mut found_space = None;
    while let Some(&' ') = chars.peek() {
        chars.next();
        if found_space.is_none() {
            found_space = Some(" ".to_string());
        } else {
            found_space = found_space.take().map(|s| s + " ");
        }
    }

    if let Token::String(s) = lex_string(chars, None::<char>, true) {
        let token = Token::RedirectOutput(dest, s, found_space, append);
        Some(token)
    } else {
        None
    }
}

fn lex_string(
    chars: &mut Peekable<Chars>,
    start: Option<impl ToString>,
    allow_ampersand: bool,
) -> Token {
    let mut s = match start {
        Some(s) => s.to_string(),
        None => String::new(),
    };

    let mut nested_level = 0;

    while let Some(&next) = chars.peek() {
        if "<> ;|".contains(next) && nested_level == 0 {
            break;
        }

        if !allow_ampersand && next == '&' {
            break;
        }

        if &s == "$" && next == '(' {
            nested_level += 1;
        }

        s.push(next);
        chars.next();

        if next == '$' {
            if let Some(&'(') = chars.peek() {
                nested_level += 1;
            }
        }

        if nested_level > 0 && next == ')' {
            nested_level -= 1;
        }
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
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                String("echo".into()),
                SingleQuotedString("foo bar".into(), true),
                Pipe,
                String("lolcat".into()),
            ],
            tokens
        );
    }

    #[test]
    fn lex_basic_2() {
        let input = "FOO= ls \"foo\" 2>/dev/null;".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                // Assignment("FOO".into(), "".into()),
                String("FOO=".into()),
                String("ls".into()),
                DoubleQuotedString("foo".into(), true),
                RedirectOutput(Some("2".into()), "/dev/null".into(), None, false),
                Semicolon,
            ],
            tokens
        );
    }

    #[test]
    fn lex_command_with_prefixes() {
        let input = "LC_ALL=en-US 2>&1 ls".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                // Assignment("LC_ALL".into(), Some("en-US".into())),
                String("LC_ALL=en-US".into()),
                RedirectOutput(Some("2".into()), "&1".into(), None, false),
                String("ls".into()),
            ],
            tokens,
        );
    }

    #[test]
    fn lex_multiple_commands_and_substitution() {
        let input = "groups \"$(whoami)\" 2>&1; sleep 3 &; :".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                String("groups".into()),
                DoubleQuotedString("$(whoami)".into(), true),
                RedirectOutput(Some("2".into()), "&1".into(), None, false),
                Semicolon,
                String("sleep".into()),
                String("3".into()),
                Ampersand,
                Semicolon,
                String(":".into()),
            ],
            tokens
        );
    }

    #[test]
    fn lex_strange_redirection() {
        let input = "cat<foo.txt 2>/dev/null".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                String("cat".into()),
                RedirectInput("foo.txt".into()),
                RedirectOutput(Some("2".into()), "/dev/null".to_string(), None, false),
            ],
            tokens,
        );
    }

    #[test]
    fn lext_append_redirection() {
        let input = "ls > foo; ls|rev >>foo".to_string();
        let tokens = lex(&input, false);

        assert_eq!(
            vec![
                String("ls".into()),
                RedirectOutput(None, "foo".to_string(), Some(" ".to_string()), false),
                Semicolon,
                String("ls".into()),
                Pipe,
                String("rev".into()),
                RedirectOutput(None, "foo".to_string(), None, true),
            ],
            tokens,
        );

        let tokens = lex(&input, true);

        assert_eq!(
            vec![
                String("ls".into()),
                Space,
                RedirectOutput(None, "foo".to_string(), Some(" ".to_string()), false),
                Semicolon,
                Space,
                String("ls".into()),
                Pipe,
                String("rev".into()),
                Space,
                RedirectOutput(None, "foo".to_string(), None, true),
            ],
            tokens,
        );
    }

    #[test]
    fn lex_num() {
        let input = "echo 123 2> foo.txt".to_string();
        let tokens = lex(&input, false);
        let tokens_with_space = lex(&input, true);

        assert_eq!(
            vec![
                String("echo".into()),
                String("123".into()),
                RedirectOutput(Some("2".into()), "foo.txt".into(), Some(" ".into()), false),
            ],
            tokens,
        );
        assert_eq!(
            vec![
                String("echo".into()),
                Space,
                String("123".into()),
                Space,
                RedirectOutput(Some("2".into()), "foo.txt".into(), Some(" ".into()), false),
            ],
            tokens_with_space,
        );
    }

    #[test]
    fn lex_or() {
        let input = "test -d foo | tee log || exit".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                String("test".to_string()),
                String("-d".to_string()),
                String("foo".to_string()),
                Pipe,
                String("tee".to_string()),
                String("log".to_string()),
                Or,
                String("exit".to_string()),
            ],
            tokens
        );
    }

    #[test]
    fn lex_and() {
        let input = "test -d foo | tee log && exit".to_string();
        let tokens = lex(input, false);

        assert_eq!(
            vec![
                String("test".to_string()),
                String("-d".to_string()),
                String("foo".to_string()),
                Pipe,
                String("tee".to_string()),
                String("log".to_string()),
                And,
                String("exit".to_string()),
            ],
            tokens
        );
    }

    #[test]
    fn lex_misc() {
        let input = "echo foo".to_string();
        let tokens = lex(input, false);
        assert_eq!(vec![String("echo".into()), String("foo".into())], tokens);

        let input = "echo 'foo bar'".to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![
                String("echo".into()),
                SingleQuotedString("foo bar".into(), true)
            ],
            tokens
        );

        let input = r#"echo "foo bar""#.to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![
                String("echo".into()),
                DoubleQuotedString("foo bar".into(), true)
            ],
            tokens
        );

        let input = r#"echo 'it\'s time to "foo bar"'"#.to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![
                String("echo".into()),
                SingleQuotedString(r#"it's time to "foo bar""#.into(), true)
            ],
            tokens
        );

        let input = "echo ''".to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![String("echo".into()), SingleQuotedString("".into(), true)],
            tokens
        );

        let input = "echo \"\"".to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![String("echo".into()), DoubleQuotedString("".into(), true)],
            tokens
        );

        let input = "PATH=\"\" ls".to_string();
        let tokens = lex(input, false);
        assert_eq!(
            vec![String("PATH=\"\"".into()), String("ls".into())],
            tokens,
        );
    }

    #[test]
    fn try_get_assignment_works() {
        let token = Token::String("FOO=bar".to_string());
        let assignment = token.try_get_assignment();

        assert_eq!(
            Some(("FOO".to_string(), Some("bar".to_string()))),
            assignment
        );

        let token = Token::String("ARGS=bar=baz,a=b".to_string());
        let assignment = token.try_get_assignment();

        assert_eq!(
            Some(("ARGS".to_string(), Some("bar=baz,a=b".to_string()))),
            assignment
        );

        let token = Token::String("FOO=".to_string());
        let assignment = token.try_get_assignment();

        assert_eq!(Some(("FOO".to_string(), None)), assignment);

        let token = Token::String("FOO".to_string());
        let assignment = token.try_get_assignment();

        assert_eq!(None, assignment);

        let token = Token::SingleQuotedString("a".to_string(), true);
        let assignment = token.try_get_assignment();

        assert_eq!(None, assignment);
    }
}
