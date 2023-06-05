use super::super::semtok::SemanticTokenizer;
use super::super::tok::Tokenizer;
use super::*;

fn tokenize(
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

fn name(name: &str) -> Name {
    Name {
        name: name.to_string(),
        whitespace: "".to_string(),
    }
}

#[test]
fn parse_variable_assignment() {
    let mut tokens = tokenize("foo=bar");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("bar", "")), "");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize("  foo='bar baz'");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("'bar baz'", "")), "  ");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(" foo=bar\\ baz");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("bar\\ baz", "")), " ");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(r#"foo="bar baz""#);
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("\"bar baz\"", "")), "");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize("foo=");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), None, "");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize("  foo=");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), None, "  ");
    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize("'foo'=");
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_none());

    let mut tokens = tokenize("'foo=bar'");
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_none());

    let mut tokens = tokenize(r#""foo"="#);
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_none());

    let mut tokens = tokenize(r#""foo=bar""#);
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_none());
}

#[test]
fn parse_redirect_output() {
    for (item, ws) in vec!["  2>/dev/null", "  2> /dev/null"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_output(
            Word::new("2", "  "),
            Word::new("/dev/null", ws),
            false,
            false,
        );
        assert_eq!(Some(expected), actual);
    }

    for (item, ws) in vec![" >>'foo bar baz'", " >> 'foo bar baz'"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_output(
            Word::new("", " "),
            Word::new("'foo bar baz'", ws),
            true,
            false,
        );
        assert_eq!(Some(expected), actual);
    }

    for (item, ws) in vec!["2>>'foo bar baz'", "2>> 'foo bar baz'"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_output(
            Word::new("2", ""),
            Word::new("'foo bar baz'", ws),
            true,
            false,
        );
        assert_eq!(Some(expected), actual);
    }

    let mut tokens = tokenize(">><");
    let actual = tokens.parse_redirection();
    assert!(actual.is_none());
    assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());
    assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());
    assert_eq!(Some(SemanticToken::RedirectInput), tokens.next());
    assert!(tokens.next().is_none());
}

#[test]
fn parse_redirect_input_and_here_document() {
    for (item, &ws) in vec![r#"  2<"foo.txt""#, r#"  2< "foo.txt""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected =
            Redirection::new_input(Word::new("2", "  "), Word::new(r#""foo.txt""#, ws), false);
        assert_eq!(Some(expected), actual);
    }

    for (item, &ws) in vec![r#" 2<<"EOF""#, r#" 2<< "EOF""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_here_doc(Word::new("2", " "), Word::new(r#""EOF""#, ws));
        assert_eq!(Some(expected), actual);
    }

    for (item, &ws) in vec![r#"<<"EOF""#, r#"<< "EOF""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = tokenize(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_here_doc(Word::new("", ""), Word::new(r#""EOF""#, ws));
        assert_eq!(Some(expected), actual);
    }
}

#[test]
fn parse_simple_command() {
    let mut tokens = tokenize("echo");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Some(Word::new("echo", "")),
        prefixes: Vec::new(),
        suffixes: Vec::new(),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    for item in &["{", "}", "!"] {
        let mut tokens = tokenize(&format!("echo {item}"));
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Some(Word::new("echo", "")),
            prefixes: Vec::new(),
            suffixes: vec![CmdSuffix::Word(Word::new(item, " "))],
        };

        assert_eq!(Some(expected), actual);
        assert!(tokens.next().is_none());
    }

    let mut tokens = tokenize("foo=bar");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: None,
        prefixes: vec![CmdPrefix::Assignment(VariableAssignment::new(
            name("foo"),
            Some(Word::new("bar", "")),
            "",
        ))],
        suffixes: Vec::new(),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("   echo foo bar baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Some(Word::new("echo", "   ")),
        prefixes: Vec::new(),
        suffixes: vec![
            CmdSuffix::Word(Word::new("foo", " ")),
            CmdSuffix::Word(Word::new("bar", " ")),
            CmdSuffix::Word(Word::new("baz", " ")),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens =
        tokenize("foo='bar baz' 3>foo bar=yo echo 4</dev/null foo 2>> stderr.log bar baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Some(Word::new("echo", " ")),
        prefixes: vec![
            CmdPrefix::Assignment(VariableAssignment::new(
                name("foo"),
                Some(Word::new("'bar baz'", "")),
                "",
            )),
            CmdPrefix::Redirection(Redirection::new_output(
                Word::new("3", " "),
                Word::new("foo", ""),
                false,
                false,
            )),
            CmdPrefix::Assignment(VariableAssignment::new(
                name("bar"),
                Some(Word::new("yo", "")),
                " ",
            )),
        ],
        suffixes: vec![
            CmdSuffix::Redirection(Redirection::new_input(
                Word::new("4", " "),
                Word::new("/dev/null", ""),
                false,
            )),
            CmdSuffix::Word(Word::new("foo", " ")),
            CmdSuffix::Redirection(Redirection::new_output(
                Word::new("2", " "),
                Word::new("stderr.log", " "),
                true,
                false,
            )),
            CmdSuffix::Word(Word::new("bar", " ")),
            CmdSuffix::Word(Word::new("baz", " ")),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("foo=bar echo bar=baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Some(Word::new("echo", " ")),
        prefixes: vec![CmdPrefix::Assignment(VariableAssignment::new(
            name("foo"),
            Some(Word::new("bar", "")),
            "",
        ))],
        suffixes: vec![CmdSuffix::Word(Word::new("bar=baz", " "))],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_pipeline() {
    let mut tokens = tokenize("echo foo 2>/dev/null|rev 2< file | cat");
    let actual = tokens.parse_pipeline();

    let expected = Pipeline {
        bang: None,

        sequence: PipeSequence {
            head: Box::new(Command::Simple(SimpleCommand {
                name: Some(Word::new("echo", "")),
                prefixes: Vec::new(),
                suffixes: vec![
                    CmdSuffix::Word(Word::new("foo", " ")),
                    CmdSuffix::Redirection(Redirection::new_output(
                        Word::new("2", " "),
                        Word::new("/dev/null", ""),
                        false,
                        false,
                    )),
                ],
            })),

            tail: vec![
                (
                    Pipe {
                        whitespace: "".to_string(),
                    },
                    Linebreak { newlines: None },
                    Command::Simple(SimpleCommand {
                        name: Some(Word::new("rev", "")),
                        prefixes: Vec::new(),
                        suffixes: vec![CmdSuffix::Redirection(Redirection::new_input(
                            Word::new("2", " "),
                            Word::new("file", " "),
                            false,
                        ))],
                    }),
                ),
                (
                    Pipe {
                        whitespace: " ".to_string(),
                    },
                    Linebreak { newlines: None },
                    Command::Simple(SimpleCommand {
                        name: Some(Word::new("cat", " ")),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    }),
                ),
            ],
        },
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_and_or_list() {
    let mut tokens = tokenize("foo && bar | rev || baz");
    let actual = tokens.parse_and_or_list();

    let expected = AndOrList {
        head: Pipeline {
            bang: None,
            sequence: PipeSequence {
                head: Box::new(Command::Simple(SimpleCommand {
                    name: Some(Word::new("foo", "")),
                    prefixes: Vec::new(),
                    suffixes: Vec::new(),
                })),
                tail: Vec::new(),
            },
        },
        tail: vec![
            (
                LogicalOp::And(" ".to_string()),
                Linebreak { newlines: None },
                Pipeline {
                    bang: None,
                    sequence: PipeSequence {
                        head: Box::new(Command::Simple(SimpleCommand {
                            name: Some(Word::new("bar", " ")),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        })),
                        tail: vec![(
                            Pipe {
                                whitespace: " ".to_string(),
                            },
                            Linebreak { newlines: None },
                            Command::Simple(SimpleCommand {
                                name: Some(Word::new("rev", " ")),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                        )],
                    },
                },
            ),
            (
                LogicalOp::Or(" ".to_string()),
                Linebreak { newlines: None },
                Pipeline {
                    bang: None,
                    sequence: PipeSequence {
                        head: Box::new(Command::Simple(SimpleCommand {
                            name: Some(Word::new("baz", " ")),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        })),
                        tail: Vec::new(),
                    },
                },
            ),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_list() {
    let mut tokens = tokenize("true && foo || bar & baz; quux | rev");
    let actual = tokens.parse_list();

    let expected = List {
        head: AndOrList {
            head: Pipeline {
                bang: None,
                sequence: PipeSequence {
                    head: Box::new(Command::Simple(SimpleCommand {
                        name: Some(Word::new("true", "")),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    })),
                    tail: Vec::new(),
                },
            },
            tail: vec![
                (
                    LogicalOp::And(" ".to_string()),
                    Linebreak { newlines: None },
                    Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("foo", " ")),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            })),
                            tail: Vec::new(),
                        },
                    },
                ),
                (
                    LogicalOp::Or(" ".to_string()),
                    Linebreak { newlines: None },
                    Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("bar", " ")),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            })),
                            tail: Vec::new(),
                        },
                    },
                ),
            ],
        },
        tail: vec![
            (
                SeparatorOp::Async(" ".to_string()),
                AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("baz", " ")),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
            ),
            (
                SeparatorOp::Sync("".to_string()),
                AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("quux", " ")),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            })),
                            tail: vec![(
                                Pipe {
                                    whitespace: " ".to_string(),
                                },
                                Linebreak { newlines: None },
                                Command::Simple(SimpleCommand {
                                    name: Some(Word::new("rev", " ")),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                }),
                            )],
                        },
                    },
                    tail: Vec::new(),
                },
            ),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_complete_command() {
    let mut tokens = tokenize("echo foo");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list_and_separator: Some((
            List {
                head: AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("echo", "")),
                                prefixes: Vec::new(),
                                suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
                tail: Vec::new(),
            },
            None,
        )),
        comment: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list_and_separator: Some((
            List {
                head: AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("echo", "")),
                                prefixes: Vec::new(),
                                suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
                tail: Vec::new(),
            },
            Some(SeparatorOp::Sync(" ".to_string())),
        )),
        comment: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list_and_separator: Some((
            List {
                head: AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("echo", "")),
                                prefixes: Vec::new(),
                                suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
                tail: Vec::new(),
            },
            Some(SeparatorOp::Async("".to_string())),
        )),
        comment: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo& true ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list_and_separator: Some((
            List {
                head: AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("echo", "")),
                                prefixes: Vec::new(),
                                suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
                tail: vec![(
                    SeparatorOp::Async("".to_string()),
                    AndOrList {
                        head: Pipeline {
                            bang: None,
                            sequence: PipeSequence {
                                head: Box::new(Command::Simple(SimpleCommand {
                                    name: Some(Word::new("true", " ")),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                })),
                                tail: Vec::new(),
                            },
                        },
                        tail: Vec::new(),
                    },
                )],
            },
            Some(SeparatorOp::Sync(" ".to_string())),
        )),
        comment: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo;true&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list_and_separator: Some((
            List {
                head: AndOrList {
                    head: Pipeline {
                        bang: None,
                        sequence: PipeSequence {
                            head: Box::new(Command::Simple(SimpleCommand {
                                name: Some(Word::new("echo", "")),
                                prefixes: Vec::new(),
                                suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                            })),
                            tail: Vec::new(),
                        },
                    },
                    tail: Vec::new(),
                },
                tail: vec![(
                    SeparatorOp::Sync("".to_string()),
                    AndOrList {
                        head: Pipeline {
                            bang: None,
                            sequence: PipeSequence {
                                head: Box::new(Command::Simple(SimpleCommand {
                                    name: Some(Word::new("true", "")),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                })),
                                tail: Vec::new(),
                            },
                        },
                        tail: Vec::new(),
                    },
                )],
            },
            Some(SeparatorOp::Async("".to_string())),
        )),
        comment: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn ast() {
    let mut tokens = tokenize(" ! 2>&1 echo foo | rev&& exit ||die; sleep 3s  &");
    let ast = tokens.parse();

    let expected = SyntaxTree {
        leading: Linebreak { newlines: None },
        commands: Some((
            CompleteCommands {
                head: CompleteCommand {
                    list_and_separator: Some((
                        List {
                            head: AndOrList {
                                head: Pipeline {
                                    bang: Some(Bang {
                                        whitespace: " ".to_string(),
                                    }),
                                    sequence: PipeSequence {
                                        head: Box::new(Command::Simple(SimpleCommand {
                                            name: Some(Word::new("echo", " ")),
                                            prefixes: vec![CmdPrefix::Redirection(
                                                Redirection::new_output(
                                                    Word::new("2", " "),
                                                    Word::new("1", ""),
                                                    false,
                                                    true,
                                                ),
                                            )],
                                            suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
                                        })),
                                        tail: vec![(
                                            Pipe {
                                                whitespace: " ".to_string(),
                                            },
                                            Linebreak { newlines: None },
                                            Command::Simple(SimpleCommand {
                                                name: Some(Word::new("rev", " ")),
                                                prefixes: Vec::new(),
                                                suffixes: Vec::new(),
                                            }),
                                        )],
                                    },
                                },
                                tail: vec![
                                    (
                                        LogicalOp::And("".to_string()),
                                        Linebreak { newlines: None },
                                        Pipeline {
                                            bang: None,
                                            sequence: PipeSequence {
                                                head: Box::new(Command::Simple(SimpleCommand {
                                                    name: Some(Word::new("exit", " ")),
                                                    prefixes: Vec::new(),
                                                    suffixes: Vec::new(),
                                                })),
                                                tail: Vec::new(),
                                            },
                                        },
                                    ),
                                    (
                                        LogicalOp::Or(" ".to_string()),
                                        Linebreak { newlines: None },
                                        Pipeline {
                                            bang: None,
                                            sequence: PipeSequence {
                                                head: Box::new(Command::Simple(SimpleCommand {
                                                    name: Some(Word::new("die", "")),
                                                    prefixes: Vec::new(),
                                                    suffixes: Vec::new(),
                                                })),
                                                tail: Vec::new(),
                                            },
                                        },
                                    ),
                                ],
                            },
                            tail: vec![(
                                SeparatorOp::Sync("".to_string()),
                                AndOrList {
                                    head: Pipeline {
                                        bang: None,
                                        sequence: PipeSequence {
                                            head: Box::new(Command::Simple(SimpleCommand {
                                                name: Some(Word::new("sleep", " ")),
                                                prefixes: Vec::new(),
                                                suffixes: vec![CmdSuffix::Word(Word::new(
                                                    "3s", " ",
                                                ))],
                                            })),
                                            tail: Vec::new(),
                                        },
                                    },
                                    tail: Vec::new(),
                                },
                            )],
                        },
                        Some(SeparatorOp::Async("  ".to_string())),
                    )),
                    comment: None,
                },
                tail: Vec::new(),
            },
            Linebreak { newlines: None },
        )),
        unparsed: "".to_string(),
    };
    assert_eq!(Ok(expected), ast);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_word() {
    let mut tokens = tokenize("  echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "  ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(" 	'echo yo'");
    let actual = tokens.parse_word(false);
    let expected = Word::new("'echo yo'", " 	");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(r#" "echo yo""#);
    let actual = tokens.parse_word(false);
    let expected = Word::new(r#""echo yo""#, " ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(" echo\\ yo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo\\ yo", " ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Some(expected), actual);
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Word("foo".to_string())), tokens.next());
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(">foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_none());
    assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());

    let mut tokens = tokenize("  >foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_none());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
}

#[test]
fn parse_redirection_fd() {
    let mut tokens = tokenize(" >");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", " ")), actual);

    let mut tokens = tokenize(">>");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", "")), actual);

    let mut tokens = tokenize(" <");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", " ")), actual);

    let mut tokens = tokenize("2>");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("2", "")), actual);

    let mut tokens = tokenize(" 2");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("2", " ")), actual);

    let mut tokens = tokenize("a");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(None, actual);
}

#[test]
fn syntax_tree_back_to_string() {
    let input = "   foo='bar  baz'\\ quux  echo yo hello	2< file &&  !   true|cat> foo; hello";
    let mut tokens = tokenize(input);
    let actual = tokens.parse().unwrap();

    assert_eq!(input.to_string(), actual.to_string());
}

#[test]
fn parse_with_comment() {
    let mut tokens = tokenize("echo foo bar #this is a comment ");
    let actual = tokens.parse().unwrap();

    let expected = SyntaxTree {
        leading: Linebreak { newlines: None },
        commands: Some((
            CompleteCommands {
                head: CompleteCommand {
                    list_and_separator: Some((
                        List {
                            head: AndOrList {
                                head: Pipeline {
                                    bang: None,
                                    sequence: PipeSequence {
                                        head: Box::new(Command::Simple(SimpleCommand {
                                            name: Some(Word::new("echo", "")),
                                            prefixes: Vec::new(),
                                            suffixes: vec![
                                                CmdSuffix::Word(Word::new("foo", " ")),
                                                CmdSuffix::Word(Word::new("bar", " ")),
                                            ],
                                        })),
                                        tail: Vec::new(),
                                    },
                                },
                                tail: Vec::new(),
                            },
                            tail: Vec::new(),
                        },
                        None,
                    )),
                    comment: Some(Comment {
                        whitespace: " ".to_string(),
                        content: "this is a comment ".to_string(),
                    }),
                },
                tail: Vec::new(),
            },
            Linebreak { newlines: None },
        )),
        unparsed: "".to_string(),
    };

    assert_eq!(actual, expected);
}

#[test]
fn word_with_parameter_expansions() {
    let mut tokens = tokenize("$foo");
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "$foo".to_string(),
        whitespace: "".to_string(),
        expansions: vec![Expansion::Parameter {
            range: 0..=3,
            name: "foo".to_string(),
        }],
    };

    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(r#""$foo""#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "\"$foo\"".to_string(),
        whitespace: "".to_string(),
        expansions: vec![Expansion::Parameter {
            range: 1..=4,
            name: "foo".to_string(),
        }],
    };

    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize("'$foo'");
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "'$foo'".to_string(),
        whitespace: "".to_string(),
        expansions: vec![],
    };

    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(r#""$foo..$bar_-""#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "\"$foo..$bar_-\"".to_string(),
        whitespace: "".to_string(),
        expansions: vec![
            Expansion::Parameter {
                range: 1..=4,
                name: "foo".to_string(),
            },
            Expansion::Parameter {
                range: 7..=11,
                name: "bar_".to_string(),
            },
        ],
    };

    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(r#"$FOO\ $_"#);
    println!("tokens: {tokens:?}");
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: r#"$FOO\ $_"#.to_string(),
        whitespace: "".to_string(),
        expansions: vec![
            Expansion::Parameter {
                range: 0..=3,
                name: "FOO".to_string(),
            },
            Expansion::Parameter {
                range: 6..=7,
                name: "_".to_string(),
            },
        ],
    };

    assert_eq!(Some(expected), actual);

    let mut tokens = tokenize(r#"$a"$FOO\ $_foo"$b'$c'"#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: r#"$a"$FOO\ $_foo"$b'$c'"#.to_string(),
        whitespace: "".to_string(),
        expansions: vec![
            Expansion::Parameter {
                range: 0..=1,
                name: "a".to_string(),
            },
            Expansion::Parameter {
                range: 3..=6,
                name: "FOO".to_string(),
            },
            Expansion::Parameter {
                range: 9..=13,
                name: "_foo".to_string(),
            },
            Expansion::Parameter {
                range: 15..=16,
                name: "b".to_string(),
            },
        ],
    };

    assert_eq!(Some(expected), actual);
}
