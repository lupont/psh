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
        whitespace: LeadingWhitespace::default(),
    }
}

#[test]
fn parse_variable_assignment() {
    let mut tokens = tokenize("foo=bar");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("bar", "")), "");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("  foo='bar baz'");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("'bar baz'", "")), "  ");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(" foo=bar\\ baz");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("bar\\ baz", "")), " ");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(r#"foo="bar baz""#);
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), Some(Word::new("\"bar baz\"", "")), "");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("foo=");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), None, "");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("  foo=");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(name("foo"), None, "  ");
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("'foo'=");
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_err());

    let mut tokens = tokenize("'foo=bar'");
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_err());

    let mut tokens = tokenize(r#""foo"="#);
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_err());

    let mut tokens = tokenize(r#""foo=bar""#);
    let actual = tokens.parse_variable_assignment();
    assert!(actual.is_err());
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

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    for item in &["{", "}", "!"] {
        let mut tokens = tokenize(&format!("echo {item}"));
        let actual = tokens.parse_simple_command();

        let expected = SimpleCommand {
            name: Some(Word::new("echo", "")),
            prefixes: Vec::new(),
            suffixes: vec![CmdSuffix::Word(Word::new(item, " "))],
        };

        assert_eq!(Ok(expected), actual);
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

    assert_eq!(Ok(expected), actual);
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

    assert_eq!(Ok(expected), actual);
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
            CmdPrefix::Redirection(Redirection::File {
                whitespace: " ".into(),
                input_fd: Some(FileDescriptor::Other(3)),
                ty: RedirectionType::Output,
                target: Word::new("foo", ""),
            }),
            CmdPrefix::Assignment(VariableAssignment::new(
                name("bar"),
                Some(Word::new("yo", "")),
                " ",
            )),
        ],
        suffixes: vec![
            CmdSuffix::Redirection(Redirection::File {
                whitespace: " ".into(),
                input_fd: Some(FileDescriptor::Other(4)),
                ty: RedirectionType::Input,
                target: Word::new("/dev/null", ""),
            }),
            CmdSuffix::Word(Word::new("foo", " ")),
            CmdSuffix::Redirection(Redirection::File {
                whitespace: " ".into(),
                input_fd: Some(FileDescriptor::Stderr),
                ty: RedirectionType::OutputAppend,
                target: Word::new("stderr.log", " "),
            }),
            CmdSuffix::Word(Word::new("bar", " ")),
            CmdSuffix::Word(Word::new("baz", " ")),
        ],
    };

    assert_eq!(Ok(expected), actual);
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

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_pipeline() {
    // let mut tokens = tokenize("echo foo 2>/dev/null|rev 2< file | cat");
    // let actual = tokens.parse_pipeline();

    // let expected = Pipeline {
    //     bang: None,

    //     sequence: PipeSequence {
    //         head: Box::new(Command::Simple(SimpleCommand {
    //             name: Some(Word::new("echo", "")),
    //             prefixes: Vec::new(),
    //             suffixes: vec![
    //                 CmdSuffix::Word(Word::new("foo", " ")),
    //                 CmdSuffix::Redirection(Redirection::new_output(
    //                     Word::new("2", " "),
    //                     Word::new("/dev/null", ""),
    //                     false,
    //                     false,
    //                 )),
    //             ],
    //         })),

    //         tail: vec![
    //             (
    //                 Pipe {
    //                     whitespace: "".to_string(),
    //                 },
    //                 Linebreak { newlines: None },
    //                 Command::Simple(SimpleCommand {
    //                     name: Some(Word::new("rev", "")),
    //                     prefixes: Vec::new(),
    //                     suffixes: vec![CmdSuffix::Redirection(Redirection::new_input(
    //                         Word::new("2", " "),
    //                         Word::new("file", " "),
    //                         false,
    //                     ))],
    //                 }),
    //             ),
    //             (
    //                 Pipe {
    //                     whitespace: " ".to_string(),
    //                 },
    //                 Linebreak { newlines: None },
    //                 Command::Simple(SimpleCommand {
    //                     name: Some(Word::new("cat", " ")),
    //                     prefixes: Vec::new(),
    //                     suffixes: Vec::new(),
    //                 }),
    //             ),
    //         ],
    //     },
    // };

    // assert_eq!(Ok(expected), actual);
    // assert!(tokens.next().is_none());
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
                LogicalOp::And(" ".into()),
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
                                whitespace: " ".into(),
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
                LogicalOp::Or(" ".into()),
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

    assert_eq!(Ok(expected), actual);
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
                    LogicalOp::And(" ".into()),
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
                    LogicalOp::Or(" ".into()),
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
                SeparatorOp::Async(" ".into()),
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
                SeparatorOp::Sync("".into()),
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
                                    whitespace: " ".into(),
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

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_complete_command() {
    let mut tokens = tokenize("echo foo");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand::List {
        list: List {
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
        separator_op: None,
        comment: None,
    };

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand::List {
        list: List {
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
        separator_op: Some(SeparatorOp::Sync(" ".into())),
        comment: None,
    };

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand::List {
        list: List {
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
        separator_op: Some(SeparatorOp::Async("".into())),
        comment: None,
    };

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo& true ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand::List {
        list: List {
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
                SeparatorOp::Async("".into()),
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
        separator_op: Some(SeparatorOp::Sync(" ".into())),
        comment: None,
    };

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo;true&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand::List {
        list: List {
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
                SeparatorOp::Sync("".into()),
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
        separator_op: Some(SeparatorOp::Async("".into())),
        comment: None,
    };

    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn ast() {
    // let mut tokens = tokenize(" ! 2>&1 echo foo | rev&& exit ||die; sleep 3s  &");
    // let ast = tokens.parse();

    // let expected = SyntaxTree {
    //     leading: Linebreak { newlines: None },
    //     commands: Some((
    //         CompleteCommands {
    //             head: CompleteCommand::List(
    //                 List {
    //                     head: AndOrList {
    //                         head: Pipeline {
    //                             bang: Some(Bang {
    //                                 whitespace: " ".to_string(),
    //                             }),
    //                             sequence: PipeSequence {
    //                                 head: Box::new(Command::Simple(SimpleCommand {
    //                                     name: Some(Word::new("echo", " ")),
    //                                     prefixes: vec![CmdPrefix::Redirection(
    //                                         Redirection::new_output(
    //                                             Word::new("2", " "),
    //                                             Word::new("1", ""),
    //                                             false,
    //                                             true,
    //                                         ),
    //                                     )],
    //                                     suffixes: vec![CmdSuffix::Word(Word::new("foo", " "))],
    //                                 })),
    //                                 tail: vec![(
    //                                     Pipe {
    //                                         whitespace: " ".to_string(),
    //                                     },
    //                                     Linebreak { newlines: None },
    //                                     Command::Simple(SimpleCommand {
    //                                         name: Some(Word::new("rev", " ")),
    //                                         prefixes: Vec::new(),
    //                                         suffixes: Vec::new(),
    //                                     }),
    //                                 )],
    //                             },
    //                         },
    //                         tail: vec![
    //                             (
    //                                 LogicalOp::And("".to_string()),
    //                                 Linebreak { newlines: None },
    //                                 Pipeline {
    //                                     bang: None,
    //                                     sequence: PipeSequence {
    //                                         head: Box::new(Command::Simple(SimpleCommand {
    //                                             name: Some(Word::new("exit", " ")),
    //                                             prefixes: Vec::new(),
    //                                             suffixes: Vec::new(),
    //                                         })),
    //                                         tail: Vec::new(),
    //                                     },
    //                                 },
    //                             ),
    //                             (
    //                                 LogicalOp::Or(" ".to_string()),
    //                                 Linebreak { newlines: None },
    //                                 Pipeline {
    //                                     bang: None,
    //                                     sequence: PipeSequence {
    //                                         head: Box::new(Command::Simple(SimpleCommand {
    //                                             name: Some(Word::new("die", "")),
    //                                             prefixes: Vec::new(),
    //                                             suffixes: Vec::new(),
    //                                         })),
    //                                         tail: Vec::new(),
    //                                     },
    //                                 },
    //                             ),
    //                         ],
    //                     },
    //                     tail: vec![(
    //                         SeparatorOp::Sync("".to_string()),
    //                         AndOrList {
    //                             head: Pipeline {
    //                                 bang: None,
    //                                 sequence: PipeSequence {
    //                                     head: Box::new(Command::Simple(SimpleCommand {
    //                                         name: Some(Word::new("sleep", " ")),
    //                                         prefixes: Vec::new(),
    //                                         suffixes: vec![CmdSuffix::Word(Word::new("3s", " "))],
    //                                     })),
    //                                     tail: Vec::new(),
    //                                 },
    //                             },
    //                             tail: Vec::new(),
    //                         },
    //                     )],
    //                 },
    //                 Some(SeparatorOp::Async("  ".to_string())),
    //                 None,
    //             ),
    //             tail: Vec::new(),
    //         },
    //         Linebreak { newlines: None },
    //     )),
    //     unparsed: "".to_string(),
    // };
    // assert_eq!(Ok(expected), ast);
    // assert!(tokens.next().is_none());
}

#[test]
fn parse_word() {
    let mut tokens = tokenize("  echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "  ");
    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(" 	'echo yo'");
    let actual = tokens.parse_word(false);
    let expected = Word::new("'echo yo'", " 	");
    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(r#" "echo yo""#);
    let actual = tokens.parse_word(false);
    let expected = Word::new(r#""echo yo""#, " ");
    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(" echo\\ yo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo\\ yo", " ");
    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Ok(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = tokenize("echo foo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Ok(expected), actual);
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Word("foo".to_string())), tokens.next());
    assert!(tokens.next().is_none());

    let mut tokens = tokenize(">foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_err());
    assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());

    let mut tokens = tokenize("  >foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_err());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
}

#[test]
fn parse_file_descriptor() {
    let mut tokens = tokenize("0");
    let actual = tokens.parse_file_descriptor();
    assert_eq!(Ok(FileDescriptor::Stdin), actual);

    let mut tokens = tokenize("1");
    let actual = tokens.parse_file_descriptor();
    assert_eq!(Ok(FileDescriptor::Stdout), actual);

    let mut tokens = tokenize("2");
    let actual = tokens.parse_file_descriptor();
    assert_eq!(Ok(FileDescriptor::Stderr), actual);

    let mut tokens = tokenize("3a");
    let actual = tokens.parse_file_descriptor();
    assert!(actual.is_err());
}

#[test]
fn parse_redirection_type() {
    let mut tokens = tokenize("< <& > >> >& >| <>");

    assert_eq!(Ok(RedirectionType::Input), tokens.parse_redirection_type());

    tokens.swallow_whitespace();
    assert_eq!(
        Ok(RedirectionType::InputFd),
        tokens.parse_redirection_type()
    );

    tokens.swallow_whitespace();
    assert_eq!(Ok(RedirectionType::Output), tokens.parse_redirection_type());

    tokens.swallow_whitespace();
    assert_eq!(
        Ok(RedirectionType::OutputAppend),
        tokens.parse_redirection_type()
    );

    tokens.swallow_whitespace();
    assert_eq!(
        Ok(RedirectionType::OutputFd),
        tokens.parse_redirection_type()
    );

    tokens.swallow_whitespace();
    assert_eq!(
        Ok(RedirectionType::OutputClobber),
        tokens.parse_redirection_type()
    );

    tokens.swallow_whitespace();
    assert_eq!(
        Ok(RedirectionType::ReadWrite),
        tokens.parse_redirection_type()
    );

    assert!(tokens.next().is_none());

    // This should be parsed as a here-doc delimiter before attempting
    // to parse as a redirection type
    let mut tokens = tokenize("<<");
    assert_eq!(Ok(RedirectionType::Input), tokens.parse_redirection_type());
}

#[test]
fn parse_here_doc_type() {
    let mut tokens = tokenize("<< <<-");
    assert_eq!(Ok(HereDocType::Normal), tokens.parse_here_doc_type());

    tokens.swallow_whitespace();
    assert_eq!(Ok(HereDocType::StripTabs), tokens.parse_here_doc_type());

    assert!(tokens.next().is_none());
}

#[test]
fn parse_file_redirection() {
    let mut tokens = tokenize("  <& file");
    let expected = Redirection::File {
        whitespace: "  ".into(),
        input_fd: None,
        ty: RedirectionType::InputFd,
        target: Word::new("file", " "),
    };
    let actual = tokens.parse_file_redirection();
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("  1<> 'file'");
    let expected = Redirection::File {
        whitespace: "  ".into(),
        input_fd: Some(FileDescriptor::Stdout),
        ty: RedirectionType::ReadWrite,
        target: Word::new("'file'", " "),
    };
    let actual = tokens.parse_file_redirection();
    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("0>'file'");
    let expected = Redirection::File {
        whitespace: "".into(),
        input_fd: Some(FileDescriptor::Stdin),
        ty: RedirectionType::Output,
        target: Word::new("'file'", ""),
    };
    let actual = tokens.parse_file_redirection();
    assert_eq!(Ok(expected), actual);
}

// #[test]
// fn parse_redirection_fd() {
//     let mut tokens = tokenize(" >");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(Ok(Word::new("", " ")), actual);

//     let mut tokens = tokenize(">>");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(Ok(Word::new("", "")), actual);

//     let mut tokens = tokenize(" <");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(Ok(Word::new("", " ")), actual);

//     let mut tokens = tokenize("2>");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(Ok(Word::new("2", "")), actual);

//     let mut tokens = tokenize(" 2");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(Ok(Word::new("2", " ")), actual);

//     let mut tokens = tokenize("a");
//     let actual = tokens.parse_redirection_fd();
//     assert_eq!(None, actual);
// }

#[test]
fn syntax_tree_back_to_string() {
    let input = "   foo='bar  baz'\\ quux  echo yo hello	2< file &&  !   true|cat> foo; hello";
    let mut tokens = tokenize(input);
    let actual = tokens.parse(true).unwrap();

    assert_eq!(input.to_string(), actual.to_string());
}

#[test]
fn parse_with_comment() {
    let mut tokens = tokenize("echo foo bar #this is a comment ");
    let actual = tokens.parse(true).unwrap();

    let expected = SyntaxTree {
        leading: Linebreak { newlines: None },
        commands: Some((
            CompleteCommands {
                head: CompleteCommand::List {
                    list: List {
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
                    separator_op: None,
                    comment: Some(Comment {
                        whitespace: " ".into(),
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
        whitespace: "".into(),
        expansions: vec![Expansion::Parameter {
            range: 0..=3,
            name: "foo".to_string(),
        }],
    };

    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(r#""$foo""#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "\"$foo\"".to_string(),
        whitespace: "".into(),
        expansions: vec![Expansion::Parameter {
            range: 1..=4,
            name: "foo".to_string(),
        }],
    };

    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize("'$foo'");
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "'$foo'".to_string(),
        whitespace: "".into(),
        expansions: vec![],
    };

    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(r#""$foo..$bar_-""#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: "\"$foo..$bar_-\"".to_string(),
        whitespace: "".into(),
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

    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(r#"$FOO\ $_"#);
    println!("tokens: {tokens:?}");
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: r#"$FOO\ $_"#.to_string(),
        whitespace: "".into(),
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

    assert_eq!(Ok(expected), actual);

    let mut tokens = tokenize(r#"$a"$FOO\ $_foo"$b'$c'"#);
    let actual = tokens.parse_word(false);

    let expected = Word {
        name: r#"$a"$FOO\ $_foo"$b'$c'"#.to_string(),
        whitespace: "".into(),
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

    assert_eq!(Ok(expected), actual);
}
