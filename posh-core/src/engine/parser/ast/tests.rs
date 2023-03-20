use super::super::semtok::SemanticTokenizer;
use super::super::tok::Tokenizer;
use super::*;

fn parse(input: &str) -> Peekable<impl Iterator<Item = SemanticToken> + Clone + std::fmt::Debug> {
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
    let mut tokens = parse("  foo='bar baz'");
    let actual = tokens.parse_variable_assignment();
    let expected =
        VariableAssignment::new(Word::new("foo", "  "), Some(Word::new("'bar baz'", "")));
    assert_eq!(Some(expected), actual);

    let mut tokens = parse(" foo=bar\\ baz");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(Word::new("foo", " "), Some(Word::new("bar\\ baz", "")));
    assert_eq!(Some(expected), actual);

    let mut tokens = parse(r#"foo="bar baz""#);
    let actual = tokens.parse_variable_assignment();
    let expected =
        VariableAssignment::new(Word::new("foo", ""), Some(Word::new("\"bar baz\"", "")));
    assert_eq!(Some(expected), actual);

    let mut tokens = parse("foo=");
    let actual = tokens.parse_variable_assignment();
    let expected = VariableAssignment::new(Word::new("foo", ""), None);
    assert_eq!(Some(expected), actual);
}

#[test]
fn parse_redirect_output() {
    for (item, ws) in vec!["  2>/dev/null", "  2> /dev/null"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected =
            Redirection::new_output(Word::new("2", "  "), Word::new("/dev/null", ws), false);
        assert_eq!(Some(expected), actual);
    }

    for (item, ws) in vec![" >>'foo bar baz'", " >> 'foo bar baz'"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected =
            Redirection::new_output(Word::new("", " "), Word::new("'foo bar baz'", ws), true);
        assert_eq!(Some(expected), actual);
    }

    for (item, ws) in vec!["2>>'foo bar baz'", "2>> 'foo bar baz'"]
        .iter()
        .zip(["", " "])
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected =
            Redirection::new_output(Word::new("2", ""), Word::new("'foo bar baz'", ws), true);
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
    for (item, &ws) in vec![r#"  2<"foo.txt""#, r#"  2< "foo.txt""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_input(Word::new("2", "  "), Word::new(r#""foo.txt""#, ws));
        assert_eq!(Some(expected), actual);
    }

    for (item, &ws) in vec![r#" 2<<"EOF""#, r#" 2<< "EOF""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_here_doc(Word::new("2", " "), Word::new(r#""EOF""#, ws));
        assert_eq!(Some(expected), actual);
    }

    for (item, &ws) in vec![r#"<<"EOF""#, r#"<< "EOF""#]
        .iter()
        .zip(["", " "].iter())
    {
        let mut tokens = parse(item);
        let actual = tokens.parse_redirection();
        let expected = Redirection::new_here_doc(Word::new("", ""), Word::new(r#""EOF""#, ws));
        assert_eq!(Some(expected), actual);
    }
}

#[test]
fn parse_simple_command() {
    let mut tokens = parse("echo");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Word::new("echo", ""),
        prefixes: Vec::new(),
        suffixes: Vec::new(),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("   echo foo bar baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Word::new("echo", "   "),
        prefixes: Vec::new(),
        suffixes: vec![
            SimpleCommandMeta::Word(Word::new("foo", " ")),
            SimpleCommandMeta::Word(Word::new("bar", " ")),
            SimpleCommandMeta::Word(Word::new("baz", " ")),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens =
        parse("foo='bar baz' 3>foo bar=yo echo 4</dev/null foo 2>> stderr.log bar baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Word::new("echo", " "),
        prefixes: vec![
            SimpleCommandMeta::Assignment(VariableAssignment::new(
                Word::new("foo", ""),
                Some(Word::new("'bar baz'", "")),
            )),
            SimpleCommandMeta::Redirection(Redirection::new_output(
                Word::new("3", " "),
                Word::new("foo", ""),
                false,
            )),
            SimpleCommandMeta::Assignment(VariableAssignment::new(
                Word::new("bar", " "),
                Some(Word::new("yo", "")),
            )),
        ],
        suffixes: vec![
            SimpleCommandMeta::Redirection(Redirection::new_input(
                Word::new("4", " "),
                Word::new("/dev/null", ""),
            )),
            SimpleCommandMeta::Word(Word::new("foo", " ")),
            SimpleCommandMeta::Redirection(Redirection::new_output(
                Word::new("2", " "),
                Word::new("stderr.log", " "),
                true,
            )),
            SimpleCommandMeta::Word(Word::new("bar", " ")),
            SimpleCommandMeta::Word(Word::new("baz", " ")),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("foo=bar echo bar=baz");
    let actual = tokens.parse_simple_command();

    let expected = SimpleCommand {
        name: Word::new("echo", " "),
        prefixes: vec![SimpleCommandMeta::Assignment(VariableAssignment::new(
            Word::new("foo", ""),
            Some(Word::new("bar", "")),
        ))],
        suffixes: vec![SimpleCommandMeta::Word(Word::new("bar=baz", " "))],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_pipeline() {
    let mut tokens = parse("echo foo 2>/dev/null|rev 2< file | cat");
    let actual = tokens.parse_pipeline();

    let expected = Pipeline {
        bang: None,

        first: Command::Simple(SimpleCommand {
            name: Word::new("echo", ""),
            prefixes: Vec::new(),
            suffixes: vec![
                SimpleCommandMeta::Word(Word::new("foo", " ")),
                SimpleCommandMeta::Redirection(Redirection::new_output(
                    Word::new("2", " "),
                    Word::new("/dev/null", ""),
                    false,
                )),
            ],
        }),

        rest: vec![
            (
                "".to_string(),
                Command::Simple(SimpleCommand {
                    name: Word::new("rev", ""),
                    prefixes: Vec::new(),
                    suffixes: vec![SimpleCommandMeta::Redirection(Redirection::new_input(
                        Word::new("2", " "),
                        Word::new("file", " "),
                    ))],
                }),
            ),
            (
                " ".to_string(),
                Command::Simple(SimpleCommand {
                    name: Word::new("cat", " "),
                    prefixes: Vec::new(),
                    suffixes: Vec::new(),
                }),
            ),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_and_or_list() {
    let mut tokens = parse("foo && bar | rev || baz");
    let actual = tokens.parse_and_or_list();

    let expected = AndOrList {
        first: Pipeline {
            first: Command::Simple(SimpleCommand {
                name: Word::new("foo", ""),
                prefixes: Vec::new(),
                suffixes: Vec::new(),
            }),
            bang: None,
            rest: Vec::new(),
        },
        rest: vec![
            (
                LogicalOp::And(" ".to_string()),
                Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("bar", " "),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    }),
                    rest: vec![(
                        " ".to_string(),
                        Command::Simple(SimpleCommand {
                            name: Word::new("rev", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                    )],
                },
            ),
            (
                LogicalOp::Or(" ".to_string()),
                Pipeline {
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("baz", " "),
                        prefixes: Vec::new(),
                        suffixes: Vec::new(),
                    }),
                    bang: None,
                    rest: Vec::new(),
                },
            ),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_simple_list() {
    let mut tokens = parse("true && foo || bar & baz; quux | rev");
    let actual = tokens.parse_list();

    let expected = List {
        first: AndOrList {
            first: Pipeline {
                first: Command::Simple(SimpleCommand {
                    name: Word::new("true", ""),
                    prefixes: Vec::new(),
                    suffixes: Vec::new(),
                }),
                rest: Vec::new(),
                bang: None,
            },
            rest: vec![
                (
                    LogicalOp::And(" ".to_string()),
                    Pipeline {
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("foo", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: Vec::new(),
                        bang: None,
                    },
                ),
                (
                    LogicalOp::Or(" ".to_string()),
                    Pipeline {
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("bar", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: Vec::new(),
                        bang: None,
                    },
                ),
            ],
        },
        rest: vec![
            (
                Separator::Async(" ".to_string()),
                AndOrList {
                    first: Pipeline {
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("baz", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: Vec::new(),
                        bang: None,
                    },
                    rest: Vec::new(),
                },
            ),
            (
                Separator::Sync("".to_string()),
                AndOrList {
                    first: Pipeline {
                        bang: None,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("quux", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: vec![(
                            " ".to_string(),
                            Command::Simple(SimpleCommand {
                                name: Word::new("rev", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                        )],
                    },
                    rest: Vec::new(),
                },
            ),
        ],
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn parse_complete_command() {
    let mut tokens = parse("echo foo");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list: List {
            first: AndOrList {
                first: Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("echo", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                    }),
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            rest: Vec::new(),
        },
        separator: None,
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo foo ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list: List {
            first: AndOrList {
                first: Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("echo", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                    }),
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            rest: Vec::new(),
        },
        separator: Some(Separator::Sync(" ".to_string())),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo foo&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list: List {
            first: AndOrList {
                first: Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("echo", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                    }),
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            rest: Vec::new(),
        },
        separator: Some(Separator::Async("".to_string())),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo foo& true ;");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list: List {
            first: AndOrList {
                first: Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("echo", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                    }),
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            rest: vec![(
                Separator::Async("".to_string()),
                AndOrList {
                    first: Pipeline {
                        bang: None,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("true", " "),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
            )],
        },
        separator: Some(Separator::Sync(" ".to_string())),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo foo;true&");
    let actual = tokens.parse_complete_command();

    let expected = CompleteCommand {
        list: List {
            first: AndOrList {
                first: Pipeline {
                    bang: None,
                    first: Command::Simple(SimpleCommand {
                        name: Word::new("echo", ""),
                        prefixes: Vec::new(),
                        suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                    }),
                    rest: Vec::new(),
                },
                rest: Vec::new(),
            },
            rest: vec![(
                Separator::Sync("".to_string()),
                AndOrList {
                    first: Pipeline {
                        bang: None,
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("true", ""),
                            prefixes: Vec::new(),
                            suffixes: Vec::new(),
                        }),
                        rest: Vec::new(),
                    },
                    rest: Vec::new(),
                },
            )],
        },
        separator: Some(Separator::Async("".to_string())),
    };

    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());
}

#[test]
fn ast() {
    let mut tokens = parse(" ! 2>&1 echo foo | rev&& exit ||die; sleep 3s  &");
    let ast = tokens.parse();

    let expected = SyntaxTree {
        program: vec![CompleteCommand {
            list: List {
                first: AndOrList {
                    first: Pipeline {
                        bang: Some(" ".to_string()),
                        first: Command::Simple(SimpleCommand {
                            name: Word::new("echo", " "),
                            prefixes: vec![SimpleCommandMeta::Redirection(
                                Redirection::new_output(
                                    Word::new("2", " "),
                                    Word::new("&1", ""),
                                    false,
                                ),
                            )],
                            suffixes: vec![SimpleCommandMeta::Word(Word::new("foo", " "))],
                        }),
                        rest: vec![(
                            " ".to_string(),
                            Command::Simple(SimpleCommand {
                                name: Word::new("rev", " "),
                                prefixes: Vec::new(),
                                suffixes: Vec::new(),
                            }),
                        )],
                    },
                    rest: vec![
                        (
                            LogicalOp::And("".to_string()),
                            Pipeline {
                                bang: None,
                                first: Command::Simple(SimpleCommand {
                                    name: Word::new("exit", " "),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                }),
                                rest: Vec::new(),
                            },
                        ),
                        (
                            LogicalOp::Or(" ".to_string()),
                            Pipeline {
                                bang: None,
                                first: Command::Simple(SimpleCommand {
                                    name: Word::new("die", ""),
                                    prefixes: Vec::new(),
                                    suffixes: Vec::new(),
                                }),
                                rest: Vec::new(),
                            },
                        ),
                    ],
                },
                rest: vec![(
                    Separator::Sync("".to_string()),
                    AndOrList {
                        first: Pipeline {
                            bang: None,
                            first: Command::Simple(SimpleCommand {
                                name: Word::new("sleep", " "),
                                prefixes: Vec::new(),
                                suffixes: vec![SimpleCommandMeta::Word(Word::new("3s", " "))],
                            }),
                            rest: Vec::new(),
                        },
                        rest: Vec::new(),
                    },
                )],
            },
            separator: Some(Separator::Async("  ".to_string())),
        }],
    };
    assert_eq!(expected, ast.unwrap());
    assert!(tokens.next().is_none());
}

#[test]
fn parse_word() {
    let mut tokens = parse("  echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "  ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse(" 	'echo yo'");
    let actual = tokens.parse_word(false);
    let expected = Word::new("'echo yo'", " 	");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse(r#" "echo yo""#);
    let actual = tokens.parse_word(false);
    let expected = Word::new(r#""echo yo""#, " ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse(" echo\\ yo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo\\ yo", " ");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Some(expected), actual);
    assert!(tokens.next().is_none());

    let mut tokens = parse("echo foo");
    let actual = tokens.parse_word(false);
    let expected = Word::new("echo", "");
    assert_eq!(Some(expected), actual);
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Word("foo".to_string())), tokens.next());
    assert!(tokens.next().is_none());

    let mut tokens = parse(">foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_none());
    assert_eq!(Some(SemanticToken::RedirectOutput), tokens.next());

    let mut tokens = parse("  >foo");
    let actual = tokens.parse_word(false);
    assert!(actual.is_none());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
    assert_eq!(Some(SemanticToken::Whitespace(' ')), tokens.next());
}

#[test]
fn parse_redirection_fd() {
    let mut tokens = parse(" >");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", " ")), actual);

    let mut tokens = parse(">>");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", "")), actual);

    let mut tokens = parse(" <");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("", " ")), actual);

    let mut tokens = parse("2>");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("2", "")), actual);

    let mut tokens = parse(" 2");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(Some(Word::new("2", " ")), actual);

    let mut tokens = parse("a");
    let actual = tokens.parse_redirection_fd();
    assert_eq!(None, actual);
}

#[test]
fn syntax_tree_back_to_string() {
    let input = "   foo='bar  baz'\\ quux  echo yo hello	2< file &&  !   true|cat> foo; hello";
    let mut tokens = parse(input);
    let actual = tokens.parse().unwrap();

    assert_eq!(input.to_string(), actual.to_string());
}
