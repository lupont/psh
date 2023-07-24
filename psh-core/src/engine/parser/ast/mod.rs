pub mod prelude;
pub mod reconstruct;

#[cfg(feature = "serde")]
mod serialize;

#[cfg(test)]
mod tests;

use std::iter::Peekable;

use self::prelude::*;
use crate::engine::parser::consumer::Consumer;
use crate::engine::parser::semtok::{ReservedWord, SemanticToken, SemanticTokenizer};
use crate::engine::parser::tok::Tokenizer;
use crate::error::{ParseError, ParseResult};
use crate::{Error, Result};

pub fn parse(input: impl AsRef<str>, allow_errors: bool) -> Result<SyntaxTree> {
    let input = input.as_ref();

    if input.chars().all(char::is_whitespace) {
        return Ok(Default::default());
    }

    match input
        .chars()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .parse(true)
    {
        Ok(ast) => Ok(ast),

        Err(Ok(ast)) if allow_errors => Ok(ast),

        Err(Err(e @ ParseError::InvalidSyntaxInCmdSub)) => Err(Error::SyntaxError(format!(
            "command substitution: `{}'",
            e,
        ))),

        Err(Ok(ast)) if ast.is_ok() => Err(Error::Incomplete(ast.to_string())),

        Err(Ok(ast)) => Err(Error::SyntaxError(format!(
            "`{}'",
            ast.unparsed.trim_start()
        ))),

        Err(Err(e)) => Err(Error::ParseError(e)),
    }
}

type StdResult<T, E> = std::result::Result<T, E>;

pub trait Parser: Iterator<Item = SemanticToken> + std::fmt::Debug + Sized + Clone {
    fn parse(
        &mut self,
        swallow_rest: bool,
    ) -> StdResult<SyntaxTree, StdResult<SyntaxTree, ParseError>> {
        let linebreak = self.parse_linebreak();
        let commands = self.parse_complete_commands();
        let trailing_linebreak = self.parse_linebreak();

        let mut unparsed: String = if swallow_rest {
            self.by_ref().map(|t| t.to_string()).collect()
        } else {
            let mut this = self.clone();
            this.by_ref().map(|t| t.to_string()).collect()
        };

        // When in an interactive session, in order to keep track of
        // what line of input a syntax error occurred, we insert the
        // trailing newlines of the tree into the unparsed section.
        // This is needed for command substitutions spanning multiple
        // lines.
        if !swallow_rest && !unparsed.is_empty() {
            if let Some(newlines) = &trailing_linebreak.newlines {
                unparsed.insert_str(0, &newlines.to_string());
            }
        }

        match commands {
            Ok(cmds) => {
                let ast = SyntaxTree {
                    leading: linebreak,
                    commands: Some((cmds, trailing_linebreak)),
                    unparsed,
                };

                if ast.is_ok() {
                    Ok(ast)
                } else {
                    Err(Ok(ast))
                }
            }

            Err(ParseError::None) => Ok(SyntaxTree {
                leading: linebreak,
                commands: None,
                unparsed,
            }),

            Err(ParseError::UnfinishedCompleteCommands(ws, cmds)) => Err(Ok(SyntaxTree {
                leading: linebreak,
                commands: Some((cmds, trailing_linebreak)),
                unparsed: format!("{ws}{unparsed}"),
            })),

            Err(ParseError::InvalidSyntaxInCmdSub) => Err(Err(ParseError::InvalidSyntaxInCmdSub)),

            Err(e) => Err(Err(e)),
        }
    }

    fn parse_complete_commands(&mut self) -> ParseResult<CompleteCommands>;
    fn parse_complete_command(&mut self) -> ParseResult<CompleteCommand>;
    fn parse_list(&mut self) -> ParseResult<List>;
    fn parse_and_or_list(&mut self) -> ParseResult<AndOrList>;
    fn parse_pipeline(&mut self) -> ParseResult<Pipeline>;
    fn parse_pipe_sequence(&mut self) -> ParseResult<PipeSequence>;
    fn parse_command(&mut self) -> ParseResult<Command>;
    fn parse_compound_command(&mut self) -> ParseResult<CompoundCommand>;
    fn parse_subshell(&mut self) -> ParseResult<Subshell>;
    fn parse_compound_list(&mut self) -> ParseResult<CompoundList>;
    fn parse_term(&mut self) -> ParseResult<Term>;
    fn parse_for_clause(&mut self) -> ParseResult<ForClause>;
    fn parse_case_clause(&mut self) -> ParseResult<CaseClause>;
    fn parse_case_list_ns(&mut self) -> ParseResult<CaseListNs>;
    fn parse_case_list(&mut self) -> ParseResult<CaseList>;
    fn parse_case_item_ns(&mut self) -> ParseResult<CaseItemNs>;
    fn parse_case_item(&mut self) -> ParseResult<CaseItem>;
    fn parse_pattern(&mut self) -> ParseResult<Pattern>;
    fn parse_if_clause(&mut self) -> ParseResult<IfClause>;
    fn parse_else_part(&mut self) -> ParseResult<ElsePart>;
    fn parse_while_clause(&mut self) -> ParseResult<WhileClause>;
    fn parse_until_clause(&mut self) -> ParseResult<UntilClause>;
    fn parse_function_definition(&mut self) -> ParseResult<FunctionDefinition>;
    fn parse_function_body(&mut self) -> ParseResult<FunctionBody>;
    fn parse_brace_group(&mut self) -> ParseResult<BraceGroup>;
    fn parse_do_group(&mut self) -> ParseResult<DoGroup>;
    fn parse_simple_command(&mut self) -> ParseResult<SimpleCommand>;
    fn parse_cmd_prefix(&mut self) -> ParseResult<CmdPrefix>;
    fn parse_cmd_suffix(&mut self, allow_reserved_words: bool) -> ParseResult<CmdSuffix>;
    fn parse_newline_list(&mut self) -> ParseResult<NewlineList>;
    fn parse_linebreak(&mut self) -> Linebreak;
    fn parse_separator_op(&mut self) -> ParseResult<SeparatorOp>;
    fn parse_separator(&mut self) -> ParseResult<Separator>;
    fn parse_sequential_separator(&mut self) -> ParseResult<SequentialSeparator>;

    fn parse_name(&mut self) -> ParseResult<Name>;
    fn parse_redirection_list(&mut self) -> Vec<Redirection>;
    fn parse_redirection(&mut self) -> ParseResult<Redirection>;
    fn parse_file_descriptor(&mut self) -> ParseResult<FileDescriptor>;
    fn parse_file_redirection(&mut self) -> ParseResult<Redirection>;
    fn parse_here_redirection(&mut self) -> ParseResult<Redirection>;
    fn parse_redirection_type(&mut self) -> ParseResult<RedirectionType>;
    fn parse_here_doc_type(&mut self) -> ParseResult<HereDocType>;
    fn parse_variable_assignment(&mut self) -> ParseResult<VariableAssignment>;
    fn parse_word(&mut self, allow_reserved_words: bool) -> ParseResult<Word>;
    fn parse_comment(&mut self) -> ParseResult<Comment>;
    fn parse_pipe(&mut self) -> ParseResult<Pipe>;
    fn parse_bang(&mut self) -> ParseResult<Bang>;
    fn parse_logical_op(&mut self) -> ParseResult<LogicalOp>;

    fn swallow_whitespace(&mut self) -> LeadingWhitespace;
}

impl<T> Parser for Peekable<T>
where
    T: Iterator<Item = SemanticToken> + Clone + std::fmt::Debug,
{
    fn parse_complete_commands(&mut self) -> ParseResult<CompleteCommands> {
        let head = match self.parse_complete_command() {
            Ok(cmd) => cmd,
            Err(ParseError::UnfinishedCompleteCommand(ws, head)) => {
                return Err(ParseError::UnfinishedCompleteCommands(
                    ws,
                    CompleteCommands {
                        head,
                        tail: Default::default(),
                    },
                ));
            }
            Err(e) => return Err(e),
        };

        let mut tail = Vec::new();

        let mut prev = self.clone();

        loop {
            let Ok(newlines) = self.parse_newline_list() else {
                break;
            };
            let Ok(cmd) = self.parse_complete_command() else {
                *self = prev;
                break;
            };
            tail.push((newlines, cmd));
            prev = self.clone();
        }

        Ok(CompleteCommands { head, tail })
    }

    fn parse_complete_command(&mut self) -> ParseResult<CompleteCommand> {
        let list_and_separator = self
            .parse_list()
            .map(|list| (list, self.parse_separator_op()));

        let comment = self.parse_comment();

        if let Ok((list, separator_op)) = list_and_separator {
            Ok(CompleteCommand::List {
                list,
                separator_op: separator_op.ok(),
                comment: comment.ok(),
            })
        } else if let Ok(comment) = comment {
            Ok(CompleteCommand::Comment { comment })
        } else if let Err(ParseError::UnfinishedList(ws, list)) = list_and_separator {
            Err(ParseError::UnfinishedCompleteCommand(
                ws,
                CompleteCommand::List {
                    list,
                    separator_op: None,
                    comment: None,
                },
            ))
        } else {
            Err(ParseError::None)
        }
    }

    fn parse_list(&mut self) -> ParseResult<List> {
        let head = match self.parse_and_or_list() {
            Ok(list) => list,
            Err(ParseError::UnfinishedAndOrList(ws, head)) => {
                return Err(ParseError::UnfinishedList(
                    ws,
                    List {
                        head,
                        tail: Default::default(),
                    },
                ));
            }
            Err(e) => return Err(e),
        };

        let mut tail = Vec::new();
        let mut prev = self.clone();

        loop {
            let Ok(sep_op) = self.parse_separator_op() else {
                *self = prev;
                break;
            };
            let and_or_list = match self.parse_and_or_list() {
                Ok(list) => list,
                Err(ParseError::UnfinishedAndOrList(ws, and_or_list)) => {
                    tail.push((sep_op, and_or_list));
                    return Err(ParseError::UnfinishedList(ws, List { head, tail }));
                }
                Err(ParseError::None) => {
                    *self = prev;
                    break;
                }
                Err(e) => {
                    return Err(e);
                }
            };
            tail.push((sep_op, and_or_list));
            prev = self.clone();
        }

        Ok(List { head, tail })
    }

    fn parse_and_or_list(&mut self) -> ParseResult<AndOrList> {
        let head = match self.parse_pipeline() {
            Ok(pipeline) => pipeline,
            Err(ParseError::UnfinishedPipeline(ws, head)) => {
                return Err(ParseError::UnfinishedAndOrList(
                    ws,
                    AndOrList {
                        head,
                        tail: Default::default(),
                    },
                ));
            }
            Err(e) => return Err(e),
        };

        let mut tail = Vec::new();

        loop {
            let Ok(logical_op) = self.parse_logical_op() else { break; };
            let linebreak = self.parse_linebreak();
            let pipeline = match self.parse_pipeline() {
                Ok(pipeline) => pipeline,
                Err(ParseError::UnfinishedPipeline(ws, pipeline)) => {
                    tail.push((logical_op, linebreak, pipeline));
                    return Err(ParseError::UnfinishedAndOrList(
                        ws,
                        AndOrList { head, tail },
                    ));
                }
                Err(ParseError::None) => {
                    tail.push((logical_op, linebreak, Pipeline::noop()));
                    return Err(ParseError::UnfinishedAndOrList(
                        Default::default(),
                        AndOrList { head, tail },
                    ));
                }
                Err(e) => return Err(e),
            };
            tail.push((logical_op, linebreak, pipeline));
        }

        Ok(AndOrList { head, tail })
    }

    fn parse_pipeline(&mut self) -> ParseResult<Pipeline> {
        let initial = self.clone();
        let bang = self.parse_bang().ok();

        match self.parse_pipe_sequence() {
            Ok(sequence) => Ok(Pipeline { bang, sequence }),
            Err(ParseError::UnfinishedPipeSequence(ws, sequence)) => Err(
                ParseError::UnfinishedPipeline(ws, Pipeline { bang, sequence }),
            ),
            Err(e) => {
                *self = initial;
                Err(e)
            }
        }
    }

    fn parse_pipe_sequence(&mut self) -> ParseResult<PipeSequence> {
        let head = match self.parse_command() {
            Ok(cmd) => cmd,
            Err(ParseError::UnfinishedCommand(cmd)) => {
                let seq = PipeSequence {
                    head: Box::new(cmd),
                    tail: Default::default(),
                };
                return Err(ParseError::UnfinishedPipeSequence(Default::default(), seq));
            }
            Err(e) => return Err(e),
        };

        let mut tail = Vec::new();

        loop {
            let Ok(pipe) = self.parse_pipe() else { break; };
            let linebreak = self.parse_linebreak();
            let cmd = match self.parse_command() {
                Ok(cmd) => cmd,
                Err(ParseError::UnfinishedCommand(cmd)) => {
                    tail.push((pipe, linebreak, cmd));
                    let seq = PipeSequence {
                        head: Box::new(head),
                        tail,
                    };
                    return Err(ParseError::UnfinishedPipeSequence(Default::default(), seq));
                }
                Err(ParseError::None) => {
                    tail.push((pipe, linebreak, Command::noop()));
                    let seq = PipeSequence {
                        head: Box::new(head),
                        tail,
                    };
                    return Err(ParseError::UnfinishedPipeSequence(Default::default(), seq));
                }
                Err(e) => return Err(e),
            };
            tail.push((pipe, linebreak, cmd));
        }

        Ok(PipeSequence {
            head: Box::new(head),
            tail,
        })
    }

    fn parse_command(&mut self) -> ParseResult<Command> {
        self.parse_function_definition()
            .map(Command::FunctionDefinition)
            .or_else(|_| {
                self.parse_compound_command()
                    .map(|c| (c, self.parse_redirection_list()))
                    .map(|(c, r)| Command::Compound(c, r))
            })
            .or_else(|_| {
                self.parse_simple_command()
                    .map(Command::Simple)
                    .map_err(|e| match e {
                        ParseError::UnfinishedSimpleCommand(cmd) => {
                            ParseError::UnfinishedCommand(Command::Simple(cmd))
                        }
                        e => e,
                    })
            })
    }

    fn parse_compound_command(&mut self) -> ParseResult<CompoundCommand> {
        self.parse_brace_group()
            .map(CompoundCommand::Brace)
            .or_else(|_| self.parse_subshell().map(CompoundCommand::Subshell))
            .or_else(|_| self.parse_for_clause().map(CompoundCommand::For))
            .or_else(|_| self.parse_case_clause().map(CompoundCommand::Case))
            .or_else(|_| self.parse_if_clause().map(CompoundCommand::If))
            .or_else(|_| self.parse_while_clause().map(CompoundCommand::While))
            .or_else(|_| self.parse_until_clause().map(CompoundCommand::Until))
    }

    fn parse_subshell(&mut self) -> ParseResult<Subshell> {
        let initial = self.clone();

        let lparen_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::LParen) else {
            *self = initial;
            return Err(ParseError::None);
        };

        let Ok(body) = self.parse_compound_list() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let rparen_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::RParen) else {
            *self = initial;
            return Err(ParseError::None);
        };

        Ok(Subshell {
            lparen_ws,
            body,
            rparen_ws,
        })
    }

    fn parse_compound_list(&mut self) -> ParseResult<CompoundList> {
        let initial = self.clone();
        let linebreak = self.parse_linebreak();

        let Ok(term) = self.parse_term() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let separator = self.parse_separator().ok();

        Ok(CompoundList {
            linebreak,
            term,
            separator,
        })
    }

    fn parse_term(&mut self) -> ParseResult<Term> {
        let head = self.parse_and_or_list()?;

        let mut prev = self.clone();
        let mut tail = Vec::new();
        while let Ok((sep, and_or)) = self
            .parse_separator()
            .and_then(|sep| self.parse_and_or_list().map(|a| (sep, a)))
        {
            tail.push((sep, and_or));
            prev = self.clone();
        }

        *self = prev;

        Ok(Term { head, tail })
    }

    fn parse_for_clause(&mut self) -> ParseResult<ForClause> {
        Err(ParseError::Unimplemented("for clause".to_string()))
    }

    fn parse_case_clause(&mut self) -> ParseResult<CaseClause> {
        Err(ParseError::Unimplemented("case clause".to_string()))
    }

    fn parse_case_list_ns(&mut self) -> ParseResult<CaseListNs> {
        Err(ParseError::Unimplemented("case list NS".to_string()))
    }

    fn parse_case_list(&mut self) -> ParseResult<CaseList> {
        Err(ParseError::Unimplemented("case list".to_string()))
    }

    fn parse_case_item_ns(&mut self) -> ParseResult<CaseItemNs> {
        Err(ParseError::Unimplemented("case item NS".to_string()))
    }

    fn parse_case_item(&mut self) -> ParseResult<CaseItem> {
        Err(ParseError::Unimplemented("case item".to_string()))
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        Err(ParseError::Unimplemented("pattern".to_string()))
    }

    fn parse_if_clause(&mut self) -> ParseResult<IfClause> {
        Err(ParseError::Unimplemented("if clause".to_string()))
    }

    fn parse_else_part(&mut self) -> ParseResult<ElsePart> {
        Err(ParseError::Unimplemented("else part".to_string()))
    }

    fn parse_while_clause(&mut self) -> ParseResult<WhileClause> {
        Err(ParseError::Unimplemented("while clause".to_string()))
    }

    fn parse_until_clause(&mut self) -> ParseResult<UntilClause> {
        Err(ParseError::Unimplemented("until clause".to_string()))
    }

    fn parse_function_definition(&mut self) -> ParseResult<FunctionDefinition> {
        let initial = self.clone();

        let name = match self.parse_name() {
            Ok(name) => name,
            Err(e) => {
                *self = initial;
                return Err(e);
            }
        };

        let mut parens = String::new();
        for token in [SemanticToken::LParen, SemanticToken::RParen] {
            parens.push_str(self.swallow_whitespace().as_ref());
            match self.consume_single(token) {
                Some(token) => parens.push_str(&token.to_string()),
                None => {
                    *self = initial;
                    return Err(ParseError::None);
                }
            };
        }

        let linebreak = self.parse_linebreak();

        let Ok(body) = self.parse_function_body() else {
            *self = initial;
            return Err(ParseError::Unimplemented("function definition".to_string()));
        };

        Ok(FunctionDefinition {
            name,
            parens,
            linebreak,
            body,
        })
    }

    fn parse_function_body(&mut self) -> ParseResult<FunctionBody> {
        let command = self.parse_compound_command()?;

        let redirections = self.parse_redirection_list();

        Ok(FunctionBody {
            command,
            redirections,
        })
    }

    fn parse_brace_group(&mut self) -> ParseResult<BraceGroup> {
        let initial = self.clone();

        let lbrace_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::Reserved(ReservedWord::LBrace)) else {
            *self = initial;
            return Err(ParseError::None);
        };

        let Ok(body) = self.parse_compound_list() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let rbrace_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::Reserved(ReservedWord::RBrace)) else {
            *self = initial;
            return Err(ParseError::None);
        };

        Ok(BraceGroup {
            lbrace_ws,
            body,
            rbrace_ws,
        })
    }

    fn parse_do_group(&mut self) -> ParseResult<DoGroup> {
        let initial = self.clone();

        // FIXME: whitespace
        self.consume_single(SemanticToken::Reserved(ReservedWord::Do))
            .ok_or_else(|| ParseError::Unimplemented("do group (do)".to_string()))
            .and_then(|_| self.parse_compound_list())
            .and_then(|list| {
                self.consume_single(SemanticToken::Reserved(ReservedWord::Done))
                    .map(|_| list)
                    .ok_or_else(|| ParseError::Unimplemented("do group (done)".to_string()))
            })
            .map(|body| DoGroup { body })
            .map_err(|_| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_simple_command(&mut self) -> ParseResult<SimpleCommand> {
        let initial = self.clone();

        let mut prefixes = Vec::new();
        let mut suffixes = Vec::new();

        loop {
            match self.parse_cmd_prefix() {
                Ok(prefix) => prefixes.push(prefix),
                Err(ParseError::UnfinishedCmdPrefix(prefix)) => {
                    prefixes.push(prefix);
                    let cmd = SimpleCommand {
                        prefixes,
                        name: None,
                        suffixes,
                    };
                    return Err(ParseError::UnfinishedSimpleCommand(cmd));
                }
                Err(e @ ParseError::InvalidSyntaxInCmdSub) => {
                    return Err(e);
                }
                _ => break,
            }
        }

        let name = match self.parse_word(false) {
            Ok(word) => Some(word),
            Err(ParseError::UnfinishedWord(word)) => {
                let cmd = SimpleCommand {
                    prefixes,
                    name: Some(word),
                    suffixes,
                };
                return Err(ParseError::UnfinishedSimpleCommand(cmd));
            }
            Err(e @ ParseError::InvalidSyntaxInCmdSub) => {
                return Err(e);
            }
            Err(_) => None,
        };

        loop {
            match self.parse_cmd_suffix(name.is_some()) {
                Ok(suffix) => suffixes.push(suffix),
                Err(ParseError::UnfinishedWord(word)) => {
                    suffixes.push(CmdSuffix::Word(word));
                    let cmd = SimpleCommand {
                        prefixes,
                        name,
                        suffixes,
                    };
                    return Err(ParseError::UnfinishedSimpleCommand(cmd));
                }
                Err(e @ ParseError::InvalidSyntaxInCmdSub) => {
                    return Err(e);
                }
                _ => break,
            }
        }

        if name.is_none() && prefixes.is_empty() && suffixes.is_empty() {
            *self = initial;
            Err(ParseError::None)
        } else {
            Ok(SimpleCommand {
                name,
                prefixes,
                suffixes,
            })
        }
    }

    fn parse_cmd_prefix(&mut self) -> ParseResult<CmdPrefix> {
        self.parse_redirection()
            .map(CmdPrefix::Redirection)
            .or_else(|_| {
                self.parse_variable_assignment()
                    .map(CmdPrefix::Assignment)
                    .map_err(|e| match e {
                        ParseError::UnfinishedVariableAssignment(assg) => {
                            ParseError::UnfinishedCmdPrefix(CmdPrefix::Assignment(assg))
                        }
                        e => e,
                    })
            })
    }

    fn parse_cmd_suffix(&mut self, allow_reserved_words: bool) -> ParseResult<CmdSuffix> {
        self.parse_redirection()
            .map(CmdSuffix::Redirection)
            .or_else(|_| self.parse_word(allow_reserved_words).map(CmdSuffix::Word))
    }

    fn parse_newline_list(&mut self) -> ParseResult<NewlineList> {
        let mut whitespace = String::new();
        let mut prev = self.clone();

        loop {
            let ws = self.swallow_whitespace();
            if let Some(SemanticToken::Whitespace(c @ '\n')) = self.next() {
                whitespace.push_str(ws.as_ref());
                whitespace.push(c);
                prev = self.clone();
            } else {
                *self = prev;
                break;
            }
        }

        if whitespace.is_empty() {
            Err(ParseError::None)
        } else {
            Ok(NewlineList { whitespace })
        }
    }

    fn parse_linebreak(&mut self) -> Linebreak {
        let newlines = self.parse_newline_list().ok();
        Linebreak { newlines }
    }

    fn parse_separator_op(&mut self) -> ParseResult<SeparatorOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::SyncSeparator)
            .or_else(|| self.consume_single(SemanticToken::AsyncSeparator))
            .ok_or(ParseError::None)
            .map(|t| match t {
                SemanticToken::SyncSeparator => SeparatorOp::Sync(ws),
                SemanticToken::AsyncSeparator => SeparatorOp::Async(ws),
                _ => unreachable!(),
            })
            .map_err(|_| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_separator(&mut self) -> ParseResult<Separator> {
        let initial = self.clone();

        if let Ok((separator_op, linebreak)) = self
            .parse_separator_op()
            .map(|op| (op, self.parse_linebreak()))
        {
            return Ok(Separator::Explicit(separator_op, linebreak));
        }

        *self = initial;
        self.parse_newline_list().map(Separator::Implicit)
    }

    fn parse_sequential_separator(&mut self) -> ParseResult<SequentialSeparator> {
        let initial = self.clone();

        if self.consume_single(SemanticToken::SyncSeparator).is_some() {
            let linebreak = self.parse_linebreak();
            return Ok(SequentialSeparator::Semi(linebreak));
        }

        *self = initial;
        self.parse_newline_list().map(SequentialSeparator::Implicit)
    }

    fn parse_name(&mut self) -> ParseResult<Name> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if let Some(SemanticToken::Word(word)) =
            self.consume_if(|t| matches!(t, SemanticToken::Word(_)))
        {
            if is_name(&word) {
                Ok(Name {
                    whitespace: ws,
                    name: word,
                })
            } else {
                *self = initial;
                Err(ParseError::None)
            }
        } else {
            Err(ParseError::None)
        }
    }

    fn parse_redirection_list(&mut self) -> Vec<Redirection> {
        let mut redirs = Vec::new();
        while let Ok(redir) = self.parse_redirection() {
            redirs.push(redir);
        }
        redirs
    }

    fn parse_redirection(&mut self) -> ParseResult<Redirection> {
        self.parse_file_redirection()
            .or_else(|_| self.parse_here_redirection())
    }

    fn parse_file_redirection(&mut self) -> ParseResult<Redirection> {
        let initial = self.clone();

        let ws = self.swallow_whitespace();

        let input_fd = self.parse_file_descriptor().ok();

        let Ok(ty) = self.parse_redirection_type() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let target = match self.parse_word(true) {
            Ok(word) => word,
            Err(e) => {
                *self = initial;
                return Err(e);
            }
        };

        Ok(Redirection::File {
            whitespace: ws,
            input_fd,
            ty,
            target,
        })
    }

    fn parse_here_redirection(&mut self) -> ParseResult<Redirection> {
        let initial = self.clone();

        let ws = self.swallow_whitespace();

        let input_fd = self.parse_file_descriptor().ok();

        let Ok(ty) = self.parse_here_doc_type() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let end = match self.parse_word(true) {
            Ok(word) => word,
            Err(e) => {
                *self = initial;
                return Err(e);
            }
        };

        // FIXME: actually parse content
        let content = Word::new("", "");

        Ok(Redirection::Here {
            whitespace: ws,
            input_fd,
            ty,
            end,
            content,
        })
    }

    fn parse_redirection_type(&mut self) -> ParseResult<RedirectionType> {
        use SemanticToken::*;
        let initial = self.clone();

        match self.next().zip(self.peek()) {
            Some((RedirectInput, AsyncSeparator)) => {
                self.next();
                Ok(RedirectionType::InputFd)
            }
            Some((RedirectInput, RedirectOutput)) => {
                self.next();
                Ok(RedirectionType::ReadWrite)
            }
            Some((RedirectInput, _)) => Ok(RedirectionType::Input),

            Some((RedirectOutput, AsyncSeparator)) => {
                self.next();
                Ok(RedirectionType::OutputFd)
            }
            Some((RedirectOutput, RedirectOutput)) => {
                self.next();
                Ok(RedirectionType::OutputAppend)
            }
            Some((RedirectOutput, Pipe)) => {
                self.next();
                Ok(RedirectionType::OutputClobber)
            }
            Some((RedirectOutput, _)) => Ok(RedirectionType::Output),

            _ => {
                *self = initial;
                Err(ParseError::None)
            }
        }
    }

    fn parse_here_doc_type(&mut self) -> ParseResult<HereDocType> {
        use SemanticToken::*;
        let initial = self.clone();

        match (self.next(), self.next(), self.peek()) {
            (Some(RedirectInput), Some(RedirectInput), Some(Word(w)))
                if w.to_string().as_str() == "-" =>
            {
                self.next();
                Ok(HereDocType::StripTabs)
            }

            (Some(RedirectInput), Some(RedirectInput), _) => Ok(HereDocType::Normal),

            _ => {
                *self = initial;
                Err(ParseError::None)
            }
        }
    }

    fn parse_variable_assignment(&mut self) -> ParseResult<VariableAssignment> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        let Ok(lhs) = self.parse_name() else {
            *self = initial;
            return Err(ParseError::None);
        };

        if self.consume_single(SemanticToken::Equals).is_none() {
            *self = initial;
            return Err(ParseError::None);
        }

        let rhs = match self.parse_word(true) {
            Ok(word) => Some(word),
            Err(ParseError::None) => None,
            Err(ParseError::UnfinishedWord(word)) => {
                let assg = VariableAssignment::new(lhs, Some(word), ws);
                return Err(ParseError::UnfinishedVariableAssignment(assg));
            }
            Err(e) => return Err(e),
        };

        Ok(VariableAssignment::new(lhs, rhs, ws))
    }

    fn parse_word(&mut self, allow_reserved_words: bool) -> ParseResult<Word> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        let mut full = String::new();
        let mut expansions = Vec::new();
        let mut is_escaped = false;
        let mut cmd_sub_finished = true;
        let mut in_double_quote = false;
        let mut in_single_quote = false;
        let mut index = 0;

        loop {
            match self.peek() {
                Some(SemanticToken::DoubleQuote) if !is_escaped => {
                    full += &self.next().unwrap().to_string();
                    index += 1;
                    is_escaped = false;
                    if !in_single_quote {
                        in_double_quote ^= true;
                    }
                }

                Some(SemanticToken::SingleQuote) if !is_escaped => {
                    full += &self.next().unwrap().to_string();
                    index += 1;
                    is_escaped = false;
                    if !in_double_quote {
                        in_single_quote ^= true;
                    }
                }

                Some(_) if is_escaped || in_single_quote => {
                    let token = self.next().unwrap().to_string();
                    full += &token;
                    index += token.len();
                    is_escaped = false;
                }

                Some(SemanticToken::Backslash) if in_single_quote || is_escaped => {
                    full += &self.next().unwrap().to_string();
                    index += 1;
                    is_escaped = false;
                }

                Some(SemanticToken::Backslash) => {
                    full += &self.next().unwrap().to_string();
                    index += 1;
                    is_escaped = true;
                    if let Some(SemanticToken::Whitespace('\n')) = self.peek() {
                        self.next();
                        full += "\n";
                        index += 1;
                        is_escaped = false;
                    }
                }

                Some(SemanticToken::Whitespace(c))
                    if in_double_quote || in_single_quote || is_escaped =>
                {
                    full.push(*c);
                    index += 1;
                    is_escaped = false;
                    self.next();
                }

                Some(SemanticToken::Equals) => {
                    full += &self.next().unwrap().to_string();
                    index += 1;
                    is_escaped = false;
                }

                Some(SemanticToken::CmdSubStart) => {
                    let token = self.next().unwrap();
                    let mut part = token.to_string();
                    let (mut ast, finished) = match self.parse(false) {
                        Ok(ast) => (ast, false),
                        Err(Ok(ast)) => {
                            let finished = ast.unparsed.trim_start().starts_with(')');
                            (ast, finished)
                        }
                        Err(Err(_)) => return Err(ParseError::InvalidSyntaxInCmdSub),
                    };

                    ast.unparsed.clear();
                    part += &ast.to_string();

                    if finished {
                        let ws = self.swallow_whitespace();
                        let Some(rparen @ SemanticToken::RParen) = self.next() else {
                            // the only time `finished` is true, is if the first
                            // non-whitespace unparsed part is a right paren, meaning
                            // we'll never get to here if that is not the case
                            unreachable!()
                        };
                        part += ws.as_ref();
                        part += &rparen.to_string();
                    }

                    let len = part.len();
                    full += &part;
                    expansions.push(Expansion::Command {
                        range: index..=index + len - 1,
                        part,
                        tree: ast,
                        finished,
                    });

                    cmd_sub_finished = finished;
                    index += len;
                    is_escaped = false;
                }

                Some(dollar @ SemanticToken::Dollar) => {
                    // TODO: actually handle this
                    let dollar = dollar.to_string();
                    full += &dollar;
                    index += dollar.len();
                    is_escaped = false;
                    self.next();
                }

                Some(SemanticToken::Word(xs)) => {
                    full += xs;
                    index += xs.len();
                    is_escaped = false;
                    self.next();
                }

                Some(SemanticToken::Reserved(reserved)) if allow_reserved_words => {
                    let res = reserved.as_ref();
                    full += res;
                    index += res.len();
                    is_escaped = false;
                    self.next();
                }

                _ => break,
            }
        }

        let mut word = Word::new(&full, ws);
        word.expansions.append(&mut expansions);

        if word.name_with_escaped_newlines.is_empty() {
            *self = initial;
            Err(ParseError::None)
        } else if cmd_sub_finished && !in_double_quote && !in_single_quote && !is_escaped {
            Ok(word)
        } else {
            Err(ParseError::UnfinishedWord(word))
        }
    }

    fn parse_file_descriptor(&mut self) -> ParseResult<FileDescriptor> {
        let initial = self.clone();

        if let Some(SemanticToken::Word(word)) = self.next() {
            match word.as_str() {
                "0" => return Ok(FileDescriptor::Stdin),
                "1" => return Ok(FileDescriptor::Stdout),
                "2" => return Ok(FileDescriptor::Stderr),
                n => {
                    if let Ok(n) = n.parse::<i32>() {
                        return Ok(FileDescriptor::Other(n));
                    }
                }
            }
        }

        *self = initial;
        Err(ParseError::None)
    }

    fn parse_comment(&mut self) -> ParseResult<Comment> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if let Some(SemanticToken::Comment(comment)) = self.next() {
            Ok(Comment {
                whitespace: ws,
                content: comment,
            })
        } else {
            *self = initial;
            Err(ParseError::None)
        }
    }

    fn parse_pipe(&mut self) -> ParseResult<Pipe> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(SemanticToken::Pipe)
            .map(|_| Pipe { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_bang(&mut self) -> ParseResult<Bang> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(SemanticToken::Reserved(ReservedWord::Bang))
            .map(|_| Bang { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_logical_op(&mut self) -> ParseResult<LogicalOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        self.consume_single(SemanticToken::And)
            .or_else(|| self.consume_single(SemanticToken::Or))
            .map(|t| match t {
                SemanticToken::And => LogicalOp::And(ws),
                SemanticToken::Or => LogicalOp::Or(ws),
                _ => unreachable!(),
            })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn swallow_whitespace(&mut self) -> LeadingWhitespace {
        let mut s = LeadingWhitespace::default();
        while let Some(SemanticToken::Whitespace(c @ (' ' | '\t'))) = self.peek() {
            s.0.push(*c);
            self.next();
        }
        s
    }
}

fn is_valid_part_of_name(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_name(input: impl AsRef<str>) -> bool {
    let mut input = input.as_ref().chars().peekable();
    match input.peek() {
        None | Some('0'..='9') => false,
        _ => input.all(is_valid_part_of_name),
    }
}
