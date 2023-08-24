pub mod nodes;
pub mod reconstruct;

#[cfg(feature = "serde")]
mod serialize;

#[cfg(test)]
mod tests;

use std::iter::Peekable;

use crate::ast::nodes::*;
use crate::consumer::Consumer;
use crate::error::{ParseError, ParseResult};
use crate::tok::{ReservedWord, Token, Tokenizer};
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
        .parse(true)
    {
        Ok(ast) => Ok(ast),

        Err(Ok(ast)) if allow_errors => Ok(ast),

        Err(Ok(ast)) if ast.is_ok() => Err(Error::Incomplete(ast.to_string())),

        Err(Ok(ast)) => Err(Error::SyntaxError(format!(
            "`{}'",
            ast.unparsed.trim_start()
        ))),

        Err(Err(e @ ParseError::InvalidSyntaxInCmdSub)) => Err(Error::SyntaxError(format!(
            "command substitution: `{}'",
            e,
        ))),

        Err(Err(e)) => Err(Error::ParseError(e.to_string())),
    }
}

type StdResult<T, E> = std::result::Result<T, E>;

pub trait Parser: Iterator<Item = Token> + Clone {
    fn parse(
        &mut self,
        swallow_rest: bool,
    ) -> StdResult<SyntaxTree, StdResult<SyntaxTree, ParseError<SyntaxTree>>> {
        let linebreak = self.parse_linebreak();
        let commands = self.parse_complete_commands();
        let trailing_linebreak = self.parse_linebreak();

        let mut unparsed: String = if swallow_rest {
            self.by_ref().map(|t| t.as_str().to_string()).collect()
        } else {
            let mut this = self.clone();
            this.by_ref().map(|t| t.as_str().to_string()).collect()
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

            Err(ParseError::Unfinished(ws, cmds)) => Err(Ok(SyntaxTree {
                leading: linebreak,
                commands: Some((cmds, trailing_linebreak)),
                unparsed: format!("{}{unparsed}", ws.unwrap_or_default()),
            })),

            Err(ParseError::InvalidSyntaxInCmdSub) => Err(Err(ParseError::InvalidSyntaxInCmdSub)),

            Err(e) => Err(Err(e.force_cast())),
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
    fn parse_while(&mut self) -> ParseResult<While>;
    fn parse_do(&mut self) -> ParseResult<Do>;
    fn parse_done(&mut self) -> ParseResult<Done>;
    fn parse_bang(&mut self) -> ParseResult<Bang>;
    fn parse_logical_op(&mut self) -> ParseResult<LogicalOp>;

    fn swallow_whitespace(&mut self) -> LeadingWhitespace;
}

impl<I> Parser for Peekable<I>
where
    I: Iterator<Item = Token> + Clone,
{
    fn parse_complete_commands(&mut self) -> ParseResult<CompleteCommands> {
        let head = match self.parse_complete_command() {
            Ok(cmd) => cmd,
            Err(e) => {
                return Err(e.cast_with(|head| CompleteCommands {
                    head,
                    tail: Default::default(),
                }))
            }
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
        } else if let Err(ParseError::Unfinished(ws, list)) = list_and_separator {
            Err(ParseError::Unfinished(
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
            Err(e) => {
                return Err(e.cast_with(|head| List {
                    head,
                    tail: Default::default(),
                }))
            }
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
                Err(ParseError::None) => {
                    *self = prev;
                    break;
                }
                Err(e) => {
                    return Err(e.cast_with(|and_or_list| {
                        tail.push((sep_op, and_or_list));
                        List { head, tail }
                    }))
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
            Err(e) => {
                return Err(e.cast_with(|head| AndOrList {
                    head,
                    tail: Default::default(),
                }))
            }
        };

        let mut tail = Vec::new();

        loop {
            let Ok(logical_op) = self.parse_logical_op() else { break; };
            let linebreak = self.parse_linebreak();
            let pipeline = match self.parse_pipeline() {
                Ok(pipeline) => pipeline,
                Err(ParseError::None) => {
                    tail.push((logical_op, linebreak, Pipeline::default()));
                    return Err(ParseError::Unfinished(
                        Default::default(),
                        AndOrList { head, tail },
                    ));
                }
                Err(e) => {
                    return Err(e.cast_with(|pipeline| {
                        tail.push((logical_op, linebreak, pipeline));
                        AndOrList { head, tail }
                    }))
                }
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
            Err(ParseError::Unfinished(ws, sequence)) => {
                Err(ParseError::Unfinished(ws, Pipeline { bang, sequence }))
            }
            Err(e) => {
                *self = initial;
                Err(e.force_cast())
            }
        }
    }

    fn parse_pipe_sequence(&mut self) -> ParseResult<PipeSequence> {
        let head = match self.parse_command() {
            Ok(cmd) => cmd,
            Err(e) => {
                return Err(e.cast_with(|cmd| PipeSequence {
                    head: Box::new(cmd),
                    tail: Default::default(),
                }))
            }
        };

        let mut tail = Vec::new();

        loop {
            let Ok(pipe) = self.parse_pipe() else { break; };
            let linebreak = self.parse_linebreak();
            let cmd = match self.parse_command() {
                Ok(cmd) => cmd,
                Err(ParseError::None) => {
                    tail.push((pipe, linebreak, Command::default()));
                    let seq = PipeSequence {
                        head: Box::new(head),
                        tail,
                    };
                    return Err(ParseError::Unfinished(Default::default(), seq));
                }
                Err(e) => {
                    return Err(e.cast_with(|cmd| {
                        tail.push((pipe, linebreak, cmd));
                        PipeSequence {
                            head: Box::new(head),
                            tail,
                        }
                    }))
                }
            };
            tail.push((pipe, linebreak, cmd));
        }

        Ok(PipeSequence {
            head: Box::new(head),
            tail,
        })
    }

    fn parse_command(&mut self) -> ParseResult<Command> {
        match self.parse_function_definition() {
            Ok(f) => Ok(Command::FunctionDefinition(f)),

            Err(e @ ParseError::Unfinished(_, _)) => Err(e.cast_with(Command::FunctionDefinition)),

            _ => match self.parse_compound_command() {
                Ok(c) => Ok(Command::Compound(c, self.parse_redirection_list())),

                Err(e @ ParseError::Unfinished(_, _)) => {
                    let redirs = self.parse_redirection_list();
                    Err(e.cast_with(|c| Command::Compound(c, redirs)))
                }

                _ => match self.parse_simple_command() {
                    Ok(s) => Ok(Command::Simple(s)),

                    Err(e @ ParseError::Unfinished(_, _)) => Err(e.cast_with(Command::Simple)),

                    Err(e) => Err(e.force_cast()),
                },
            },
        }
    }

    fn parse_compound_command(&mut self) -> ParseResult<CompoundCommand> {
        self.parse_while_clause()
            .map(CompoundCommand::While)
            .map_err(|e| e.cast_with(CompoundCommand::While))
    }

    fn parse_subshell(&mut self) -> ParseResult<Subshell> {
        let initial = self.clone();

        let lparen_ws = self.swallow_whitespace();
        if self.consume_single(Token::LParen).is_none() {
            *self = initial;
            return Err(ParseError::None);
        };

        let Ok(body) = self.parse_compound_list() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let rparen_ws = self.swallow_whitespace();
        if self.consume_single(Token::RParen).is_none() {
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
        let head = match self.parse_and_or_list() {
            Ok(list) => list,
            Err(e) => {
                return Err(e.cast_with(|head| Term {
                    head,
                    tail: Default::default(),
                }))
            }
        };

        let mut tail = Vec::new();
        let mut prev = self.clone();

        loop {
            let Ok(separator) = self.parse_separator() else { *self = prev; break; };

            let and_or_list = match self.parse_and_or_list() {
                Ok(list) => list,
                Err(ParseError::Unfinished(ws, list)) => {
                    tail.push((separator, list));
                    return Err(ParseError::Unfinished(ws, Term {
                        head,
                        tail,
                    }));
                },
                Err(e) => return Err(e.force_cast()),
            };

            tail.push((separator, and_or_list));
            prev = self.clone();
        }

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
        let while_part = match self.parse_while() {
            Ok(while_part) => while_part,
            Err(e) => {
                return Err(e.cast_with(|while_part| WhileClause {
                    while_part,
                    predicate: Default::default(),
                    body: Default::default(),
                }))
            }
        };


        let predicate = match self.parse_compound_list() {
            Ok(predicate) => predicate,
            Err(e) => {
                return Err(e.cast_with(|predicate| WhileClause {
                    while_part,
                    predicate,
                    body: Default::default(),
                }))
            }
        };

        let body = match self.parse_do_group() {
            Ok(body) => body,
            Err(e) => {
                return Err(e.cast_with(|body| WhileClause {
                    while_part,
                    predicate,
                    body,
                }))
            }
        };

        Ok(WhileClause {
            while_part,
            predicate,
            body,
        })
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
                return Err(e.cast_with(|name| FunctionDefinition {
                    name,
                    parens: Default::default(),
                    linebreak: Default::default(),
                    body: FunctionBody::default(),
                }));
            }
        };

        let mut parens = String::new();
        for token in [Token::LParen, Token::RParen] {
            parens.push_str(self.swallow_whitespace().as_ref());
            match self.consume_single(token) {
                Some(token) => parens.push_str(&token.as_str()),
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
        let command = match self.parse_compound_command() {
            Ok(cmd) => cmd,
            Err(e) => {
                return Err(e.cast_with(|command| FunctionBody {
                    command,
                    redirections: Default::default(),
                }))
            }
        };

        let redirections = self.parse_redirection_list();

        Ok(FunctionBody {
            command,
            redirections,
        })
    }

    fn parse_brace_group(&mut self) -> ParseResult<BraceGroup> {
        let initial = self.clone();

        let lbrace_ws = self.swallow_whitespace();
        if self
            .consume_single(Token::Reserved(ReservedWord::LBrace))
            .is_none()
        {
            *self = initial;
            return Err(ParseError::None);
        };

        let Ok(body) = self.parse_compound_list() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let rbrace_ws = self.swallow_whitespace();
        if self
            .consume_single(Token::Reserved(ReservedWord::RBrace))
            .is_none()
        {
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
        let do_part = match self.parse_do() {
            Ok(do_part) => do_part,
            Err(e) => return Err(e.cast_with(|do_part| DoGroup {
                do_part,
                body: Default::default(),
                done_part: Default::default(),
            })),
        };

        let body = match self.parse_compound_list() {
            Ok(body) => body,
            Err(e) => {
                return Err(e.cast_with(|body| DoGroup {
                    do_part,
                    body,
                    done_part: Default::default(),
                }))
            }
        };

        let done_part = match self.parse_done() {
            Ok(done_part) => done_part,
            Err(e) => return Err(e.cast_with(|done_part| DoGroup {
                do_part,
                body,
                done_part,
            })),
        };

        Ok(DoGroup {
            do_part,
            body,
            done_part,
        })
    }

    fn parse_simple_command(&mut self) -> ParseResult<SimpleCommand> {
        let initial = self.clone();

        let mut prefixes = Vec::new();
        let mut suffixes = Vec::new();

        loop {
            match self.parse_cmd_prefix() {
                Ok(prefix) => prefixes.push(prefix),
                Err(ParseError::Unfinished(ws, prefix)) => {
                    prefixes.push(prefix);
                    let cmd = SimpleCommand {
                        prefixes,
                        name: None,
                        suffixes,
                    };
                    return Err(ParseError::Unfinished(ws, cmd));
                }
                Err(ParseError::InvalidSyntaxInCmdSub) => {
                    return Err(ParseError::InvalidSyntaxInCmdSub);
                }
                _ => break,
            }
        }

        let name = match self.parse_word(false) {
            Ok(word) => Some(word),
            Err(ParseError::Unfinished(ws, word)) => {
                let cmd = SimpleCommand {
                    prefixes,
                    name: Some(word),
                    suffixes,
                };
                return Err(ParseError::Unfinished(ws, cmd));
            }
            Err(ParseError::InvalidSyntaxInCmdSub) => {
                return Err(ParseError::InvalidSyntaxInCmdSub);
            }
            Err(_) => None,
        };

        loop {
            match self.parse_cmd_suffix(name.is_some()) {
                Ok(suffix) => suffixes.push(suffix),
                Err(ParseError::Unfinished(ws, suffix)) => {
                    suffixes.push(suffix);
                    let cmd = SimpleCommand {
                        prefixes,
                        name,
                        suffixes,
                    };
                    return Err(ParseError::Unfinished(ws, cmd));
                }
                Err(ParseError::InvalidSyntaxInCmdSub) => {
                    return Err(ParseError::InvalidSyntaxInCmdSub);
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
        match self.parse_redirection() {
            Ok(redirection) => Ok(CmdPrefix::Redirection(redirection)),

            Err(ParseError::None) => match self.parse_variable_assignment() {
                Ok(assg) => Ok(CmdPrefix::Assignment(assg)),
                Err(e) => Err(e.cast_with(CmdPrefix::Assignment)),
            },

            Err(e) => Err(e.cast_with(CmdPrefix::Redirection)),
        }
    }

    fn parse_cmd_suffix(&mut self, allow_reserved_words: bool) -> ParseResult<CmdSuffix> {
        match self.parse_redirection() {
            Ok(redirection) => Ok(CmdSuffix::Redirection(redirection)),

            Err(ParseError::None) => match self.parse_word(allow_reserved_words) {
                Ok(word) => Ok(CmdSuffix::Word(word)),
                Err(e) => Err(e.cast_with(CmdSuffix::Word)),
            },

            Err(e) => Err(e.cast_with(CmdSuffix::Redirection)),
        }
    }

    fn parse_newline_list(&mut self) -> ParseResult<NewlineList> {
        let mut whitespace = String::new();
        let mut prev = self.clone();

        loop {
            let ws = self.swallow_whitespace();
            if let Some(Token::Whitespace(c @ '\n')) = self.next() {
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
        if self.consume_single(Token::SyncSeparator).is_some() {
            Ok(SeparatorOp::Sync(ws))
        } else if self.consume_single(Token::AsyncSeparator).is_some() {
            Ok(SeparatorOp::Async(ws))
        } else {
            *self = initial;
            Err(ParseError::None)
        }
    }

    fn parse_separator(&mut self) -> ParseResult<Separator> {
        let initial = self.clone();

        if let Ok((separator_op, linebreak)) = self
            .parse_separator_op()
            .map(|op| (op, self.parse_linebreak()))
        {
            Ok(Separator::Explicit(separator_op, linebreak))
        } else {
            *self = initial;
            self.parse_newline_list()
                .map(Separator::Implicit)
                .map_err(|e| e.force_cast())
        }
    }

    fn parse_sequential_separator(&mut self) -> ParseResult<SequentialSeparator> {
        let initial = self.clone();

        if self.consume_single(Token::SyncSeparator).is_some() {
            let linebreak = self.parse_linebreak();
            Ok(SequentialSeparator::Semi(linebreak))
        } else {
            *self = initial;
            self.parse_newline_list()
                .map(SequentialSeparator::Implicit)
                .map_err(|e| e.force_cast())
        }
    }

    fn parse_name(&mut self) -> ParseResult<Name> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        match self.next() {
            Some(Token::Word(name)) if is_name(&name) => Ok(Name { whitespace, name }),

            _ => {
                *self = initial;
                Err(ParseError::None)
            }
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
        match self.parse_file_redirection() {
            Err(e @ ParseError::Unfinished(_, _)) => Err(e),
            Err(ParseError::None) => match self.parse_here_redirection() {
                Ok(redirection) => Ok(redirection),
                Err(e @ ParseError::Unfinished(_, _)) => Err(e),
                Err(e) => Err(e),
            },
            otherwise => otherwise,
        }
    }

    fn parse_file_redirection(&mut self) -> ParseResult<Redirection> {
        let initial = self.clone();

        let whitespace = self.swallow_whitespace();

        let input_fd = self.parse_file_descriptor().ok();

        let Ok(ty) = self.parse_redirection_type() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let target = match self.parse_word(true) {
            Ok(word) => word,
            Err(ParseError::Unfinished(ws, target)) => {
                return Err(ParseError::Unfinished(
                    ws,
                    Redirection::File {
                        whitespace,
                        input_fd,
                        ty,
                        target,
                    },
                ));
            }
            Err(e) => {
                *self = initial;
                return Err(e.force_cast());
            }
        };

        Ok(Redirection::File {
            whitespace,
            input_fd,
            ty,
            target,
        })
    }

    fn parse_here_redirection(&mut self) -> ParseResult<Redirection> {
        let initial = self.clone();

        let whitespace = self.swallow_whitespace();

        let input_fd = self.parse_file_descriptor().ok();

        let Ok(ty) = self.parse_here_doc_type() else {
            *self = initial;
            return Err(ParseError::None);
        };

        let end = match self.parse_word(true) {
            Ok(word) => word,
            Err(ParseError::Unfinished(ws, end)) => {
                return Err(ParseError::Unfinished(
                    ws,
                    Redirection::Here {
                        whitespace,
                        input_fd,
                        ty,
                        end,
                        // FIXME: actually parse content
                        content: Word::new("", ""),
                    },
                ));
            }
            Err(e) => {
                *self = initial;
                return Err(e.force_cast());
            }
        };

        // FIXME: actually parse content
        let content = Word::new("", "");

        Ok(Redirection::Here {
            whitespace,
            input_fd,
            ty,
            end,
            content,
        })
    }

    fn parse_redirection_type(&mut self) -> ParseResult<RedirectionType> {
        use Token::*;
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
        use Token::*;
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
        let whitespace = self.swallow_whitespace();

        let Ok(lhs) = self.parse_name() else {
            *self = initial;
            return Err(ParseError::None);
        };

        if self.consume_single(Token::Equals).is_none() {
            *self = initial;
            return Err(ParseError::None);
        }

        let rhs = match self.parse_word(true) {
            Ok(word) => Some(word),
            Err(ParseError::None) => None,
            Err(e) => {
                return Err(e.cast_with(|word| VariableAssignment::new(lhs, Some(word), whitespace)))
            }
        };

        Ok(VariableAssignment::new(lhs, rhs, whitespace))
    }

    fn parse_word(&mut self, allow_reserved_words: bool) -> ParseResult<Word> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        let mut full = String::new();
        let mut expansions = Vec::new();
        let mut is_escaped = false;
        let mut in_double_quote = false;
        let mut in_single_quote = false;
        let mut index = 0;

        loop {
            match self.peek() {
                Some(Token::DoubleQuote) if !is_escaped => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = false;
                    if !in_single_quote {
                        in_double_quote ^= true;
                    }
                }

                Some(Token::SingleQuote) if !is_escaped => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = false;
                    if !in_double_quote {
                        in_single_quote ^= true;
                    }
                }

                Some(Token::Backslash) if in_single_quote || is_escaped => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = false;
                }

                Some(Token::QuestionMark) => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = false;
                }

                Some(Token::Backslash) => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = true;
                    if let Some(Token::Whitespace('\n')) = self.peek() {
                        self.next();
                        full += "\n";
                        index += 1;
                        is_escaped = false;
                    }
                }

                Some(Token::Whitespace(c)) if in_double_quote || in_single_quote || is_escaped => {
                    full.push(*c);
                    index += 1;
                    is_escaped = false;
                    self.next();
                }

                Some(Token::Equals) => {
                    full += &self.next().unwrap().as_str();
                    index += 1;
                    is_escaped = false;
                }

                Some(Token::CmdSubStart) if !in_single_quote && !is_escaped => {
                    let token = self.next().unwrap();
                    let mut part = String::from(token.as_str());
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
                        let Some(rparen @ Token::RParen) = self.next() else {
                            // the only time `finished` is true, is if the first
                            // non-whitespace unparsed part is a right paren, meaning
                            // we'll never get to here if that is not the case
                            unreachable!()
                        };
                        part += ws.as_ref();
                        part += &rparen.as_str();
                    }

                    let len = part.len();
                    full += &part;
                    expansions.push(Expansion::Command {
                        range: index..=index + len - 1,
                        part,
                        tree: ast,
                        finished,
                        quoted: in_double_quote,
                    });

                    index += len;
                    is_escaped = false;
                }

                Some(Token::Dollar) if !in_single_quote && !is_escaped => {
                    // TODO: support ${}
                    self.next();
                    let mut parameter = String::new();
                    let mut rest = String::new();

                    match self.peek() {
                        Some(Token::QuestionMark) => {
                            parameter.push('?');
                            self.next();
                        }

                        Some(Token::Word(word)) => {
                            let mut chars = word.chars().peekable();
                            while let Some(c) = chars.peek() {
                                if !is_valid_part_of_name(*c) {
                                    break;
                                }
                                let c = chars.next().unwrap();
                                parameter.push(c);
                            }
                            rest = chars.collect::<String>();
                            self.next();
                        }

                        _ => {}
                    }

                    full.push('$');
                    is_escaped = false;

                    if !parameter.is_empty() {
                        let len = parameter.len();
                        full += &parameter;
                        let expansion = Expansion::Parameter {
                            range: index..=index + len,
                            name: parameter,
                            finished: true,
                            quoted: in_double_quote,
                        };
                        index += len;
                        expansions.push(expansion);
                    }
                    full += &rest;
                    index += rest.len() + 1;
                }

                Some(_) if is_escaped || in_single_quote || in_double_quote => {
                    let token = self.next().unwrap();
                    let token = token.as_str();
                    full += &token;
                    index += token.len();
                    is_escaped = false;
                }

                Some(Token::Tilde) => {
                    self.next();
                    full.push('~');
                    if full == "~" && !is_escaped {
                        match self.peek() {
                            Some(Token::Word(word)) => {
                                let slash_index = word.find('/').unwrap_or(word.len());
                                let name = &word[..slash_index];
                                if name.is_empty() || is_name(name) {
                                    expansions.push(Expansion::Tilde {
                                        range: index..=index + name.len(),
                                        name: name.to_string(),
                                    });
                                }
                                full.push_str(word);
                                index += word.len();
                                self.next();
                            }

                            Some(Token::Whitespace(_)) | None => {
                                expansions.push(Expansion::Tilde {
                                    range: index..=index,
                                    name: String::new(),
                                });
                            }

                            Some(Token::Backslash) => {
                                let prev = self.clone();
                                self.next();
                                if let Some(Token::Whitespace('\n')) = self.next() {
                                    expansions.push(Expansion::Tilde {
                                        range: index..=index,
                                        name: String::new(),
                                    });
                                }
                                *self = prev;
                            }

                            _ => {}
                        }
                    }
                    index += 1;
                    is_escaped = false;
                }

                Some(Token::Pound) if !full.is_empty() => {
                    full.push('#');
                    index += 1;
                    is_escaped = false;
                    self.next();
                }

                Some(Token::Word(xs)) => {
                    full += xs;
                    index += xs.len();
                    is_escaped = false;
                    self.next();
                }

                Some(Token::Reserved(reserved)) if allow_reserved_words => {
                    let res = reserved.as_ref();
                    full += res;
                    index += res.len();
                    is_escaped = false;
                    self.next();
                }

                _ => break,
            }
        }

        let expansions_finished = expansions.iter().all(Expansion::is_finished);

        let mut word = Word::new(&full, ws);
        word.expansions.append(&mut expansions);

        if word.name.is_empty() {
            *self = initial;
            Err(ParseError::None)
        } else if !in_double_quote && !in_single_quote && !is_escaped && expansions_finished {
            Ok(word)
        } else {
            Err(ParseError::Unfinished(None, word))
        }
    }

    fn parse_file_descriptor(&mut self) -> ParseResult<FileDescriptor> {
        let initial = self.clone();

        if let Some(Token::Word(word)) = self.next() {
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

        if self.consume_single(Token::Pound).is_none() {
            *self = initial;
            return Err(ParseError::None);
        };

        let content = self
            .consume_until(|t| matches!(t, Token::Whitespace('\n')))
            .unwrap_or_default()
            .into_iter()
            .map(|t| t.as_str().to_string())
            .collect();

        Ok(Comment {
            whitespace: ws,
            content,
        })
    }

    fn parse_pipe(&mut self) -> ParseResult<Pipe> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(Token::Pipe)
            .map(|_| Pipe { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_while(&mut self) -> ParseResult<While> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(Token::Reserved(ReservedWord::While))
            .map(|_| While { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_do(&mut self) -> ParseResult<Do> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(Token::Reserved(ReservedWord::Do))
            .map(|_| Do { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_done(&mut self) -> ParseResult<Done> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(Token::Reserved(ReservedWord::Done))
            .map(|_| Done { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_bang(&mut self) -> ParseResult<Bang> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(Token::Reserved(ReservedWord::Bang))
            .map(|_| Bang { whitespace })
            .ok_or_else(|| {
                *self = initial;
                ParseError::None
            })
    }

    fn parse_logical_op(&mut self) -> ParseResult<LogicalOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if self.consume_single(Token::And).is_some() {
            Ok(LogicalOp::And(ws))
        } else if self.consume_single(Token::Or).is_some() {
            Ok(LogicalOp::Or(ws))
        } else {
            *self = initial;
            Err(ParseError::None)
        }
    }

    fn swallow_whitespace(&mut self) -> LeadingWhitespace {
        let mut s = LeadingWhitespace::default();
        while let Some(Token::Whitespace(c @ (' ' | '\t'))) = self.peek() {
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
