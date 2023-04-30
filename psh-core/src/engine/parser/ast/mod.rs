pub mod prelude;
pub mod reconstruct;

#[cfg(test)]
mod tests;

use std::iter::Peekable;

use self::prelude::*;
use crate::engine::parser::consumer::Consumer;
use crate::engine::parser::semtok::{ReservedWord, SemanticToken, SemanticTokenizer};
use crate::engine::parser::tok::Tokenizer;
use crate::{Error, Result};

pub fn parse(input: impl AsRef<str>, allow_errors: bool) -> Result<SyntaxTree> {
    match input
        .as_ref()
        .chars()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .tokenize()
        .into_iter()
        .peekable()
        .parse()
    {
        Ok(ast) => Ok(ast),
        Err(ast) if allow_errors => Ok(ast),
        Err(ast) => Err(Error::SyntaxError(format!(
            "could not parse the following: `{}`",
            ast.unparsed.trim_start(),
        ))),
    }
}

pub trait Parser: Iterator<Item = SemanticToken> + std::fmt::Debug + Sized {
    fn parse(&mut self) -> std::result::Result<SyntaxTree, SyntaxTree> {
        let linebreak = self.parse_linebreak();

        let commands = self
            .parse_complete_commands()
            .map(|c| (c, self.parse_linebreak()));

        let mut unparsed = String::new();
        for token in self.by_ref() {
            unparsed.push_str(&token.to_string());
        }

        let ok = unparsed.is_empty() || unparsed.chars().all(|c| c.is_ascii_whitespace());

        let ast = SyntaxTree {
            leading: linebreak,
            commands,
            unparsed,
        };

        if ok {
            Ok(ast)
        } else {
            Err(ast)
        }
    }

    fn parse_complete_commands(&mut self) -> Option<CompleteCommands>;
    fn parse_complete_command(&mut self) -> Option<CompleteCommand>;
    fn parse_list(&mut self) -> Option<List>;
    fn parse_and_or_list(&mut self) -> Option<AndOrList>;
    fn parse_pipeline(&mut self) -> Option<Pipeline>;
    fn parse_pipe_sequence(&mut self) -> Option<PipeSequence>;
    fn parse_command(&mut self) -> Option<Command>;
    fn parse_compound_command(&mut self) -> Option<CompoundCommand>;
    fn parse_subshell(&mut self) -> Option<Subshell>;
    fn parse_compound_list(&mut self) -> Option<CompoundList>;
    fn parse_term(&mut self) -> Option<Term>;
    fn parse_for_clause(&mut self) -> Option<ForClause>;
    fn parse_case_clause(&mut self) -> Option<CaseClause>;
    fn parse_case_list_ns(&mut self) -> Option<CaseListNs>;
    fn parse_case_list(&mut self) -> Option<CaseList>;
    fn parse_case_item_ns(&mut self) -> Option<CaseItemNs>;
    fn parse_case_item(&mut self) -> Option<CaseItem>;
    fn parse_pattern(&mut self) -> Option<Pattern>;
    fn parse_if_clause(&mut self) -> Option<IfClause>;
    fn parse_else_part(&mut self) -> Option<ElsePart>;
    fn parse_while_clause(&mut self) -> Option<WhileClause>;
    fn parse_until_clause(&mut self) -> Option<UntilClause>;
    fn parse_function_definition(&mut self) -> Option<FunctionDefinition>;
    fn parse_function_body(&mut self) -> Option<FunctionBody>;
    fn parse_brace_group(&mut self) -> Option<BraceGroup>;
    fn parse_do_group(&mut self) -> Option<DoGroup>;
    fn parse_simple_command(&mut self) -> Option<SimpleCommand>;
    fn parse_cmd_prefix(&mut self) -> Option<CmdPrefix>;
    fn parse_cmd_suffix(&mut self, allow_reserved_words: bool) -> Option<CmdSuffix>;
    fn parse_newline_list(&mut self) -> Option<NewlineList>;
    fn parse_linebreak(&mut self) -> Linebreak;
    fn parse_separator_op(&mut self) -> Option<SeparatorOp>;
    fn parse_separator(&mut self) -> Option<Separator>;
    fn parse_sequential_separator(&mut self) -> Option<SequentialSeparator>;

    fn parse_name(&mut self) -> Option<Name>;
    fn parse_redirection_list(&mut self) -> Vec<Redirection>;
    fn parse_redirection(&mut self) -> Option<Redirection>;
    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment>;
    fn parse_word(&mut self, allow_reserved_words: bool) -> Option<Word>;
    fn parse_redirection_fd(&mut self) -> Option<Word>;
    fn parse_comment(&mut self) -> Option<Comment>;
    fn parse_pipe(&mut self) -> Option<Pipe>;
    fn parse_bang(&mut self) -> Option<Bang>;
    fn parse_logical_op(&mut self) -> Option<LogicalOp>;

    fn swallow_whitespace(&mut self) -> LeadingWhitespace;
}

impl<T> Parser for Peekable<T>
where
    T: Iterator<Item = SemanticToken> + Clone + std::fmt::Debug,
{
    fn parse_complete_commands(&mut self) -> Option<CompleteCommands> {
        let Some(head) = self.parse_complete_command() else {
            return None;
        };

        let mut tail = Vec::new();

        while let Some((newlines, cmd)) = self
            .parse_newline_list()
            .and_then(|n| self.parse_complete_command().map(|c| (n, c)))
        {
            tail.push((newlines, cmd));
        }

        Some(CompleteCommands { head, tail })
    }

    fn parse_complete_command(&mut self) -> Option<CompleteCommand> {
        let initial = self.clone();

        let list_and_separator = self
            .parse_list()
            .map(|list| (list, self.parse_separator_op()));

        let comment = self.parse_comment();

        if let Some((list, separator_op)) = list_and_separator {
            Some(CompleteCommand::List(list, separator_op, comment))
        } else if let Some(comment) = comment {
            Some(CompleteCommand::Comment(comment))
        } else {
            *self = initial;
            None
        }
    }

    fn parse_list(&mut self) -> Option<List> {
        let Some(head) = self.parse_and_or_list() else {
            return None;
        };

        let mut prev = self.clone();
        let mut tail = Vec::new();

        while let Some(thing) = self
            .parse_separator_op()
            .and_then(|s| self.parse_and_or_list().map(|a| (s, a)))
            .or_else(|| {
                *self = prev.clone();
                None
            })
        {
            tail.push(thing);
            prev = self.clone();
        }

        Some(List { head, tail })
    }

    fn parse_and_or_list(&mut self) -> Option<AndOrList> {
        let Some(head) = self.parse_pipeline() else {
            return None;
        };

        let mut prev = self.clone();
        let mut tail = Vec::new();

        while let Some(thing) = self
            .parse_logical_op()
            .map(|op| (op, self.parse_linebreak()))
            .and_then(|(op, l)| self.parse_pipeline().map(|c| (op, l, c)))
            .or_else(|| {
                *self = prev.clone();
                None
            })
        {
            tail.push(thing);
            prev = self.clone();
        }

        Some(AndOrList { head, tail })
    }

    fn parse_pipeline(&mut self) -> Option<Pipeline> {
        let initial = self.clone();
        let bang = self.parse_bang();

        let Some(sequence) = self.parse_pipe_sequence() else {
            *self = initial;
            return None;
        };

        Some(Pipeline { bang, sequence })
    }

    fn parse_pipe_sequence(&mut self) -> Option<PipeSequence> {
        let Some(head) = self.parse_command() else {
            return None;
        };

        let mut prev = self.clone();
        let mut tail = Vec::new();

        while let Some(part) = self
            .parse_pipe()
            .map(|pipe| (pipe, self.parse_linebreak()))
            .and_then(|(pipe, l)| self.parse_command().map(|cmd| (pipe, l, cmd)))
            .or_else(|| {
                *self = prev;
                None
            })
        {
            tail.push(part);
            prev = self.clone();
        }

        Some(PipeSequence {
            head: Box::new(head),
            tail,
        })
    }

    fn parse_command(&mut self) -> Option<Command> {
        self.parse_function_definition()
            .map(Command::FunctionDefinition)
            .or_else(|| {
                self.parse_compound_command()
                    .map(|c| (c, self.parse_redirection_list()))
                    .map(|(c, r)| Command::Compound(c, r))
            })
            .or_else(|| self.parse_simple_command().map(Command::Simple))
    }

    fn parse_compound_command(&mut self) -> Option<CompoundCommand> {
        self.parse_brace_group()
            .map(CompoundCommand::Brace)
            .or_else(|| self.parse_subshell().map(CompoundCommand::Subshell))
            .or_else(|| self.parse_for_clause().map(CompoundCommand::For))
            .or_else(|| self.parse_case_clause().map(CompoundCommand::Case))
            .or_else(|| self.parse_if_clause().map(CompoundCommand::If))
            .or_else(|| self.parse_while_clause().map(CompoundCommand::While))
            .or_else(|| self.parse_until_clause().map(CompoundCommand::Until))
    }

    fn parse_subshell(&mut self) -> Option<Subshell> {
        let initial = self.clone();

        let lparen_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::LParen) else {
            *self = initial;
            return None;
        };

        let Some(body) = self.parse_compound_list() else {
            *self = initial;
            return None;
        };

        let rparen_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::RParen) else {
            *self = initial;
            return None;
        };

        Some(Subshell {
            lparen_ws,
            body,
            rparen_ws,
        })
    }

    fn parse_compound_list(&mut self) -> Option<CompoundList> {
        let initial = self.clone();
        let linebreak = self.parse_linebreak();

        let Some(term) = self.parse_term() else {
            *self = initial;
            return None;
        };

        let separator = self.parse_separator();

        Some(CompoundList {
            linebreak,
            term,
            separator,
        })
    }

    fn parse_term(&mut self) -> Option<Term> {
        let Some(head) = self.parse_and_or_list() else {
            return None;
        };

        let mut prev = self.clone();
        let mut tail = Vec::new();
        while let Some((sep, and_or)) = self
            .parse_separator()
            .and_then(|sep| self.parse_and_or_list().map(|a| (sep, a)))
        {
            tail.push((sep, and_or));
            prev = self.clone();
        }

        *self = prev;

        Some(Term { head, tail })
    }

    fn parse_for_clause(&mut self) -> Option<ForClause> {
        None
    }

    fn parse_case_clause(&mut self) -> Option<CaseClause> {
        None
    }

    fn parse_case_list_ns(&mut self) -> Option<CaseListNs> {
        None
    }

    fn parse_case_list(&mut self) -> Option<CaseList> {
        None
    }

    fn parse_case_item_ns(&mut self) -> Option<CaseItemNs> {
        None
    }

    fn parse_case_item(&mut self) -> Option<CaseItem> {
        None
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        None
    }

    fn parse_if_clause(&mut self) -> Option<IfClause> {
        None
    }

    fn parse_else_part(&mut self) -> Option<ElsePart> {
        None
    }

    fn parse_while_clause(&mut self) -> Option<WhileClause> {
        None
    }

    fn parse_until_clause(&mut self) -> Option<UntilClause> {
        None
    }

    fn parse_function_definition(&mut self) -> Option<FunctionDefinition> {
        let initial = self.clone();

        let Some(name) = self.parse_name() else {
            return None;
        };

        let mut parens = String::new();
        for token in [SemanticToken::LParen, SemanticToken::RParen] {
            parens.push_str(&self.swallow_whitespace());
            match self.consume_single(token) {
                Some(token) => parens.push_str(&token.to_string()),
                None => {
                    *self = initial;
                    return None;
                }
            };
        }

        let linebreak = self.parse_linebreak();

        let Some(body) = self.parse_function_body() else {
            *self = initial;
            return None;
        };

        Some(FunctionDefinition {
            name,
            parens,
            linebreak,
            body,
        })
    }

    fn parse_function_body(&mut self) -> Option<FunctionBody> {
        let Some(command) = self.parse_compound_command() else {
            return None;
        };

        let redirections = self.parse_redirection_list();

        Some(FunctionBody {
            command,
            redirections,
        })
    }

    fn parse_brace_group(&mut self) -> Option<BraceGroup> {
        let initial = self.clone();

        let lbrace_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::Reserved(ReservedWord::LBrace)) else {
            *self = initial;
            return None;
        };

        let Some(body) = self.parse_compound_list() else {
            *self = initial;
            return None;
        };

        let rbrace_ws = self.swallow_whitespace();
        let Some(_) = self.consume_single(SemanticToken::Reserved(ReservedWord::RBrace)) else {
            *self = initial;
            return None;
        };

        Some(BraceGroup {
            lbrace_ws,
            body,
            rbrace_ws,
        })
    }

    fn parse_do_group(&mut self) -> Option<DoGroup> {
        let initial = self.clone();

        // FIXME: whitespace
        self.consume_single(SemanticToken::Reserved(ReservedWord::Do))
            .and_then(|_| self.parse_compound_list())
            .and_then(|list| {
                self.consume_single(SemanticToken::Reserved(ReservedWord::Done))
                    .map(|_| list)
            })
            .map(|body| DoGroup { body })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_simple_command(&mut self) -> Option<SimpleCommand> {
        let initial = self.clone();

        let mut prefixes = Vec::new();
        let mut suffixes = Vec::new();

        while let Some(prefix) = self.parse_cmd_prefix() {
            prefixes.push(prefix);
        }

        let name = self.parse_word(false);

        while let Some(suffix) = self.parse_cmd_suffix(name.is_some()) {
            suffixes.push(suffix);
        }

        if name.is_none() && prefixes.is_empty() && suffixes.is_empty() {
            *self = initial;
            None
        } else {
            Some(SimpleCommand {
                name,
                prefixes,
                suffixes,
            })
        }
    }

    fn parse_cmd_prefix(&mut self) -> Option<CmdPrefix> {
        self.parse_redirection()
            .map(CmdPrefix::Redirection)
            .or_else(|| self.parse_variable_assignment().map(CmdPrefix::Assignment))
    }

    fn parse_cmd_suffix(&mut self, allow_reserved_words: bool) -> Option<CmdSuffix> {
        self.parse_redirection()
            .map(CmdSuffix::Redirection)
            .or_else(|| self.parse_word(allow_reserved_words).map(CmdSuffix::Word))
    }

    fn parse_newline_list(&mut self) -> Option<NewlineList> {
        let mut prev = self.clone();
        let mut whitespace = String::new();

        loop {
            let ws = self.swallow_whitespace();
            if self
                .consume_single(SemanticToken::Whitespace('\n'))
                .is_some()
            {
                whitespace.push_str(&ws);
                whitespace.push('\n');
                prev = self.clone();
            } else {
                *self = prev;
                break;
            }
        }

        if whitespace.is_empty() {
            None
        } else {
            Some(NewlineList { whitespace })
        }
    }

    fn parse_linebreak(&mut self) -> Linebreak {
        let newlines = self.parse_newline_list();
        Linebreak { newlines }
    }

    fn parse_separator_op(&mut self) -> Option<SeparatorOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();
        self.consume_single(SemanticToken::SyncSeparator)
            .or_else(|| self.consume_single(SemanticToken::AsyncSeparator))
            .map(|t| match t {
                SemanticToken::SyncSeparator => SeparatorOp::Sync(ws),
                SemanticToken::AsyncSeparator => SeparatorOp::Async(ws),
                _ => unreachable!(),
            })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_separator(&mut self) -> Option<Separator> {
        let initial = self.clone();

        if let Some((separator_op, linebreak)) = self
            .parse_separator_op()
            .map(|op| (op, self.parse_linebreak()))
        {
            return Some(Separator::Explicit(separator_op, linebreak));
        }

        *self = initial;
        self.parse_newline_list().map(Separator::Implicit)
    }

    fn parse_sequential_separator(&mut self) -> Option<SequentialSeparator> {
        let initial = self.clone();

        if self.consume_single(SemanticToken::SyncSeparator).is_some() {
            let linebreak = self.parse_linebreak();
            return Some(SequentialSeparator::Semi(linebreak));
        }

        *self = initial;
        self.parse_newline_list().map(SequentialSeparator::Implicit)
    }

    fn parse_name(&mut self) -> Option<Name> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if let Some(SemanticToken::Word(word)) =
            self.consume_if(|t| matches!(t, SemanticToken::Word(_)))
        {
            if is_name(&word) {
                Some(Name {
                    whitespace: ws,
                    name: word,
                })
            } else {
                *self = initial;
                None
            }
        } else {
            None
        }
    }

    fn parse_redirection_list(&mut self) -> Vec<Redirection> {
        let mut redirs = Vec::new();
        while let Some(redir) = self.parse_redirection() {
            redirs.push(redir);
        }
        redirs
    }

    fn parse_redirection(&mut self) -> Option<Redirection> {
        let initial = self.clone();

        let fd = self.parse_redirection_fd().unwrap_or_else(|| {
            let ws = self.swallow_whitespace();
            Word::new("", ws)
        });

        match self
            .consume_single(SemanticToken::RedirectOutput)
            .or_else(|| self.consume_single(SemanticToken::RedirectInput))
        {
            Some(SemanticToken::RedirectInput) => {
                let is_here_doc = self.consume_single(SemanticToken::RedirectInput).is_some();

                // FIXME: if ampersand is used, "target" should be a file descriptor.
                //        probably needs to be fixed in the enum definition
                let target_is_fd = self.consume_single(SemanticToken::AsyncSeparator).is_some();

                match self.parse_word(true) {
                    Some(target) if is_here_doc => Some(Redirection::HereDocument {
                        file_descriptor: fd,
                        delimiter: target,
                    }),

                    Some(target) => Some(Redirection::Input {
                        file_descriptor: fd,
                        target,
                        target_is_fd,
                    }),

                    None => {
                        *self = initial;
                        None
                    }
                }
            }

            Some(SemanticToken::RedirectOutput) => {
                let append = self.consume_single(SemanticToken::RedirectOutput).is_some();

                // FIXME: if ampersand is used, "target" should be a file descriptor.
                //        probably needs to be fixed in the enum definition
                let target_is_fd = self.consume_single(SemanticToken::AsyncSeparator).is_some();

                let target = match self.parse_word(true) {
                    Some(word) => word,
                    None => {
                        *self = initial;
                        return None;
                    }
                };

                Some(Redirection::Output {
                    file_descriptor: fd,
                    append,
                    target,
                    target_is_fd,
                })
            }

            _ => {
                *self = initial;
                None
            }
        }
    }

    fn parse_variable_assignment(&mut self) -> Option<VariableAssignment> {
        let initial = self.clone();

        if let Some(Word {
            whitespace, name, ..
        }) = self.parse_word(false)
        {
            match name.split_once('=') {
                Some((lhs, rhs)) if is_name(lhs) => {
                    let lhs = Name {
                        whitespace: "".to_string(),
                        name: lhs.to_string(),
                    };

                    let rhs = if !rhs.is_empty() {
                        Some(Word::new(rhs, ""))
                    } else {
                        None
                    };

                    let var_assg = VariableAssignment::new(lhs, rhs, whitespace);
                    return Some(var_assg);
                }
                _ => {}
            }
        }

        *self = initial;
        None
    }

    fn parse_word(&mut self, allow_reserved_words: bool) -> Option<Word> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        match self.peek() {
            Some(SemanticToken::Word(xs)) => {
                let word = Word::new(xs, ws);
                self.next();
                Some(word)
            }

            Some(SemanticToken::Reserved(reserved)) if allow_reserved_words => {
                let word = Word::new(&reserved.to_string(), ws);
                self.next();
                Some(word)
            }

            _ => {
                *self = initial;
                None
            }
        }
    }

    fn parse_redirection_fd(&mut self) -> Option<Word> {
        let ws = self.swallow_whitespace();

        match self.peek() {
            Some(SemanticToken::RedirectOutput | SemanticToken::RedirectInput) => {
                Some(Word::new("", ws))
            }

            Some(SemanticToken::Word(word)) if word.len() == 1 => match word.chars().next() {
                Some('0'..='9') => {
                    let word = Word::new(word, ws);
                    self.next();
                    Some(word)
                }
                _ => None,
            },

            _ => None,
        }
    }

    fn parse_comment(&mut self) -> Option<Comment> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        if let Some(SemanticToken::Comment(comment)) = self.next() {
            Some(Comment {
                whitespace: ws,
                content: comment,
            })
        } else {
            *self = initial;
            None
        }
    }

    fn parse_pipe(&mut self) -> Option<Pipe> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(SemanticToken::Pipe)
            .map(|_| Pipe { whitespace })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_bang(&mut self) -> Option<Bang> {
        let initial = self.clone();
        let whitespace = self.swallow_whitespace();

        self.consume_single(SemanticToken::Reserved(ReservedWord::Bang))
            .map(|_| Bang { whitespace })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn parse_logical_op(&mut self) -> Option<LogicalOp> {
        let initial = self.clone();
        let ws = self.swallow_whitespace();

        self.consume_single(SemanticToken::And)
            .or_else(|| self.consume_single(SemanticToken::Or))
            .map(|t| match t {
                SemanticToken::And => LogicalOp::And(ws),
                SemanticToken::Or => LogicalOp::Or(ws),
                _ => unreachable!(),
            })
            .or_else(|| {
                *self = initial;
                None
            })
    }

    fn swallow_whitespace(&mut self) -> LeadingWhitespace {
        let mut s = LeadingWhitespace::new();
        while let Some(SemanticToken::Whitespace(c @ (' ' | '\t'))) = self.peek() {
            s.push(*c);
            self.next();
        }
        s
    }
}

fn is_valid_part_of_name(c: char) -> bool {
    matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
}

fn is_name(input: impl AsRef<str>) -> bool {
    let mut input = input.as_ref().chars().peekable();
    match input.peek() {
        Some('0'..='9') => false,
        None => false,
        _ => input.all(is_valid_part_of_name),
    }
}
