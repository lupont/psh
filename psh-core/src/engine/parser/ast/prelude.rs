use std::ops::RangeInclusive;

use crate::path;

pub use super::parse;
pub use super::reconstruct;
pub use super::Parser;

/// Type alias used in data structures that keep track of whitespace.
pub type LeadingWhitespace = String;

/// program : linebreak complete_commands linebreak
///         | linebreak
///         ;
#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    pub leading: Linebreak,
    pub commands: Option<(CompleteCommands, Linebreak)>,
    pub unparsed: String,
}

/// complete_commands: complete_commands newline_list complete_command
///                  |                                complete_command
///                  ;
#[derive(Debug, PartialEq, Eq)]
pub struct CompleteCommands {
    pub head: CompleteCommand,
    pub tail: Vec<(NewlineList, CompleteCommand)>,
}

impl CompleteCommands {
    pub fn full(&self) -> Vec<&CompleteCommand> {
        let mut v = vec![&self.head];
        for (_, cmd) in &self.tail {
            v.push(cmd);
        }
        v
    }
}

/// complete_command : list separator_op
///                  | list
///                  ;
#[derive(Debug, PartialEq, Eq)]
pub enum CompleteCommand {
    List(List, Option<SeparatorOp>, Option<Comment>),
    Comment(Comment),
}

impl CompleteCommand {
    pub fn list_with_separator(&self) -> Vec<(&AndOrList, SeparatorOp)> {
        let mut items = Vec::new();

        let (list, separator_op) = match self {
            Self::List(list, separator_op, _) => (list, separator_op),
            Self::Comment(_) => return items,
        };

        let final_separator = match separator_op {
            Some(separator) => separator.clone(),
            None => Default::default(),
        };

        if list.tail.is_empty() {
            items.push((&list.head, final_separator));
        } else {
            let mut prev_list = &list.head;

            for (sep, and_or_list) in &list.tail {
                items.push((prev_list, sep.clone()));
                prev_list = and_or_list;
            }

            if let Some((_, and_or_list)) = list.tail.last() {
                items.push((and_or_list, final_separator));
            }
        }

        items
    }
}

/// list : list separator_op and_or
///      |                   and_or
///      ;
#[derive(Debug, PartialEq, Eq)]
pub struct List {
    pub head: AndOrList,
    pub tail: Vec<(SeparatorOp, AndOrList)>,
}

/// and_or :                         pipeline
///        | and_or AND_IF linebreak pipeline
///        | and_or OR_IF  linebreak pipeline
///        ;
#[derive(Debug, PartialEq, Eq)]
pub struct AndOrList {
    pub head: Pipeline,

    // As noted semantically by having it be the first part of
    // the tuple, each `LogicalOp` here operates on the previous
    // `Pipeline` and it's tuple partner.
    pub tail: Vec<(LogicalOp, Linebreak, Pipeline)>,
}

impl AndOrList {
    pub fn all_pipelines(&self) -> Vec<&Pipeline> {
        let mut pipelines = vec![&self.head];
        for (_, _, p) in &self.tail {
            pipelines.push(p);
        }
        pipelines
    }
}

/// pipeline :      pipe_sequence
///          | Bang pipe_sequence
///          ;
#[derive(Debug, PartialEq, Eq)]
pub struct Pipeline {
    pub bang: Option<Bang>,
    pub sequence: PipeSequence,
}

impl Pipeline {
    /// Always at least one in length, since this joins self.first and self.rest.
    pub fn full(&self) -> Vec<&Command> {
        let mut v = vec![&*self.sequence.head];
        for (_, _, cmd) in &self.sequence.tail {
            v.push(cmd);
        }
        v
    }

    pub fn has_bang(&self) -> bool {
        self.bang.is_some()
    }
}

/// pipe_sequence :                             command
///               | pipe_sequence '|' linebreak command
///               ;
#[derive(Debug, PartialEq, Eq)]
pub struct PipeSequence {
    pub head: Box<Command>,
    pub tail: Vec<(Pipe, Linebreak, Command)>,
}

/// command : simple_command
///         | compound_command
///         | compound_command redirect_list
///         | function_definition
///         ;
#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Simple(SimpleCommand),
    Compound(CompoundCommand, Vec<Redirection>),
    FunctionDefinition(FunctionDefinition),
}

/// compound_command : brace_group
///                  | subshell
///                  | for_clause
///                  | case_clause
///                  | if_clause
///                  | while_clause
///                  | until_clause
///                  ;
#[derive(Debug, PartialEq, Eq)]
pub enum CompoundCommand {
    Brace(BraceGroup),
    Subshell(Subshell),
    For(ForClause),
    Case(CaseClause),
    If(IfClause),
    While(WhileClause),
    Until(UntilClause),
}

/// subshell : '(' compound_list ')'
///          ;
#[derive(Debug, PartialEq, Eq)]
pub struct Subshell {
    pub lparen_ws: LeadingWhitespace,
    pub body: CompoundList,
    pub rparen_ws: LeadingWhitespace,
}

/// compound_list : linebreak term
///               | linebreak term separator
///               ;
#[derive(Debug, PartialEq, Eq)]
pub struct CompoundList {
    pub linebreak: Linebreak,
    pub term: Term,
    pub separator: Option<Separator>,
}

/// term : term separator and_or
///      |                and_or
///      ;
#[derive(Debug, PartialEq, Eq)]
pub struct Term {
    pub head: AndOrList,
    pub tail: Vec<(Separator, AndOrList)>,
}

/// for_clause : For name                                      do_group
///            | For name                       sequential_sep do_group
///            | For name linebreak in          sequential_sep do_group
///            | For name linebreak in wordlist sequential_sep do_group
///            ;
#[derive(Debug, PartialEq, Eq)]
pub enum ForClause {
    Simple(Name, DoGroup),
    Padded(Name, SequentialSeparator, DoGroup),
    Full(Name, Linebreak, Vec<Word>, SequentialSeparator, DoGroup),
}

/// name : NAME /* Apply rule 5 */
///      ;
#[derive(Debug, PartialEq, Eq)]
pub struct Name {
    pub whitespace: LeadingWhitespace,
    pub name: String,
}

/// case_clause : Case WORD linebreak in linebreak case_list    Esac
///             | Case WORD linebreak in linebreak case_list_ns Esac
///             | Case WORD linebreak in linebreak              Esac
///             ;
#[derive(Debug, PartialEq, Eq)]
pub enum CaseClause {
    Normal(Word, Linebreak, Linebreak, CaseList),
    NoSeparator(Word, Linebreak, Linebreak, CaseListNs),
    Empty(Word, Linebreak, Linebreak),
}

/// case_list_ns : case_list case_item_ns
///              |           case_item_ns
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct CaseListNs {
    pub case_list: Option<CaseList>,
    pub last: CaseItemNs,
}

/// case_list : case_list case_item
///           |           case_item
///           ;
#[derive(Debug, PartialEq, Eq)]
pub struct CaseList {
    pub head: CaseItem,
    pub tail: Vec<CaseItem>,
}

/// case_item_ns :     pattern ')' linebreak
///              |     pattern ')' compound_list
///              | '(' pattern ')' linebreak
///              | '(' pattern ')' compound_list
///              ;
#[derive(Debug, PartialEq, Eq)]
pub enum CaseItemNs {
    Empty(bool, Pattern, Linebreak),
    List(bool, Pattern, CompoundList),
}

/// case_item :     pattern ')' linebreak     DSEMI linebreak
///           |     pattern ')' compound_list DSEMI linebreak
///           | '(' pattern ')' linebreak     DSEMI linebreak
///           | '(' pattern ')' compound_list DSEMI linebreak
///           ;
#[derive(Debug, PartialEq, Eq)]
pub enum CaseItem {
    Empty(bool, Pattern, Linebreak, Linebreak),
    List(bool, Pattern, CompoundList, Linebreak),
}

/// pattern :             WORD /* Apply rule 4 */
///         | pattern '|' WORD /* Do not apply rule 4 */
///         ;
#[derive(Debug, PartialEq, Eq)]
pub struct Pattern {
    pub head: Word,
    pub tail: Vec<Word>,
}

/// if_clause : If compound_list Then compound_list else_part Fi
///           | If compound_list Then compound_list           Fi
///           ;
#[derive(Debug, PartialEq, Eq)]
pub struct IfClause {
    pub predicate: CompoundList,
    pub body: CompoundList,
    pub else_part: Option<ElsePart>,
}

/// else_part : Elif compound_list Then compound_list
///           | Elif compound_list Then compound_list else_part
///           | Else compound_list
///           ;
#[derive(Debug, PartialEq, Eq)]
pub struct ElsePart {
    pub elseifs: Vec<(CompoundList, CompoundList)>,
    pub else_part: Option<CompoundList>,
}

/// while_clause : While compound_list do_group
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct WhileClause {
    pub predicate: CompoundList,
    pub body: DoGroup,
}

/// until_clause : Until compound_list do_group
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct UntilClause {
    pub predicate: CompoundList,
    pub body: DoGroup,
}

/// function_definition : fname '(' ')' linebreak function_body
///                  ;
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Name,
    pub parens: String,
    pub linebreak: Linebreak,
    pub body: FunctionBody,
}

/// function_body : compound_command               /* Apply rule 9 */
///               | compound_command redirect_list /* Apply rule 9 */
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub command: CompoundCommand,
    pub redirections: Vec<Redirection>,
}

/// brace_group  : Lbrace compound_list Rbrace
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct BraceGroup {
    pub lbrace_ws: LeadingWhitespace,
    pub body: CompoundList,
    pub rbrace_ws: LeadingWhitespace,
}

/// do_group : Do compound_list Done /* Apply rule 6 */
///          ;
#[derive(Debug, PartialEq, Eq)]
pub struct DoGroup {
    pub body: CompoundList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    pub name: Option<Word>,
    pub prefixes: Vec<CmdPrefix>,
    pub suffixes: Vec<CmdSuffix>,
}

impl SimpleCommand {
    pub fn name(&self) -> Option<&String> {
        if let Some(word) = &self.name {
            Some(&word.name)
        } else {
            None
        }
    }

    pub fn args(&self) -> impl Iterator<Item = &String> {
        self.suffixes
            .iter()
            .filter_map(|m| match m {
                CmdSuffix::Word(w) => Some(w),
                _ => None,
            })
            .map(|w| &w.name)
    }

    pub fn assignments(&self) -> impl Iterator<Item = &VariableAssignment> {
        self.prefixes.iter().filter_map(|m| match m {
            CmdPrefix::Assignment(a) => Some(a),
            _ => None,
        })
    }

    pub fn redirections(&self) -> impl Iterator<Item = &Redirection> {
        self.prefixes
            .iter()
            .filter_map(|m| match m {
                CmdPrefix::Redirection(r) => Some(r),
                _ => None,
            })
            .chain(self.suffixes.iter().filter_map(|m| match m {
                CmdSuffix::Redirection(r) => Some(r),
                _ => None,
            }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CmdPrefix {
    Redirection(Redirection),
    Assignment(VariableAssignment),
}

#[derive(Debug, PartialEq, Eq)]
pub enum CmdSuffix {
    Redirection(Redirection),
    Word(Word),
}

// We might want to do something like this. The reason for not doing it
// now is that I'm not sure how FileDescriptor should work, since (as I
// understand it), the amount of valid file descriptors can vary. Would
// be used in the Redirection enum like this (extract):
//     enum Redirection {
//       Input {
//         file_descriptor: Option<FileDescriptor>,
//         target: Either<Word, FileDescriptor>,
//       },
//     }
// The target being a FileDescriptor would mean that it was invoked as
// e.g. `echo foo >&2`. This would remove the need for the
// `target_is_fd` field.
//
// enum Either<L, R> {
//     Left(L),
//     Right(R),
// }

// #[derive(Debug, PartialEq, Eq)]
// pub enum FileDescriptor {
//     Stdin,
//     Stdout,
//     Stderr,
// }

#[derive(Debug, PartialEq, Eq)]
pub enum Redirection {
    Output {
        file_descriptor: Word,
        append: bool,
        target: Word,
        target_is_fd: bool,
    },

    Input {
        file_descriptor: Word,
        target: Word,
        target_is_fd: bool,
    },

    HereDocument {
        file_descriptor: Word,
        delimiter: Word,
    },
}

impl Redirection {
    pub fn new_output(fd: Word, target: Word, append: bool, target_is_fd: bool) -> Self {
        Self::Output {
            file_descriptor: fd,
            append,
            target,
            target_is_fd,
        }
    }

    pub fn new_input(fd: Word, target: Word, target_is_fd: bool) -> Self {
        Self::Input {
            file_descriptor: fd,
            target,
            target_is_fd,
        }
    }

    pub fn new_here_doc(fd: Word, delimiter: Word) -> Self {
        Self::HereDocument {
            file_descriptor: fd,
            delimiter,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableAssignment {
    pub whitespace: LeadingWhitespace,
    pub lhs: Name,
    pub rhs: Option<Word>,
}

impl VariableAssignment {
    pub fn new(lhs: Name, rhs: Option<Word>, whitespace: impl Into<LeadingWhitespace>) -> Self {
        Self {
            whitespace: whitespace.into(),
            lhs,
            rhs,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum QuoteState {
    Single,
    Double,
    None,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Word {
    pub whitespace: LeadingWhitespace,
    pub name: String,
    pub expansions: Vec<Expansion>,
}

impl Word {
    pub fn new(input: &str, whitespace: impl Into<LeadingWhitespace>) -> Self {
        let expansions = Self::find_expansions(input);

        Self {
            name: input.to_string(),
            whitespace: whitespace.into(),
            expansions,
        }
    }

    fn find_expansions(input: &str) -> Vec<Expansion> {
        let mut expansions = Vec::new();

        if let Some(tilde) = Self::find_tilde_expansion(input) {
            expansions.push(tilde);
        }

        let parameters = Self::find_parameter_expansions(input);
        expansions.extend(&mut parameters.into_iter());

        if let Some(cmd_sub) = Self::find_cmd_sub_expansions(input) {
            expansions.push(cmd_sub);
        }

        expansions
    }

    fn find_parameter_expansions(input: &str) -> Vec<Expansion> {
        let mut state = QuoteState::None;
        let mut is_escaped = false;

        let mut expansions = Vec::new();
        let mut curr_expansion = None::<String>;
        let mut curr_expansion_start = 0;
        let mut curr_expansion_end = 0;

        let chars = input.chars().peekable();

        for (index, c) in chars.enumerate() {
            match (c, state) {
                ('\'', QuoteState::Single) => {
                    state = QuoteState::None;
                }
                ('\'', QuoteState::None) => {
                    state = QuoteState::Single;
                }

                ('"', QuoteState::Double) if !is_escaped => {
                    state = QuoteState::None;
                }
                ('"', QuoteState::None) => {
                    state = QuoteState::Double;
                }

                ('\\', QuoteState::None | QuoteState::Double) => is_escaped = !is_escaped,

                ('$', QuoteState::None | QuoteState::Double) if !is_escaped => {
                    if matches!(&curr_expansion, Some(s) if !s.is_empty()) {
                        expansions.push(Expansion::Parameter {
                            range: curr_expansion_start..=curr_expansion_end,
                            name: curr_expansion.unwrap(),
                        });
                    }
                    curr_expansion = Some(String::new());
                    curr_expansion_start = index;
                }

                (c, _) if !is_escaped && curr_expansion.is_some() => {
                    if super::is_valid_part_of_name(c) {
                        curr_expansion.as_mut().unwrap().push(c);
                        curr_expansion_end = index;
                    } else {
                        let parameter = curr_expansion.unwrap();
                        if !parameter.is_empty() {
                            expansions.push(Expansion::Parameter {
                                range: curr_expansion_start..=curr_expansion_end,
                                name: parameter,
                            });
                        }
                        curr_expansion = None;
                    }
                }

                (_, _) => {}
            }

            if !matches!((c, state), ('\\', QuoteState::None | QuoteState::Double)) {
                is_escaped = false;
            }
        }

        if let Some(exp) = curr_expansion {
            if !exp.is_empty() {
                expansions.push(Expansion::Parameter {
                    range: curr_expansion_start..=curr_expansion_end,
                    name: exp,
                });
            }
        }

        expansions
    }

    fn find_tilde_expansion(input: &str) -> Option<Expansion> {
        if !matches!(input.chars().next(), Some('~')) {
            return None;
        }

        let slash_index = match Self::find(QuoteState::None, input, '/', true) {
            Some(index) => index,
            None => input.len(),
        };

        let name = &input[1..slash_index];

        if !path::is_portable_filename(name) {
            return None;
        }

        Some(Expansion::Tilde {
            range: 0..=slash_index - 1,
            name: name.to_string(),
        })
    }

    fn find_cmd_sub_expansions(input: &str) -> Option<Expansion> {
        let unquoted_start_index = match Self::find(QuoteState::None, input, '$', true) {
            Some(dollar_index) => match Self::find(QuoteState::None, input, '(', true) {
                Some(lparen_index) if dollar_index + 1 == lparen_index => Some(dollar_index),

                _ => None,
            },

            None => None,
        };
        let unquoted_end_index = Self::find(QuoteState::None, input, ')', false);

        match (unquoted_start_index, unquoted_end_index) {
            (Some(start), Some(end)) => {
                let sub = &input[start + 2..=end - 1];
                let expansion = Expansion::Command {
                    range: start..=end,
                    part: sub.to_string(),
                    tree: parse(sub, false).unwrap(),
                };

                Some(expansion)
            }

            _ => None,
        }
    }

    pub fn find(
        target_state: QuoteState,
        haystack: &str,
        needle: char,
        first: bool,
    ) -> Option<usize> {
        let mut state = QuoteState::None;
        let mut is_escaped = false;

        let mut found = None;

        for (i, c) in haystack.chars().enumerate() {
            match (c, state) {
                ('\'', QuoteState::Single) => {
                    state = QuoteState::None;
                }
                ('\'', QuoteState::None) => {
                    state = QuoteState::Single;
                }

                ('"', QuoteState::Double) if !is_escaped => {
                    state = QuoteState::None;
                }
                ('"', QuoteState::None) => {
                    state = QuoteState::Double;
                }

                ('\\', _) if !is_escaped => is_escaped = true,

                (c, _) => {
                    is_escaped = false;
                    if state == target_state && c == needle {
                        found = Some(i);
                        if first {
                            break;
                        }
                    }
                }
            }
        }

        found
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expansion {
    Tilde {
        range: RangeInclusive<usize>,
        name: String,
    },

    Glob {
        range: RangeInclusive<usize>,
        recursive: bool,
        pattern: String,
    },

    Brace {
        range: RangeInclusive<usize>,
        pattern: String,
    },

    Parameter {
        range: RangeInclusive<usize>,
        name: String,
    },

    Command {
        range: RangeInclusive<usize>,
        part: String,
        tree: SyntaxTree,
    },

    Arithmetic {
        range: RangeInclusive<usize>,
        expression: Word,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And(LeadingWhitespace),
    Or(LeadingWhitespace),
}

/// newline_list :              NEWLINE
///              | newline_list NEWLINE
///              ;
#[derive(Debug, PartialEq, Eq)]
pub struct NewlineList {
    /// This String may contain a mix of ' ', \t, and \n
    pub whitespace: String,
}

/// linebreak : newline_list
///           | /* empty */
///           ;
#[derive(Debug, PartialEq, Eq)]
pub struct Linebreak {
    pub newlines: Option<NewlineList>,
}

/// separator_op : '&'
///              | ';'
///              ;
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SeparatorOp {
    Sync(LeadingWhitespace),
    Async(LeadingWhitespace),
}

impl SeparatorOp {
    pub fn is_sync(&self) -> bool {
        matches!(self, Self::Sync(_))
    }

    pub fn is_async(&self) -> bool {
        !self.is_sync()
    }
}

impl Default for SeparatorOp {
    fn default() -> Self {
        Self::Sync(Default::default())
    }
}

/// separator : separator_op linebreak
///           | newline_list
///           ;
#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    Explicit(SeparatorOp, Linebreak),
    Implicit(NewlineList),
}

/// sequential_sep : ';' linebreak
///                | newline_list
///                ;
#[derive(Debug, PartialEq, Eq)]
pub enum SequentialSeparator {
    Semi(Linebreak),
    Implicit(NewlineList),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bang {
    pub whitespace: LeadingWhitespace,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comment {
    pub whitespace: LeadingWhitespace,
    pub content: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pipe {
    pub whitespace: LeadingWhitespace,
}
