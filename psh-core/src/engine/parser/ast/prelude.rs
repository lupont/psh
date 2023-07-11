use serde::Serialize;

use std::ops::RangeInclusive;

use crate::engine::builtin;
use crate::engine::expand::Expand;
use crate::path;
use crate::Engine;

pub use super::parse;
pub use super::reconstruct;
pub use super::Parser;

/// Type alias used in data structures that keep track of whitespace.
pub type LeadingWhitespace = String;

/// ```[no_run]
/// program : linebreak complete_commands linebreak
///         | linebreak
///         ;
/// ```
#[derive(Serialize, Debug, Default, PartialEq, Eq)]
pub struct SyntaxTree {
    #[serde(rename = "leading_linebreaks")]
    pub leading: Linebreak,
    pub commands: Option<(CompleteCommands, Linebreak)>,
    pub unparsed: String,
}

impl SyntaxTree {
    pub fn is_ok(&self) -> bool {
        self.unparsed.chars().all(char::is_whitespace)
    }
}

/// ```[no_run]
/// complete_commands : complete_commands newline_list complete_command
///                   |                                complete_command
///                   ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct CompleteCommands {
    #[serde(rename = "complete_command")]
    pub head: CompleteCommand,
    pub tail: Vec<(NewlineList, CompleteCommand)>,
}

impl CompleteCommands {
    pub fn full(self) -> Vec<CompleteCommand> {
        let mut v = vec![self.head];
        for (_, cmd) in self.tail {
            v.push(cmd);
        }
        v
    }
}

/// ```[no_run]
/// complete_command : list separator_op
///                  | list
///                  ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
#[serde(tag = "type", content = "content")]
pub enum CompleteCommand {
    #[serde(rename = "list")]
    List {
        list: List,
        separator_op: Option<SeparatorOp>,
        comment: Option<Comment>,
    },

    #[serde(rename = "comment")]
    Comment { comment: Comment },
}

impl CompleteCommand {
    pub fn list_with_separator(self) -> Vec<(AndOrList, SeparatorOp)> {
        let mut items = Vec::new();

        let (list, separator_op) = match self {
            Self::List {
                list, separator_op, ..
            } => (list, separator_op),
            Self::Comment { .. } => return items,
        };

        let final_separator = match separator_op {
            Some(separator) => separator,
            None => Default::default(),
        };

        if list.tail.is_empty() {
            items.push((list.head, final_separator));
        } else {
            let mut prev_list = list.head;

            for (sep, and_or_list) in list.tail {
                items.push((prev_list, sep));
                prev_list = and_or_list;
            }

            items.push((prev_list, final_separator));
        }

        items
    }
}

/// ```[no_run]
/// list : list separator_op and_or
///      |                   and_or
///      ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct List {
    #[serde(rename = "and_or_list")]
    pub head: AndOrList,
    pub tail: Vec<(SeparatorOp, AndOrList)>,
}

/// ```[no_run]
/// and_or :                         pipeline
///        | and_or AND_IF linebreak pipeline
///        | and_or OR_IF  linebreak pipeline
///        ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct AndOrList {
    #[serde(rename = "pipeline")]
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

/// ```[no_run]
/// pipeline :      pipe_sequence
///          | Bang pipe_sequence
///          ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Pipeline {
    #[serde(flatten)]
    pub bang: Option<Bang>,

    #[serde(rename = "pipe_sequence")]
    pub sequence: PipeSequence,
}

impl Pipeline {
    /// Always at least one in length, since this joins self.first and self.rest.
    pub fn full(self) -> Vec<Command> {
        let mut v = vec![*self.sequence.head];
        for (_, _, cmd) in self.sequence.tail {
            v.push(cmd);
        }
        v
    }

    pub fn has_bang(&self) -> bool {
        self.bang.is_some()
    }

    pub fn noop() -> Self {
        Self {
            bang: None,
            sequence: PipeSequence::noop(),
        }
    }
}

/// ```[no_run]
/// pipe_sequence :                             command
///               | pipe_sequence '|' linebreak command
///               ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct PipeSequence {
    #[serde(rename = "command")]
    pub head: Box<Command>,

    pub tail: Vec<(Pipe, Linebreak, Command)>,
}

impl PipeSequence {
    pub fn noop() -> Self {
        Self {
            head: Box::new(Command::noop()),
            tail: Default::default(),
        }
    }
}

/// ```[no_run]
/// command : simple_command
///         | compound_command
///         | compound_command redirect_list
///         | function_definition
///         ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
#[serde(tag = "type", content = "content")]
pub enum Command {
    #[serde(rename = "simple")]
    Simple(SimpleCommand),

    #[serde(rename = "compound")]
    Compound(CompoundCommand, Vec<Redirection>),

    #[serde(rename = "function_definition")]
    FunctionDefinition(FunctionDefinition),
}

impl Command {
    pub fn noop() -> Self {
        Self::Simple(SimpleCommand::noop())
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            Command::Simple(cmd) => {
                matches!(&cmd.name, Some(word) if builtin::has(word))
            }
            _ => false,
        }
    }
}

/// ```[no_run]
/// compound_command : brace_group
///                  | subshell
///                  | for_clause
///                  | case_clause
///                  | if_clause
///                  | while_clause
///                  | until_clause
///                  ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
#[serde(tag = "type", content = "content")]
pub enum CompoundCommand {
    Brace(BraceGroup),
    Subshell(Subshell),
    For(ForClause),
    Case(CaseClause),
    If(IfClause),
    While(WhileClause),
    Until(UntilClause),
}

/// ```[no_run]
/// subshell : '(' compound_list ')'
///          ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Subshell {
    pub lparen_ws: LeadingWhitespace,
    pub body: CompoundList,
    pub rparen_ws: LeadingWhitespace,
}

/// ```[no_run]
/// compound_list : linebreak term
///               | linebreak term separator
///               ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct CompoundList {
    pub linebreak: Linebreak,
    pub term: Term,
    pub separator: Option<Separator>,
}

/// ```[no_run]
/// term : term separator and_or
///      |                and_or
///      ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Term {
    pub head: AndOrList,
    pub tail: Vec<(Separator, AndOrList)>,
}

/// ```[no_run]
/// for_clause : For name                                      do_group
///            | For name                       sequential_sep do_group
///            | For name linebreak in          sequential_sep do_group
///            | For name linebreak in wordlist sequential_sep do_group
///            ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum ForClause {
    Simple(Name, DoGroup),
    Padded(Name, SequentialSeparator, DoGroup),
    Full(Name, Linebreak, Vec<Word>, SequentialSeparator, DoGroup),
}

/// ```[no_run]
/// name : NAME /* Apply rule 5 */
///      ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Name {
    pub whitespace: LeadingWhitespace,
    pub name: String,
}

/// ```[no_run]
/// case_clause : Case WORD linebreak in linebreak case_list    Esac
///             | Case WORD linebreak in linebreak case_list_ns Esac
///             | Case WORD linebreak in linebreak              Esac
///             ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum CaseClause {
    Normal(Word, Linebreak, Linebreak, CaseList),
    NoSeparator(Word, Linebreak, Linebreak, CaseListNs),
    Empty(Word, Linebreak, Linebreak),
}

/// ```[no_run]
/// case_list_ns : case_list case_item_ns
///              |           case_item_ns
///              ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct CaseListNs {
    pub case_list: Option<CaseList>,
    pub last: CaseItemNs,
}

/// ```[no_run]
/// case_list : case_list case_item
///           |           case_item
///           ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct CaseList {
    pub head: CaseItem,
    pub tail: Vec<CaseItem>,
}

/// ```[no_run]
/// case_item_ns :     pattern ')' linebreak
///              |     pattern ')' compound_list
///              | '(' pattern ')' linebreak
///              | '(' pattern ')' compound_list
///              ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum CaseItemNs {
    Empty(bool, Pattern, Linebreak),
    List(bool, Pattern, CompoundList),
}

/// ```[no_run]
/// case_item :     pattern ')' linebreak     DSEMI linebreak
///           |     pattern ')' compound_list DSEMI linebreak
///           | '(' pattern ')' linebreak     DSEMI linebreak
///           | '(' pattern ')' compound_list DSEMI linebreak
///           ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum CaseItem {
    Empty(bool, Pattern, Linebreak, Linebreak),
    List(bool, Pattern, CompoundList, Linebreak),
}

/// ```[no_run]
/// pattern :             WORD /* Apply rule 4 */
///         | pattern '|' WORD /* Do not apply rule 4 */
///         ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Pattern {
    pub head: Word,
    pub tail: Vec<Word>,
}

/// ```[no_run]
/// if_clause : If compound_list Then compound_list else_part Fi
///           | If compound_list Then compound_list           Fi
///           ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct IfClause {
    pub predicate: CompoundList,
    pub body: CompoundList,
    pub else_part: Option<ElsePart>,
}

/// ```[no_run]
/// else_part : Elif compound_list Then compound_list
///           | Elif compound_list Then compound_list else_part
///           | Else compound_list
///           ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct ElsePart {
    pub elseifs: Vec<(CompoundList, CompoundList)>,
    pub else_part: Option<CompoundList>,
}

/// ```[no_run]
/// while_clause : While compound_list do_group
///              ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct WhileClause {
    pub predicate: CompoundList,
    pub body: DoGroup,
}

/// ```[no_run]
/// until_clause : Until compound_list do_group
///              ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct UntilClause {
    pub predicate: CompoundList,
    pub body: DoGroup,
}

/// ```[no_run]
/// function_definition : fname '(' ')' linebreak function_body
///                     ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Name,
    pub parens: String,
    pub linebreak: Linebreak,
    pub body: FunctionBody,
}

/// ```[no_run]
/// function_body : compound_command               /* Apply rule 9 */
///               | compound_command redirect_list /* Apply rule 9 */
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub command: CompoundCommand,
    pub redirections: Vec<Redirection>,
}

/// ```[no_run]
/// brace_group : Lbrace compound_list Rbrace
///             ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct BraceGroup {
    pub lbrace_ws: LeadingWhitespace,
    pub body: CompoundList,
    pub rbrace_ws: LeadingWhitespace,
}

/// ```[no_run]
/// do_group : Do compound_list Done /* Apply rule 6 */
///          ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct DoGroup {
    pub body: CompoundList,
}

/// ```[no_run]
/// simple_command : cmd_prefix cmd_word cmd_suffix
///                | cmd_prefix cmd_word
///                | cmd_prefix
///                | cmd_name cmd_suffix
///                | cmd_name
///                ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
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
    pub fn expand_name(self, engine: &mut Engine) -> Self {
        match self.name {
            Some(name) => SimpleCommand {
                prefixes: self.prefixes,
                name: Some(name.expand(engine)),
                suffixes: self.suffixes,
            },
            None => self,
        }
    }

    pub fn expand_prefixes(self, engine: &mut Engine) -> Self {
        let mut prefixes = Vec::new();

        for prefix in self.prefixes {
            prefixes.push(prefix.expand(engine));
        }

        Self {
            prefixes,
            name: self.name,
            suffixes: self.suffixes,
        }
    }

    pub fn expand_suffixes(self, engine: &mut Engine) -> Self {
        let mut suffixes = Vec::new();
        for suffix in self.suffixes {
            match suffix {
                CmdSuffix::Word(word) => {
                    let empty = word.name.is_empty();
                    if !empty {
                        let expanded = word.expand(engine);
                        suffixes.push(CmdSuffix::Word(expanded));
                    }
                }

                CmdSuffix::Redirection(r) => {
                    suffixes.push(CmdSuffix::Redirection(r.expand(engine)))
                }
            }
        }

        Self {
            prefixes: self.prefixes,
            name: self.name,
            suffixes,
        }
    }

    pub fn noop() -> Self {
        Self {
            name: None,
            prefixes: Default::default(),
            suffixes: Default::default(),
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

/// ```[no_run]
/// cmd_prefix :            io_redirect
///            | cmd_prefix io_redirect
///            |            ASSIGNMENT_WORD
///            | cmd_prefix ASSIGNMENT_WORD
///            ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum CmdPrefix {
    Redirection(Redirection),
    Assignment(VariableAssignment),
}

/// ```[no_run]
/// cmd_suffix :            io_redirect
///            | cmd_suffix io_redirect
///            |            WORD
///            | cmd_suffix WORD
///            ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum CmdSuffix {
    Redirection(Redirection),
    Word(Word),
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum FileDescriptor {
    Stdin,
    Stdout,
    Stderr,
    Other(i32),
}

/// `Input`:         `<`
/// `InputFd`:       `<&`
/// `ReadWrite`:     `<>`
/// `Output`:        `>`
/// `OutputFd`:      `>&`
/// `OutputAppend`:  `>>`
/// `OutputClobber`: `>|`
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum RedirectionType {
    /// `<`
    Input,

    /// `<&`
    InputFd,

    /// `<>`
    ReadWrite,

    /// `>`
    Output,

    /// `>&`
    OutputFd,

    /// `>>`
    OutputAppend,

    /// `>|`
    OutputClobber,
}

/// `Normal`:    `<<`
/// `StripTabs`: `<<-`
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum HereDocType {
    /// `<<`
    Normal,

    /// `<<-`
    StripTabs,
}

/// ```[no_run]
/// io_redirect :           io_file
///             | IO_NUMBER io_file
///             |           io_here
///             | IO_NUMBER io_here
///             ;
///
/// io_file : '<'       filename
///         | LESSAND   filename
///         | '>'       filename
///         | GREATAND  filename
///         | DGREAT    filename
///         | LESSGREAT filename
///         | CLOBBER   filename
///         ;
///
/// io_here : DLESS     here_end
///         | DLESSDASH here_end
///         ;
/// ```
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum Redirection {
    File {
        whitespace: LeadingWhitespace,
        input_fd: Option<FileDescriptor>,
        ty: RedirectionType,
        target: Word,
    },

    Here {
        whitespace: LeadingWhitespace,
        input_fd: Option<FileDescriptor>,

        ty: HereDocType,

        /// The delimiter
        end: Word,

        /// The entire content of the here document
        content: Word,
    },
}

impl Redirection {
    pub fn new_file(input_fd: Option<FileDescriptor>, ty: RedirectionType, target: Word) -> Self {
        Self::File {
            whitespace: Default::default(),
            input_fd,
            ty,
            target,
        }
    }

    pub fn new_input(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::Input, target)
    }

    pub fn new_input_fd(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::InputFd, target)
    }

    pub fn new_output(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::Output, target)
    }

    pub fn new_output_fd(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::OutputFd, target)
    }

    pub fn new_output_append(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::OutputAppend, target)
    }

    pub fn new_output_clobber(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::OutputClobber, target)
    }

    pub fn new_read_write(fd: Option<FileDescriptor>, target: Word) -> Self {
        Self::new_file(fd, RedirectionType::ReadWrite, target)
    }

    pub fn new_here(
        input_fd: Option<FileDescriptor>,
        strip_tabs: bool,
        content: Word,
        end: Word,
    ) -> Self {
        Self::Here {
            whitespace: Default::default(),
            input_fd,
            ty: if strip_tabs {
                HereDocType::StripTabs
            } else {
                HereDocType::Normal
            },
            content,
            end,
        }
    }
}

#[derive(Serialize, Debug, PartialEq, Eq)]
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

#[derive(Serialize, Debug, PartialEq, Clone, Copy)]
pub enum QuoteState {
    Single,
    Double,
    None,
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Word {
    pub whitespace: LeadingWhitespace,
    pub name: String,
    pub name_with_escaped_newlines: String,
    pub expansions: Vec<Expansion>,
}

impl Word {
    pub fn new(input: &str, whitespace: impl Into<LeadingWhitespace>) -> Self {
        let expansions = Self::find_expansions(input);
        let name = input.replace("\\\n", "");
        let name_with_escaped_newlines = input.to_string();

        Self {
            whitespace: whitespace.into(),
            name,
            name_with_escaped_newlines,
            expansions,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    pub fn is_finished(&self) -> bool {
        let single_quotes =
            Self::find_all(&[QuoteState::None, QuoteState::Single], &self.name, '\'').len();

        let double_quotes =
            Self::find_all(&[QuoteState::None, QuoteState::Double], &self.name, '"').len();

        let mut trailing_backslash = false;

        // FIXME: this does not take the current quote state into account,
        //        meaning if a word is e.g. 'foo\, this will mark it as
        //        a trailing backslash, when it should not
        let mut iter = self.name.chars().rev();
        while let Some('\\') = iter.next() {
            trailing_backslash ^= true;
        }

        single_quotes % 2 == 0 && double_quotes % 2 == 0 && !trailing_backslash
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

                ('?', QuoteState::None) if !is_escaped && curr_expansion.is_some() => {
                    if matches!(&curr_expansion, Some(s) if s.is_empty()) {
                        expansions.push(Expansion::Parameter {
                            range: curr_expansion_start..=index,
                            name: "?".to_string(),
                        });
                        curr_expansion = None;
                    }
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

    pub fn find_all(target_states: &[QuoteState], haystack: &str, needle: char) -> Vec<usize> {
        let mut state = QuoteState::None;
        let mut is_escaped = false;

        let mut found = Vec::new();

        for (i, c) in haystack.chars().enumerate() {
            if needle == c && target_states.contains(&state) && !is_escaped {
                found.push(i);
            }
            match (c, state, is_escaped) {
                ('\'', QuoteState::Single, _) => {
                    state = QuoteState::None;
                    is_escaped = false;
                }
                ('\'', QuoteState::None, false) => {
                    state = QuoteState::Single;
                    is_escaped = false;
                }
                (_, QuoteState::Single, _) => {
                    is_escaped = false;
                }

                ('"', QuoteState::Double, false) => {
                    state = QuoteState::None;
                    is_escaped = false;
                }

                ('"', QuoteState::None, false) => {
                    state = QuoteState::Double;
                    is_escaped = false;
                }

                ('\\', _, false) => {
                    is_escaped = true;
                }

                (_, _, _) => {
                    is_escaped = false;
                }
            }
        }

        found
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
            if needle == c && target_state == state && !is_escaped {
                found = Some(i);
            }
            match (c, state, is_escaped) {
                ('\'', QuoteState::Single, _) => {
                    state = QuoteState::None;
                    is_escaped = false;
                }
                ('\'', QuoteState::None, false) => {
                    state = QuoteState::Single;
                    is_escaped = false;
                }
                (_, QuoteState::Single, _) => {
                    is_escaped = false;
                }

                ('"', QuoteState::Double, false) => {
                    state = QuoteState::None;
                    is_escaped = false;
                }

                ('"', QuoteState::None, false) => {
                    state = QuoteState::Double;
                    is_escaped = false;
                }

                ('\\', _, false) => {
                    is_escaped = true;
                }

                (_, _, _) => {
                    is_escaped = false;
                }
            }
            if found.is_some() && first {
                break;
            }
        }

        found
    }
}

#[derive(Serialize, Debug, PartialEq, Eq)]
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

#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And(LeadingWhitespace),
    Or(LeadingWhitespace),
}

/// newline_list :              NEWLINE
///              | newline_list NEWLINE
///              ;
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct NewlineList {
    /// This String may contain a mix of ' ', \t, and \n
    pub whitespace: String,
}

/// linebreak : newline_list
///           | /* empty */
///           ;
#[derive(Serialize, Debug, Default, PartialEq, Eq)]
pub struct Linebreak {
    #[serde(flatten)]
    pub newlines: Option<NewlineList>,
}

/// separator_op : '&'
///              | ';'
///              ;
#[derive(Serialize, Debug, PartialEq, Eq, Clone)]
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
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum Separator {
    Explicit(SeparatorOp, Linebreak),
    Implicit(NewlineList),
}

/// sequential_sep : ';' linebreak
///                | newline_list
///                ;
#[derive(Serialize, Debug, PartialEq, Eq)]
pub enum SequentialSeparator {
    Semi(Linebreak),
    Implicit(NewlineList),
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Bang {
    pub whitespace: LeadingWhitespace,
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Comment {
    pub whitespace: LeadingWhitespace,
    pub content: String,
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Pipe {
    pub whitespace: LeadingWhitespace,
}
