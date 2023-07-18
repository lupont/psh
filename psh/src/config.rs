use crossterm::style::Color;

pub struct Colors;

impl Colors {
    pub const UNPARSED: Color = Color::Red;

    pub const COMMENT_COLOR: Color = Color::DarkGrey;

    pub const BANG_COLOR: Color = Color::Magenta;
    pub const PIPE_COLOR: Color = Color::Magenta;

    pub const VALID_CMD_COLOR: Color = Color::Yellow;
    pub const INVALID_CMD_COLOR: Color = Color::DarkRed;

    pub const TRAILING_WORD_COLOR: Color = Color::Grey;

    pub const REDIRECTION_FD_COLOR: Color = Color::White;
    pub const REDIRECTION_OP_COLOR: Color = Color::Yellow;
    pub const REDIRECTION_TARGET_COLOR: Color = Color::White;

    pub const ASSIGNMENT_LHS_COLOR: Color = Color::White;
    pub const ASSIGNMENT_OP_COLOR: Color = Color::Blue;
    pub const ASSIGNMENT_RHS_COLOR: Color = Color::White;

    pub const SEPARATOR_COLOR: Color = Color::Magenta;
    pub const LOGICAL_OP_COLOR: Color = Color::Magenta;

    pub const FUNC_DEF_PAREN_COLOR: Color = Color::Grey;
    pub const BRACE_GROUP_COLOR: Color = Color::DarkGreen;

    pub const PROMPT: Color = Color::DarkGrey;
}

pub const PS1_USER_PROMPT: &str = "$ ";
pub const PS1_ROOT_PROMPT: &str = "# ";
pub const PS2_PROMPT: &str = "> ";
