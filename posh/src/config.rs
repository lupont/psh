use crossterm::style::Color;

pub struct Colors;

impl Colors {
    pub const UNPARSED: Color = Color::Red;

    pub const COMMENT_COLOR: Color = Color::DarkGrey;

    pub const BANG_COLOR: Color = Color::Magenta;
    pub const PIPE_COLOR: Color = Color::Magenta;

    pub const VALID_CMD_COLOR: Color = Color::Yellow;
    pub const INVALID_CMD_COLOR: Color = Color::Red;

    pub const TRAILING_WORD_COLOR: Color = Color::Grey;

    pub const REDIRECTION_FD_COLOR: Color = Color::White;
    pub const REDIRECTION_OP_COLOR: Color = Color::Yellow;
    pub const REDIRECTION_TARGET_COLOR: Color = Color::White;

    pub const ASSIGNMENT_LHS_COLOR: Color = Color::White;
    pub const ASSIGNMENT_OP_COLOR: Color = Color::Blue;
    pub const ASSIGNMENT_RHS_COLOR: Color = Color::White;

    pub const SEPARATOR_COLOR: Color = Color::DarkBlue;
    pub const LOGICAL_OP_COLOR: Color = Color::DarkBlue;

    // pub const VALID_ABBR: Color = Color::Yellow;
    // pub const VALID_CMD: Color = Color::Yellow;
    // pub const VALID_BUILTIN: Color = Color::DarkYellow;
    // pub const INVALID_CMD: Color = Color::Red;

    // pub const STRING: Color = Color::Reset;
    // pub const SINGLE_QUOTED_STRING: Color = Color::Blue;
    // pub const DOUBLE_QUOTED_STRING: Color = Color::DarkGreen;
    // pub const ASSIGNMENT: Color = Color::DarkBlue;
    // pub const FLAG: Color = Color::Grey;
    // pub const PIPE: Color = Color::DarkMagenta;
    // pub const SEMICOLON: Color = Color::Magenta;
    // pub const REDIRECT_OUTPUT: Color = Color::Grey;
    pub const REDIRECT_INPUT: Color = Color::Grey;
    // pub const NYI: Color = Color::Red;
    // pub const INCOMPLETE: Color = Color::Red;

    pub const NON_ZERO_RC: Color = Color::Red;
    pub const PROMPT: Color = Color::Yellow;
    pub const CWD: Color = Color::DarkMagenta;
}

pub const PROMPT: &str = "$";

pub const ABBREVIATIONS: [(&str, &str); 3] = [
    ("gs", "git status"),
    ("pacs", "sudo pacman -S"),
    ("e", "nvim"),
];
