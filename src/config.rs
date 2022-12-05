use crossterm::style::Color;

pub struct Colors;

impl Colors {
    pub const VALID_ABBR: Color = Color::Yellow;
    pub const VALID_CMD: Color = Color::Yellow;
    pub const VALID_BUILTIN: Color = Color::DarkYellow;
    pub const INVALID_CMD: Color = Color::Red;

    pub const STRING: Color = Color::Reset;
    pub const SINGLE_QUOTED_STRING: Color = Color::Blue;
    pub const DOUBLE_QUOTED_STRING: Color = Color::DarkGreen;
    pub const PIPE: Color = Color::DarkMagenta;
    pub const SEMICOLON: Color = Color::Magenta;
    pub const NYI: Color = Color::Red;

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
