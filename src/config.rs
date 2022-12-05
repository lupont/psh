use crossterm::style::Color;

pub struct Colors;

impl Colors {
    pub const VALID_ABBR: Color = Color::Yellow;
    pub const VALID_CMD: Color = Color::Yellow;
    pub const VALID_BUILTIN: Color = Color::DarkYellow;
    pub const INVALID_CMD: Color = Color::Red;

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
