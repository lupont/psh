use crossterm::style::Color;

pub(crate) struct Colors;

impl Colors {
    pub(crate) const VALID_CMD: Color = Color::Yellow;
    pub(crate) const VALID_BUILTIN: Color = Color::DarkYellow;
    pub(crate) const INVALID_CMD: Color = Color::Red;

    pub(crate) const NON_ZERO_RC: Color = Color::Red;
    pub(crate) const PROMPT: Color = Color::Yellow;
    pub(crate) const CWD: Color = Color::DarkMagenta;
}
