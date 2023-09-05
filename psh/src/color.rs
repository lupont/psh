use crossterm::style::Color;
use psh_core::Engine;

macro_rules! color {
    ($func:ident, $var:expr) => {
        pub fn $func(engine: &Engine) -> Color {
            Color::AnsiValue(
                engine
                    .get_value_of($var)
                    .and_then(|c| c.parse::<u8>().ok())
                    .unwrap_or(15),
            )
        }
    };
}

color!(cmd_sub, "PSH_CMD_SUB_COL");
color!(comment, "PSH_COMMENT_COL");
color!(invalid_cmd, "PSH_INVALID_CMD_COL");
color!(lhs, "PSH_LHS_COL");
color!(normal, "PSH_NORMAL_COL");
color!(op, "PSH_OP_COL");
color!(prompt, "PSH_PROMPT_COL");
color!(rhs, "PSH_RHS_COL");
color!(separator, "PSH_SEPARATOR_COL");
color!(unparsed, "PSH_UNPARSED_COL");
color!(valid_cmd, "PSH_VALID_CMD_COL");
