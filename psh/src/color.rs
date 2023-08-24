use crossterm::style::Color;
use psh_core::Engine;

fn from_var(var: &str, engine: &Engine) -> Color {
    match engine.get_value_of(var) {
        Some(color) => match color.parse::<u8>() {
            Ok(val) => Color::AnsiValue(val),
            Err(_) => Color::AnsiValue(15),
        },
        None => Color::AnsiValue(15),
    }
}

pub fn unparsed(engine: &Engine) -> Color {
    from_var("PSH_UNPARSED_COL", engine)
}

pub fn comment(engine: &Engine) -> Color {
    from_var("PSH_COMMENT_COL", engine)
}

pub fn separator(engine: &Engine) -> Color {
    from_var("PSH_SEPARATOR_COL", engine)
}

pub fn valid_cmd(engine: &Engine) -> Color {
    from_var("PSH_VALID_CMD_COL", engine)
}

pub fn invalid_cmd(engine: &Engine) -> Color {
    from_var("PSH_INVALID_CMD_COL", engine)
}

pub fn cmd_sub(engine: &Engine) -> Color {
    from_var("PSH_CMD_SUB_COL", engine)
}

pub fn normal(engine: &Engine) -> Color {
    from_var("PSH_NORMAL_COL", engine)
}

pub fn op(engine: &Engine) -> Color {
    from_var("PSH_OP_COL", engine)
}

pub fn lhs(engine: &Engine) -> Color {
    from_var("PSH_LHS_COL", engine)
}

pub fn rhs(engine: &Engine) -> Color {
    from_var("PSH_RHS_COL", engine)
}

pub fn prompt(engine: &Engine) -> Color {
    from_var("PSH_PROMPT_COL", engine)
}

pub fn keyword(engine: &Engine) -> Color {
    from_var("PSH_KEYWORD", engine)
}
