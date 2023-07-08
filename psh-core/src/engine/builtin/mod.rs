mod cd;
mod colon;
mod exit;

pub use cd::cd;
pub use colon::colon;
pub use exit::exit;

use crate::ast::prelude::Word;

use super::expand::remove_quotes;

pub fn has(s: &Word) -> bool {
    let name = remove_quotes(&s.name);
    let has = |s| name == s || name.starts_with(&format!("{s} "));
    has("cd") || has("exit") || has(":")
}
