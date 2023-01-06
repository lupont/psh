pub mod ast;
pub mod lexer;
mod util;

pub use ast::{parse, SyntaxTree};
pub use lexer::{lex, Token};
pub use util::has_relative_command;
