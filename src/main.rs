mod builtin;
mod config;
mod engine;
mod error;
mod input;
mod path;
mod repl;

pub use crate::engine::Engine;
pub use crate::error::{Error, Result};

fn main() {
    if let Err(e) = repl::run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
