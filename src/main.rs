mod builtin;
mod config;
mod engine;
mod error;
mod input;
mod path;
mod repl;

pub(crate) use crate::engine::Engine;
pub(crate) use crate::error::{Error, Result};

fn main() {
    if let Err(e) = repl::run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
