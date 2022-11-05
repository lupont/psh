mod config;
mod engine;
mod error;
mod path;
mod repl;

pub use crate::engine::{Engine, ExitStatus};
pub use crate::error::{Error, Result};

fn main() {
    let mut repl = repl::Repl::new();

    if let Err(e) = repl.run() {
        eprintln!("rush: Unrecoverable error occurred: {e}");
    }
}
