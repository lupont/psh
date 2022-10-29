mod config;
mod error;
mod input;
mod path;
mod repl;

pub(crate) use crate::error::{Error, Result};
pub(crate) use crate::repl::Repl;

fn main() {
    let mut repl = Repl::init();

    if let Err(e) = repl.run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
