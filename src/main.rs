mod error;
mod repl;

pub(crate) use crate::error::{Error, Result};
pub(crate) use crate::repl::Repl;

fn main() {
    let repl = Repl::init();

    if let Err(e) = repl.run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
