mod args;
mod config;
mod engine;
mod error;
mod path;
mod repl;

use crate::engine::parser::{lex, parse};
pub use crate::engine::{Engine, ExitStatus};
pub use crate::error::{Error, Result};

use clap::Parser;

fn main() {
    let args = args::Args::parse();

    if args.lex {
        let tokens = lex(args.command.unwrap(), args.include_space);
        for token in tokens {
            println!("{:?}", token);
        }
        return;
    } else if args.ast {
        let ast = parse(args.command.unwrap());
        println!("{:#?}", ast);
        return;
    }

    if let Some(cmd) = args.command {
        let code = match Engine::default().execute_line(cmd) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(|e| e.code).unwrap(),

            Err(e) => {
                eprintln!("rush: Could not execute command: {e}");
                1
            }
        };

        std::process::exit(code);
    }

    let mut repl = repl::Repl::new();

    if let Err(e) = repl.run() {
        eprintln!("rush: Unrecoverable error occurred: {e}");
    }
}
