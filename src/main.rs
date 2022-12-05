mod args;
mod config;
mod engine;
mod error;
mod path;
mod repl;

use crate::engine::parser::ast::parse;
pub use crate::engine::{Engine, ExitStatus};
pub use crate::error::{Error, Result};

use clap::Parser;

fn main() {
    let args = args::Args::parse();

    if args.lex {
        let tokens = engine::parser::lex(args.command.unwrap());
        for token in tokens {
            println!("{:?}", token);
        }
        return;
    }

    if args.ast {
        let ast = parse(args.command.unwrap());
        println!("{:#?}", ast);
        return;
    }

    if let Some(cmd) = args.command {
        match Engine::default().execute_line(cmd) {
            Ok(codes) => {
                std::process::exit(codes.last().map(|e| e.code).unwrap());
            }
            Err(e) => {
                eprintln!("rush: Could not execute command: {e}");
                std::process::exit(1);
            }
        }
    }

    let mut repl = repl::Repl::new();

    if let Err(e) = repl.run() {
        eprintln!("rush: Unrecoverable error occurred: {e}");
    }
}
