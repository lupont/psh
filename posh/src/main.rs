mod args;
mod config;
mod repl;

use clap::Parser;
use posh_core::{lex, parse, Engine};

fn main() {
    let args = args::Args::parse();

    if args.lex {
        let tokens = lex(args.command.unwrap(), args.include_space);

        for token in tokens {
            println!("{:?}", token);
        }

        std::process::exit(0);
    } else if args.ast {
        let ast = parse(args.command.unwrap());

        println!("{:#?}", ast);

        std::process::exit(0);
    } else if let Some(cmd) = args.command {
        let code = match Engine::default().execute_line(cmd) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(|e| e.code).unwrap(),

            Err(e) => {
                eprintln!("posh: Could not execute command: {e}");
                1
            }
        };

        std::process::exit(code);
    }

    let mut repl = repl::Repl::new();

    if let Err(e) = repl.run() {
        eprintln!("posh: Unrecoverable error occurred: {e}");
    }
}
