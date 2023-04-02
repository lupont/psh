mod args;
mod config;
mod repl;

use std::path::PathBuf;

use clap::Parser;

use posh_core::engine::parser::semtok::lex;
use posh_core::engine::parser::tok::tokenize;
use posh_core::parse;
use posh_core::Engine;

fn main() {
    let args = args::Args::parse();

    if args.tokenize && args.command.is_some() {
        let tokens = tokenize(args.command.unwrap());

        for token in tokens {
            println!("{:?}", token);
        }

        std::process::exit(0);
    } else if args.lex && args.command.is_some() {
        let tokens = lex(args.command.unwrap());

        for token in tokens {
            println!("{:?}", token);
        }

        std::process::exit(0);
    } else if args.ast && args.command.is_some() {
        let ast = parse(args.command.unwrap(), true);

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
    } else if let Some(file) = args.file {
        let path = PathBuf::from(file);

        if args.tokenize {
            let tokens = tokenize(std::fs::read_to_string(path).unwrap());
            for token in tokens {
                println!("{:?}", token);
            }
            std::process::exit(0);
        } else if args.lex {
            let tokens = lex(std::fs::read_to_string(path).unwrap());
            for token in tokens {
                println!("{:?}", token);
            }
            std::process::exit(0);
        } else if args.ast {
            let ast = parse(std::fs::read_to_string(path).unwrap(), true);

            println!("{:#?}", ast);

            std::process::exit(0);
        }
        let code = match Engine::default().execute_file(path) {
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
