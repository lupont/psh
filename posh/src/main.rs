mod args;
mod config;
mod repl;

use std::path::PathBuf;

use clap::Parser;

use posh_core::engine::parser::semtok;
use posh_core::engine::parser::tok;
use posh_core::parse;
use posh_core::Engine;
use posh_core::ExitStatus;

fn main() {
    let args = args::Args::parse();

    if let Some(command) = args.command {
        run_command(&command, args.tokenize, args.lex, args.ast);
    } else if let Some(file) = args.file {
        run_file(&file, args.tokenize, args.lex, args.ast);
    } else {
        let mut repl = repl::Repl::new();

        if let Err(e) = repl.run(args.tokenize, args.lex, args.ast) {
            eprintln!("posh: Unrecoverable error occurred: {e}");
            std::process::exit(7);
        }
    }
}

fn run_command(command: &String, tokenize: bool, lex: bool, ast: bool) {
    if tokenize {
        for token in tok::tokenize(command) {
            println!("{token:?}");
        }
    } else if lex {
        for token in semtok::lex(command) {
            println!("{token:?}");
        }
    } else if ast {
        let ast = parse(command, true);
        println!("{ast:#?}");
    } else {
        let code = match Engine::default().execute_line(command) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(ExitStatus::raw_code).unwrap(),

            Err(e) => {
                eprintln!("posh: Could not execute command: {e}");
                1
            }
        };
        std::process::exit(code);
    }
}

fn run_file(file: &String, tokenize: bool, lex: bool, ast: bool) {
    let path = PathBuf::from(file);
    if tokenize {
        let content = std::fs::read_to_string(path).unwrap();
        for token in tok::tokenize(content) {
            println!("{token:?}");
        }
    } else if lex {
        let content = std::fs::read_to_string(path).unwrap();
        for token in semtok::lex(content) {
            println!("{token:?}");
        }
    } else if ast {
        let content = std::fs::read_to_string(path).unwrap();
        let ast = parse(content, true);
        println!("{ast:#?}");
    } else {
        let code = match Engine::default().execute_file(path) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(ExitStatus::raw_code).unwrap(),

            Err(e) => {
                eprintln!("posh: Could not execute command: {e}");
                1
            }
        };
        std::process::exit(code);
    }
}
