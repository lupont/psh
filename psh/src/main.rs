mod args;
mod config;
mod repl;

use std::path::PathBuf;

use clap::Parser;

use psh_core::ast::parse;
use psh_core::parser::tok;
use psh_core::Engine;
use psh_core::ExitStatus;

fn main() {
    let args = args::Args::parse();

    #[cfg(feature = "serde")]
    let json = args.json;

    #[cfg(not(feature = "serde"))]
    let json = false;

    if let Some(target) = args.target {
        if args.command {
            run_command(&target, args.lex, args.ast, json);
        } else {
            run_file(&target, args.lex, args.ast, json);
        }
    } else {
        let mut repl = repl::Repl::new();

        if let Err(e) = repl.run(args.lex, args.ast, json) {
            eprintln!("psh: Unrecoverable error occurred: {e}");
            std::process::exit(7);
        }
    }
}

fn run_command(command: &str, lex: bool, ast: bool, _json: bool) {
    if lex {
        for token in tok::lex(command) {
            println!("{token:?}");
        }
    } else if ast {
        let ast = parse(command, true);

        #[cfg(feature = "serde")]
        if _json {
            println!("{}", ast.unwrap().as_json().unwrap());
        } else {
            println!("{:#?}", ast);
        }

        #[cfg(not(feature = "serde"))]
        println!("{:#?}", ast);
    } else {
        let code = match Engine::default().execute_line(command) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(ExitStatus::raw_code).unwrap(),

            Err(e) => {
                eprintln!("psh: Could not execute command: {e}");
                1
            }
        };
        std::process::exit(code);
    }
}

fn run_file(file: &String, lex: bool, ast: bool, _json: bool) {
    let path = PathBuf::from(file);
    if lex {
        let content = std::fs::read_to_string(path).unwrap();
        for token in tok::lex(content) {
            println!("{token:?}");
        }
    } else if ast {
        let content = std::fs::read_to_string(path).unwrap();
        let ast = parse(content, true);

        #[cfg(feature = "serde")]
        if _json {
            println!("{}", ast.unwrap().as_json().unwrap());
        } else {
            println!("{:#?}", ast);
        }

        #[cfg(not(feature = "serde"))]
        println!("{:#?}", ast);
    } else {
        let code = match Engine::default().execute_file(path) {
            Ok(codes) if codes.is_empty() => 0,

            Ok(codes) => codes.last().map(ExitStatus::raw_code).unwrap(),

            Err(e) => {
                eprintln!("psh: Could not execute command: {e}");
                1
            }
        };
        std::process::exit(code);
    }
}
