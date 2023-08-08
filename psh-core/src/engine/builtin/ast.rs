
use crate::{Engine, ExitStatus, Result};

#[cfg(feature = "serde")]
const HELP: &str = "\
usage: ast [ -h | --help ] [ -j | --json ] <input>

Print the AST of the supplied input string.

ast -h           print this text
ast cmd          print the AST as parsed from cmd
ast -j cmdl      print the AST as parsed from cmd, in JSON format";

#[cfg(not(feature = "serde"))]
const HELP: &str = "\
usage: ast [ -h | --help ] <input>

Print the AST of the supplied input string.

ast -h           print this text
ast cmd          print the AST as parsed from cmd";

pub fn execute(_: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        args if args.is_empty() || args.contains(&"-h") || args.contains(&"--help") => {
            println!("{}", HELP);
            Ok(ExitStatus::from_code(0))
        }

        #[cfg(feature = "serde")]
        [cmd, "-j" | "--json"] | ["-j" | "--json", cmd] => {
            let ast = crate::ast::parse(cmd, true)?;
            println!("{}", ast.as_json()?);
            Ok(ExitStatus::from_code(0))
        }

        #[cfg(feature = "serde")]
        ["-j" | "--json"] => {
            println!("ast: missing command");
            Ok(ExitStatus::from_code(2))
        }

        [cmd] => {
            let ast = crate::ast::parse(cmd, true)?;
            println!("{ast:#?}");
            Ok(ExitStatus::from_code(0))
        }

        _ => {
            eprintln!("ast: Too many arguments");
            Ok(ExitStatus::from_code(1))
        }
    }
}
