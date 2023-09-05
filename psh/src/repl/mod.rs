pub mod input;

use std::env;
use std::process;

use crossterm::terminal;

use psh_core::{ast, path, tok, Engine, Error, Result};

#[derive(Default)]
pub struct Repl {
    engine: Engine,
}

impl Repl {
    fn read_init_file(&mut self) -> Result<()> {
        match self.engine.execute_file(path::init_file()) {
            Ok(_) => Ok(()),
            Err(Error::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        }
    }

    fn update_shlvl(&self) {
        if let Ok(var) = env::var("SHLVL") {
            if let Ok(shlvl) = var.parse::<u8>() {
                let shlvl = shlvl + 1;
                env::set_var("SHLVL", shlvl.to_string());
            }
        } else {
            env::set_var("SHLVL", "1");
        }
    }

    pub fn run(&mut self, lex: bool, ast: bool, _json: bool) -> Result<()> {
        self.read_init_file()?;
        self.update_shlvl();

        if self.engine.get_value_of("PS1").is_none() {
            self.engine.assignments.insert(
                "PS1".to_string(),
                match is_root() {
                    true => input::PS1_ROOT_PROMPT,
                    false => input::PS1_USER_PROMPT,
                }
                .to_string(),
            );
        }
        if self.engine.get_value_of("PS2").is_none() {
            self.engine
                .assignments
                .insert("PS2".to_string(), input::PS2_PROMPT.to_string());
        }

        ctrlc::set_handler(|| {}).expect("psh: Error setting ^C handler");

        loop {
            let line = input::read_full_command(&mut self.engine)?;

            if lex && line != "exit" {
                for token in tok::lex(line) {
                    println!("{token:?}");
                }
            } else if ast && line != "exit" {
                let ast = ast::parse(line, true)?;

                #[cfg(feature = "serde")]
                if _json {
                    println!("{}", ast.as_json()?);
                } else {
                    println!("{ast:#?}");
                }

                #[cfg(not(feature = "serde"))]
                println!("{ast:#?}");
            } else {
                self.engine.history.append(&line)?;
                match self.engine.execute_line(line) {
                    Ok(statuses) if statuses.is_empty() => {}

                    Ok(statuses) => {
                        self.engine.last_status = statuses;
                    }

                    Err(e) => {
                        eprintln!("psh: {e}");
                    }
                }
            }
        }
    }
}

fn is_root() -> bool {
    let id = process::Command::new("id").arg("-u").output();
    matches!(id, Ok(id) if id.stdout == b"0\n")
}

pub struct RawMode;

impl RawMode {
    pub fn init() -> Result<Self> {
        terminal::enable_raw_mode()?;
        Ok(Self)
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        terminal::disable_raw_mode().expect("could not disable raw mode");
    }
}
