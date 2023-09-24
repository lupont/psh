use crate::{Engine, ExitStatus, Result};

const HELP: &str = "\
usage: . <file>

Execute the file in the current execution context. If the file
does not contain a '/' character, $PATH is searched for it.";

pub fn execute(engine: &mut Engine, args: &[&str]) -> Result<ExitStatus> {
    match args {
        [] => Ok(ExitStatus::from_code(2)),

        args if args.contains(&"-h") || args.contains(&"--help") => {
            println!("{}", HELP);
            Ok(ExitStatus::from_code(0))
        }

        [file, ..] => {
            if file.contains('/') {
                engine.execute_file(file.into())?;
                Ok(ExitStatus::from_code(0))
            } else if let Some(file) = engine.get_file_in_path(file, false) {
                engine.execute_file(file.into())?;
                Ok(ExitStatus::from_code(0))
            } else {
                println!(".: '{file}': no such file");
                Ok(ExitStatus::from_code(1))
            }
        }
    }
}
