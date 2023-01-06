use std::os::unix::prelude::PermissionsExt;

pub fn is_valid_first_character_of_expansion(c: char) -> bool {
    // TODO: figure out if this is actually "correct" (POSIX)
    c.is_alphanumeric()
}

pub fn has_relative_command(cmd: impl AsRef<str>) -> bool {
    let cmd = cmd.as_ref();

    if !cmd.starts_with('/') && !cmd.starts_with('.') {
        return false;
    }

    match std::fs::metadata(cmd) {
        Ok(metadata) => {
            let mode = metadata.permissions().mode();
            !metadata.is_dir() && mode & 0o111 != 0
        }

        Err(_) => false,
    }
}
