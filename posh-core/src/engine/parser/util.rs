pub fn is_valid_first_character_of_expansion(c: char) -> bool {
    // TODO: figure out if this is actually "correct" (POSIX)
    c.is_alphanumeric()
}
