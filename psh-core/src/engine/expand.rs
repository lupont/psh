use std::env;
use std::ffi::{CStr, CString};
use std::ops::RangeInclusive;

use nix::libc::getpwnam;

use crate::ast::nodes::*;
use crate::{path, Engine, Result};

pub trait Expand {
    fn expand(self, engine: &mut Engine) -> Vec<String>;
}

impl Expand for Word {
    fn expand(mut self, engine: &mut Engine) -> Vec<String> {
        let og = self.name.clone();

        expand_tilde(&mut self.name, &mut self.expansions);

        let field_split_candidates =
            expand_parameters(&mut self.name, &mut self.expansions, engine);

        // FIXME: command substitution
        // FIXME: arithmetic expression

        let remove_empty = !og.contains(['\'', '"']);

        let it = field_split(self.name, field_split_candidates, remove_empty, engine);

        // FIXME: pathname expand

        it.into_iter()
            .filter_map(|s| {
                let remove_empty = !s.contains(['\'', '"']);
                remove_quotes(&s, remove_empty)
            })
            .collect()
    }
}

fn field_split(
    input: String,
    ranges: Vec<RangeInclusive<usize>>,
    remove_empty: bool,
    engine: &Engine,
) -> Vec<String> {
    let ifs_chars = engine
        .get_value_of("IFS")
        .unwrap_or_else(|| String::from(" \n\t"));

    let mut fields = Vec::new();
    let mut current_field: Option<String> = None;

    let mut chars = input.chars().enumerate().peekable();

    while let Some((i, c)) = chars.next() {
        if ifs_chars.contains(c) && ranges.iter().any(|range| range.contains(&i)) {
            if let Some(field) = current_field {
                if !(remove_empty && field.is_empty()) {
                    fields.push(field);
                }
            }
            current_field = None;
        } else if let Some(field) = current_field.as_mut() {
            field.push(c);
            if chars.peek().is_none() {
                fields.push(field.to_string());
            }
        } else if chars.peek().is_none() {
            fields.push(c.to_string());
        } else {
            current_field = Some(c.to_string());
        }
    }

    fields
}

fn expand_tilde(input: &mut String, expansions: &mut Vec<Expansion>) {
    let mut indices = Vec::new();

    for (i, exp) in expansions.iter().enumerate() {
        if let Expansion::Tilde { .. } = exp {
            indices.push(i);
        }
    }

    while let Some(index) = indices.pop() {
        let Expansion::Tilde { range, name } = expansions.remove(index) else {
            unreachable!()
        };

        if !name.is_empty() && path::is_portable_filename(&name) {
            let c_str = CString::new(name).unwrap();
            let pointer = c_str.as_ptr();
            // SAFETY: we own the pointer which was created via CString::new
            //         from a known Rust string
            let passwd = unsafe { getpwnam(pointer) };

            if !passwd.is_null() {
                // SAFETY: the input is the return value of the `getpwnam`
                //         library function, and we know it is not null
                let dir = unsafe { CStr::from_ptr((*passwd).pw_dir) };
                let dir = dir.to_str().unwrap();
                input.replace_range(range, dir);
            }
        } else if name.is_empty() {
            input.replace_range(range, &path::home_dir());
        }
    }
}

fn expand_parameters(
    input: &mut String,
    expansions: &mut Vec<Expansion>,
    engine: &mut Engine,
) -> Vec<RangeInclusive<usize>> {
    let mut indices = Vec::new();
    for (i, exp) in expansions.iter().enumerate() {
        if let Expansion::Parameter { .. } = exp {
            indices.push(i);
        }
    }

    let mut field_split_candidates = Vec::new();

    while let Some(index) = indices.pop() {
        let Expansion::Parameter { range, name, finished: true, quoted } = expansions.remove(index) else {
            unreachable!()
        };

        if name == "?" {
            let status = engine
                .last_status
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("|");
            if !quoted {
                let start = *range.start();
                let len = status.len();
                let range = start..=start + len;
                field_split_candidates.push(range);
            }
            input.replace_range(range, &status);
        } else {
            let val = engine.get_value_of(&name).unwrap_or_default();
            if !quoted {
                let start = *range.start();
                let len = val.len();
                let range = start..=start + len;
                field_split_candidates.push(range);
            }
            input.replace_range(range, &val);
        }
    }

    field_split_candidates
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum QuoteState {
    Single,
    Double,
    None,
}

pub fn remove_quotes(s: &str, remove_empty: bool) -> Option<String> {
    let mut name = String::new();
    let mut state = QuoteState::None;
    let mut is_escaped = false;

    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        match (c, state, is_escaped) {
            ('\'', QuoteState::Single, _) => {
                state = QuoteState::None;
                is_escaped = false;
            }

            ('\'', QuoteState::None, false) => {
                state = QuoteState::Single;
                is_escaped = false;
            }

            ('"', QuoteState::Double, false) => {
                state = QuoteState::None;
                is_escaped = false;
            }

            ('"', QuoteState::None, false) => {
                state = QuoteState::Double;
                is_escaped = false;
            }

            ('\\', QuoteState::None | QuoteState::Double, false)
                if matches!(chars.peek(), Some('\n')) =>
            {
                chars.next();
                is_escaped = false;
            }

            ('\\', QuoteState::None, false) => {
                is_escaped = true;
            }

            ('\\', QuoteState::Double, false) if matches!(chars.peek(), Some('"')) => {
                is_escaped = true;
            }

            (c, _, _) => {
                name.push(c);
                is_escaped = false;
            }
        }
    }

    if name.is_empty() && remove_empty {
        None
    } else {
        Some(name)
    }
}

pub fn expand_prompt(mut word: Word, engine: &mut Engine) -> Result<String> {
    expand_parameters(&mut word.name, &mut word.expansions, engine);
    // FIXME: command substitution
    // FIXME: arithmetic expression
    // FIXME: ! expansion

    let input = word.name;
    let output = if input.contains("\\w") {
        let cwd = env::var("PWD")?;
        let compressed_cwd = path::compress_tilde(cwd);

        input.replace("\\w", &compressed_cwd)
    } else {
        input
    };

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backslash_removal() {
        let input = "hello\\ there";
        let output = remove_quotes(input, false);
        assert_eq!(Some("hello there".to_string()), output);

        let input = "'hello\\ there'";
        let output = remove_quotes(input, false);
        assert_eq!(Some("hello\\ there".to_string()), output);

        let input = "\"hello\\ there\"";
        let output = remove_quotes(input, false);
        assert_eq!(Some("hello\\ there".to_string()), output);

        let input = r#""'foo' \"bar\"""#;
        let output = remove_quotes(input, false);
        assert_eq!(Some(r#"'foo' "bar""#.to_string()), output);
    }
}
