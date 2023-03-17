use std::iter::Peekable;
use std::str::Chars;

pub trait Consumer<T>: Iterator<Item = T>
where
    T: PartialEq,
{
    fn consume_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<T>;
    fn consume_single(&mut self, needle: T) -> Option<T>;
    fn consume_until(&mut self, predicate: impl Fn(T) -> bool) -> Option<Vec<T>>;
    fn consume_multiple(&mut self, literal: impl Iterator<Item = T>) -> Option<Vec<T>>;
}

impl Consumer<char> for Peekable<Chars<'_>> {
    fn consume_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        if let Some(&t) = self.peek() {
            if predicate(t) {
                self.next();
                return Some(t);
            }
        }

        None
    }

    fn consume_single(&mut self, needle: char) -> Option<char> {
        self.consume_if(|c| c == needle)
    }

    fn consume_until(&mut self, predicate: impl Fn(char) -> bool) -> Option<Vec<char>> {
        let mut v = Vec::new();

        while let Some(&c) = self.peek() {
            if predicate(c) {
                break;
            }

            self.next();
            v.push(c);
        }

        if v.is_empty() {
            None
        } else {
            Some(v)
        }
    }

    fn consume_multiple(&mut self, mut literal: impl Iterator<Item = char>) -> Option<Vec<char>> {
        let initial = self.clone();

        let mut v = Vec::new();

        for c in literal.by_ref() {
            if let Some(i) = self.consume_single(c) {
                v.push(i);
            } else {
                *self = initial;
                return None;
            }
        }

        Some(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consume_single() {
        let mut input = "hej hopp".chars().peekable();

        let consumed = input.consume_single('h');

        assert_eq!(Some('h'), consumed);
        assert_eq!(Some('e'), input.next());
    }

    #[test]
    fn test_consume_until() {
        let mut input = "hej hopp".chars().peekable();

        let consumed = input
            .consume_until(char::is_whitespace)
            .map(|v| v.iter().collect::<String>());

        assert_eq!(Some("hej".to_string()), consumed);
        assert_eq!(Some(' '), input.next());
    }

    #[test]
    fn test_consume_multiple() {
        let mut input = "hej hopp".chars().peekable();

        let consumed = input
            .consume_multiple("hej".chars())
            .map(|v| v.iter().collect::<String>());

        assert_eq!(Some("hej".to_string()), consumed);
        assert_eq!(Some(' '), input.next());

        let mut input = "hej hopp".chars().peekable();

        let consumed = input
            .consume_multiple("hopp".chars())
            .map(|v| v.iter().collect::<String>());

        assert_eq!(None, consumed);
        assert_eq!(Some('h'), input.next());

        let mut input = "hej hopp".chars().peekable();

        let consumed = input
            .consume_multiple("hep".chars())
            .map(|v| v.iter().collect::<String>());

        assert_eq!(None, consumed);
        assert_eq!(Some('h'), input.next());
    }
}
