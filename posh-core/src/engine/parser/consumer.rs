use std::iter::Peekable;

pub trait Consumer<T>: Iterator<Item = T>
where
    T: PartialEq + Clone,
{
    fn consume_if(&mut self, predicate: impl Fn(&T) -> bool) -> Option<T>;
    fn consume_single(&mut self, needle: T) -> Option<T>;
    fn consume_until(&mut self, predicate: impl Fn(&T) -> bool) -> Option<Vec<T>>;
    fn consume_multiple(&mut self, literal: impl IntoIterator<Item = T>) -> Option<Vec<T>>;
}

impl<T, I> Consumer<T> for Peekable<I>
where
    I: Iterator<Item = T> + Clone,
    T: PartialEq + Clone,
{
    fn consume_if(&mut self, predicate: impl Fn(&T) -> bool) -> Option<T> {
        match self.peek() {
            Some(t) if predicate(t) => Some(self.next().unwrap()),
            _ => None,
        }
    }

    fn consume_single(&mut self, needle: T) -> Option<T> {
        self.consume_if(|c| c == &needle)
    }

    fn consume_until(&mut self, predicate: impl Fn(&T) -> bool) -> Option<Vec<T>> {
        let mut v = Vec::new();

        while let Some(c) = self.peek() {
            if predicate(c) {
                break;
            }

            v.push(self.next().unwrap());
        }

        if v.is_empty() {
            None
        } else {
            Some(v)
        }
    }

    fn consume_multiple(&mut self, literal: impl IntoIterator<Item = T>) -> Option<Vec<T>> {
        let initial = self.clone();

        let mut v = Vec::new();

        let mut iter = literal.into_iter();
        for c in iter.by_ref() {
            match self.consume_single(c) {
                Some(i) => v.push(i),
                None => {
                    *self = initial;
                    return None;
                }
            }
        }

        Some(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consume_if() {
        let mut input = vec![1, 2, 3].into_iter().peekable();

        assert_eq!(Some(1), input.consume_if(|&d| d < 3));
        assert_eq!(Some(2), input.consume_if(|&d| d < 3));
        assert_eq!(None, input.consume_if(|&d| d < 3));
    }

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
            .consume_until(|c| c.is_whitespace())
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
