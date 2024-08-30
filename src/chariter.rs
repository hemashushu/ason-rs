// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::io::{BufRead, BufReader, Read};

use crate::error::Error;

struct CharIterFromReader {
    bufreader: Box<dyn BufRead>,
}

impl CharIterFromReader {
    fn new(reader: Box<dyn Read>) -> Self {
        Self {
            bufreader: Box::new(BufReader::new(reader)),
        }
    }
}

impl Iterator for CharIterFromReader {
    type Item = Result<u8, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0_u8; 1];
        match self.bufreader.read(&mut buf) {
            Ok(len) => {
                if len == 0 {
                    None
                } else {
                    Some(Ok(buf[0]))
                }
            }
            Err(e) => Some(Err(Error::Message(e.to_string()))),
        }
    }
}

pub struct CharIterFromOrdinary<'a> {
    upstream: &'a mut dyn Iterator<Item = char>,
}

impl<'a> CharIterFromOrdinary<'a> {
    pub fn new(upstream: &'a mut dyn Iterator<Item = char>) -> Self {
        Self { upstream }
    }
}

impl<'a> Iterator for CharIterFromOrdinary<'a> {
    type Item = Result<char, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.upstream.next() {
            Some(c) => Some(Ok(c)),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::CharIterFromOrdinary;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_char_iter_from_ordinary() {
        let mut chars = "abc".chars();
        let mut chariter = CharIterFromOrdinary::new(&mut chars);

        assert_eq!(chariter.next(), Some(Ok('a')));
        assert_eq!(chariter.next(), Some(Ok('b')));
        assert_eq!(chariter.next(), Some(Ok('c')));
        assert_eq!(chariter.next(), None);
    }
}
