// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{error::Error, location::Position};

#[derive(Debug, PartialEq)]
pub struct CharWithPosition {
    pub character: char,
    pub position: Position,
}

impl CharWithPosition {
    pub fn new(character: char, position: Position) -> Self {
        Self {
            character,
            position,
        }
    }
}

pub struct CharsWithPositionIter<'a> {
    upstream: &'a mut dyn Iterator<Item = Result<char, Error>>,
    current_position: Position,
}

impl<'a> CharsWithPositionIter<'a> {
    pub fn new(unit: usize, upstream: &'a mut dyn Iterator<Item = Result<char, Error>>) -> Self {
        Self {
            upstream,
            current_position: Position::new(unit, 0, 0, 0),
        }
    }
}

impl<'a> Iterator for CharsWithPositionIter<'a> {
    type Item = Result<CharWithPosition, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.upstream.next() {
            Some(Ok(c)) => {
                let last_position = self.current_position; // Copy

                // increase positions
                self.current_position.index += 1;

                if c == '\n' {
                    self.current_position.line += 1;
                    self.current_position.column = 0;
                } else {
                    self.current_position.column += 1;
                }

                Some(Ok(CharWithPosition::new(c, last_position)))
            }
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        charstream::CharStreamFromCharIter,
        charposition::{CharWithPosition, CharsWithPositionIter},
        location::Position,
    };

    #[test]
    fn test_chars_with_position_iter() {
        {
            let mut chars = "a\nmn\nxyz".chars();
            let mut chariter = CharStreamFromCharIter::new(&mut chars);
            let mut positer = CharsWithPositionIter::new(0, &mut chariter);

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('a', Position::new(0, 0, 0, 0))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\n', Position::new(0, 1, 0, 1))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('m', Position::new(0, 2, 1, 0))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('n', Position::new(0, 3, 1, 1))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\n', Position::new(0, 4, 1, 2))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('x', Position::new(0, 5, 2, 0))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('y', Position::new(0, 6, 2, 1))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('z', Position::new(0, 7, 2, 2))))
            );

            assert!(positer.next().is_none());
        }

        {
            let mut chars = "\n\r\n\n".chars();
            let mut chariter = CharStreamFromCharIter::new(&mut chars);
            let mut positer = CharsWithPositionIter::new(1, &mut chariter);

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\n', Position::new(1, 0, 0, 0))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\r', Position::new(1, 1, 1, 0))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\n', Position::new(1, 2, 1, 1))))
            );

            assert_eq!(
                positer.next(),
                Some(Ok(CharWithPosition::new('\n', Position::new(1, 3, 2, 0))))
            );

            assert!(positer.next().is_none());
        }
    }
}
