// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub unit: usize,   // the index of source file
    pub index: usize,  // character index
    pub line: usize,   // line index
    pub column: usize, // column index
}

impl Location {
    pub fn new(unit: usize, index: usize, line: usize, column: usize) -> Self {
        Self {
            unit,
            index,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LocationWithRange {
    pub unit: usize,
    pub index: usize,
    pub line: usize,
    pub column: usize,

    pub length: usize,
}

impl LocationWithRange {
    pub fn new(unit: usize, index: usize, line: usize, column: usize, length: usize) -> Self {
        Self {
            unit,
            index,
            line,
            column,
            length,
        }
    }

    pub fn from_location(location: &Location, length: usize) -> Self {
        Self::new(
            location.unit,
            location.index,
            location.line,
            location.column,
            length,
        )
    }

    pub fn from_location_pair(start_location: &Location, end_location: &Location) -> Self {
        Self::new(
            start_location.unit,
            start_location.index,
            start_location.line,
            start_location.column,
            end_location.index - start_location.index,
        )
    }

    pub fn from_location_pair_include(start_location: &Location, end_location_include: &Location) -> Self {
        Self::new(
            start_location.unit,
            start_location.index,
            start_location.line,
            start_location.column,
            end_location_include.index - start_location.index + 1,
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CharWithLocation {
    pub character: char,
    pub location: Location,
}

impl CharWithLocation {
    pub fn new(character: char, location: Location) -> Self {
        Self {
            character,
            location,
        }
    }
}

pub struct CharsWithLocationIter<'a> {
    upstream: &'a mut dyn Iterator<Item = char>,
    current_location: Location,
}

impl<'a> CharsWithLocationIter<'a> {
    pub fn new(unit: usize, upstream: &'a mut dyn Iterator<Item = char>) -> Self {
        Self {
            upstream,
            current_location: Location::new(unit, 0, 0, 0),
        }
    }
}

impl<'a> Iterator for CharsWithLocationIter<'a> {
    type Item = CharWithLocation;

    fn next(&mut self) -> Option<Self::Item> {
        match self.upstream.next() {
            Some(c) => {
                let last_location = self.current_location; // Copy

                // increase positions
                self.current_location.index += 1;

                if c == '\n' {
                    self.current_location.line += 1;
                    self.current_location.column = 0;
                } else {
                    self.current_location.column += 1;
                }

                Some(CharWithLocation::new(c, last_location))
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::location::{CharWithLocation, CharsWithLocationIter, Location};

    #[test]
    fn test_chars_with_location_iter1() {
        let mut chars = "a\nmn\nxyz".chars();
        let mut iter = CharsWithLocationIter::new(0, &mut chars);

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('a', Location::new(0, 0, 0, 0)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\n', Location::new(0, 1, 0, 1)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('m', Location::new(0, 2, 1, 0)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('n', Location::new(0, 3, 1, 1)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\n', Location::new(0, 4, 1, 2)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('x', Location::new(0, 5, 2, 0)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('y', Location::new(0, 6, 2, 1)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('z', Location::new(0, 7, 2, 2)))
        );

        assert!(iter.next().is_none());
    }

    #[test]
    fn test_chars_with_location_iter2() {
        let mut chars = "\n\r\n\n".chars();
        let mut iter = CharsWithLocationIter::new(1, &mut chars);

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\n', Location::new(1, 0, 0, 0)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\r', Location::new(1, 1, 1, 0)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\n', Location::new(1, 2, 1, 1)))
        );

        assert_eq!(
            iter.next(),
            Some(CharWithLocation::new('\n', Location::new(1, 3, 2, 0)))
        );

        assert!(iter.next().is_none());
    }
}
