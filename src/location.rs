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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Range {
    pub unit: usize,   // the index of source file
    pub index: usize,  // character index
    pub line: usize,   // line index
    pub column: usize, // column index
    pub length: usize, // text length
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

impl Range {
    pub fn new(unit: usize, index: usize, line: usize, column: usize, length: usize) -> Self {
        Self {
            unit,
            index,
            line,
            column,
            length,
        }
    }

    pub fn from_location_and_length(location: &Location, length: usize) -> Self {
        Self::new(
            location.unit,
            location.index,
            location.line,
            location.column,
            length,
        )
    }

    pub fn from_location_pair(location_start: &Location, location_end: &Location) -> Self {
        Self::new(
            location_start.unit,
            location_start.index,
            location_start.line,
            location_start.column,
            location_end.index - location_start.index,
        )
    }

    pub fn from_location_pair_include(
        location_start: &Location,
        location_end_included: &Location,
    ) -> Self {
        Self::new(
            location_start.unit,
            location_start.index,
            location_start.line,
            location_start.column,
            location_end_included.index - location_start.index + 1,
        )
    }

    pub fn from_range_pair(range_start: &Range, range_end: &Range) -> Self {
        Self::new(
            range_start.unit,
            range_start.index,
            range_start.line,
            range_start.column,
            range_end.index - range_start.index + range_end.length,
        )
    }
}
