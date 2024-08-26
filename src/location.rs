// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
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

impl Position {
    pub fn new(unit: usize, index: usize, line: usize, column: usize) -> Self {
        Self {
            unit,
            index,
            line,
            column,
        }
    }

    pub fn from_range_start(range: &Range) -> Self {
        Self {
            unit: range.unit,
            index: range.index,
            line: range.line,
            column: range.column,
        }
    }

    pub fn from_range_end(range: &Range) -> Self {
        let index = range.index + range.length;
        let column = range.column + range.length;
        Self {
            unit: range.unit,
            index,
            line: range.line,
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

    pub fn from_position_and_length(position: &Position, length: usize) -> Self {
        Self::new(
            position.unit,
            position.index,
            position.line,
            position.column,
            length,
        )
    }

    pub fn from_position_pair(position_start: &Position, position_end: &Position) -> Self {
        Self::new(
            position_start.unit,
            position_start.index,
            position_start.line,
            position_start.column,
            position_end.index - position_start.index,
        )
    }

    pub fn from_position_pair_include(
        position_start: &Position,
        position_end_included: &Position,
    ) -> Self {
        Self::new(
            position_start.unit,
            position_start.index,
            position_start.line,
            position_start.column,
            position_end_included.index - position_start.index + 1,
        )
    }

    pub fn combine_range_pair(range_start: &Range, range_end: &Range) -> Self {
        Self::new(
            range_start.unit,
            range_start.index,
            range_start.line,
            range_start.column,
            range_end.index - range_start.index + range_end.length,
        )
    }
}
