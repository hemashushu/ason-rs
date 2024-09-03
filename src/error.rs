// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::{self, Display};

use crate::location::{Position, Range};

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Message(String),
    UnexpectedEndOfDocument(String),

    // note that the "index" (and the result of "index+length") may exceed
    // the last index of string, for example, the "char incomplete" error raised by a string `'a`,
    // which index is 2.
    MessageWithPosition(String, Position),
    MessageWithRange(String, Range),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg) => f.write_str(msg),
            Error::UnexpectedEndOfDocument(msg) => {
                write!(f, "Unexpected to reach the end of document: {}", msg)
            }
            Error::MessageWithPosition(msg, position) => {
                write!(
                    f,
                    "{}\nLine: {}, column: {}",
                    msg,
                    position.line + 1,
                    position.column + 1
                )
            }
            Error::MessageWithRange(msg, range) => {
                write!(
                    f,
                    "{}\nLine: {}, column: {}",
                    msg,
                    range.line + 1,
                    range.column + 1,
                )
            }
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn with_source(&self, _source: &str) -> String {
        // print human readable error message with the source
        todo!()
    }
}
