// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::{self, Display};

use crate::location::Location;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Message(String),
    UnexpectedEndOfDocument(String),

    // note that the "index" (and the result of "index+length") may exceed
    // the last index of string, for example, the "char incomplete" error raised by a string `'a`,
    // which index is 2.
    MessageWithLocation(String, Location),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg) => f.write_str(msg),
            Error::UnexpectedEndOfDocument(detail) => {
                writeln!(f, "Unexpected to reach the end of document.")?;
                write!(f, "{}", detail)
            }
            Error::MessageWithLocation(detail, location) => {
                writeln!(
                    f,
                    "Error at line: {}, column: {}",
                    location.line + 1,
                    location.column + 1
                )?;
                write!(f, "{}", detail)
            }
        }
    }
}

impl std::error::Error for Error {}
