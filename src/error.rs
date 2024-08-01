// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::{self, Display};

use crate::location::{Location, LocationWithRange};

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Message(String),
    MessageWithLocation(String, Location),
    MessageWithLocationRange(String, LocationWithRange),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg) => {
                write!(f, "ASON error: {}", msg)
                // f.write_str(msg)
            }
            Error::MessageWithLocation(msg, loc) => {
                write!(
                    f,
                    "ASON error: {}\nLine: {}, column: {}",
                    msg,
                    loc.line + 1,
                    loc.column + 1
                )
            }
            Error::MessageWithLocationRange(msg, loc) => {
                write!(
                    f,
                    "ASON error: {}\nLine: {}, columns: {}, chars: {}",
                    msg,
                    loc.line + 1,
                    loc.column + 1,
                    loc.length
                )
            }
        }
    }
}

impl std::error::Error for Error {}
