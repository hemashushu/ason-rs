// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::{self, Display};

#[derive(Debug)]
pub enum Error {
    Message(String),
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg) => {
                // write!(f, "Ason Error: {}", msg)
                formatter.write_str(msg)
            }
        }
    }
}

impl std::error::Error for Error {}
