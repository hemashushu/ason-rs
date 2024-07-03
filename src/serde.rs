// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub mod de;
pub mod ser;
// pub mod serde_hex_bytes;

use std::fmt::Display;

use crate::error::Error;

// note about the data type 'byte data'
// TODO::

// note about the data type 'date'
//
// currently the DateTime is serialized as a RFC3339 ordinary string
// without the 'd' prefix, e.g.
// "2024-06-26T16:38:50+08:00"
// instead of
// d"2024-06-26T16:38:50+08:00"
//
// the workaround is wrap the Datetime into an enum, e.g.
// enum Date {
//    Rfc3339(DateTime<FixedOffset>)
// }
//
// let date1 = Date::Rfc3339(DateTime::parse_from_rfc3339("2024-06-26T16:38:50+08:00").unwrap());
// let date2 = Date::Rfc3339(DateTime::parse_from_rfc3339("2024-06-26T16:38:50Z").unwrap());

// #[derive(Deserialize, Serialize, Debug, Clone)]
// pub enum Date {
//     Rfc3339(DateTime<FixedOffset>),
// }
//
// #[derive(Deserialize, Serialize, Debug, Clone)]
// pub enum Binary {
//     #[serde(with = "serde_hex_bytes")]
//     Hex(Vec<u8>),
// }

pub type Result<T> = std::result::Result<T, Error>;

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Error::Message(msg.to_string())
    }
}

impl serde::de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Error::Message(msg.to_string())
    }
}
