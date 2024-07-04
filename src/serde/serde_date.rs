// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

//! Since the current date type is not compatible with serde, ASON's date
//! value (d"YYYY-MM-DDTHH:mm:ssZ") cannot be directly deserialized using
//! serde. Additionally, serializing a `chrono::DateTime` value will only
//! result in a plain string representation.
//!
//! To address this, we can wrap the date value within an `ason::Date` variant.
//! When serializing this variant, the output will be a string like
//! "Date::Rfc3339("2024-06-26T16:38:50+08:00")".
//!
//! While the date value remains serialized as a plain string, encasing it within
//! a variant allows for proper deserialization into `chrono::DateTime` rather
//! than String.

use chrono::{DateTime, FixedOffset, ParseResult};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Date {
    Rfc3339(DateTime<FixedOffset>),
}

impl Date {
    pub fn new(d: DateTime<FixedOffset>) -> Self {
        Self::Rfc3339(d)
    }

    pub fn from_rfc3339(s: &str) -> ParseResult<Self> {
        DateTime::parse_from_rfc3339(s).map(Self::Rfc3339)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::serde::{de::from_str, ser::to_string, serde_date::Date};

    #[test]
    fn test_serialize() {
        assert_eq!(
            to_string(&Date::from_rfc3339("2024-06-26T16:38:50+08:00").unwrap()).unwrap(),
            r#"Date::Rfc3339("2024-06-26T16:38:50+08:00")"#
        );

        assert_eq!(
            to_string(&Date::from_rfc3339("2024-06-26T16:38:50Z").unwrap()).unwrap(),
            r#"Date::Rfc3339("2024-06-26T16:38:50Z")"#
        );
    }

    #[test]
    fn test_deserialize() {
        assert_eq!(
            from_str::<Date>(r#"Date::Rfc3339("2024-06-26T16:38:50+08:00")"#).unwrap(),
            Date::from_rfc3339("2024-06-26T16:38:50+08:00").unwrap()
        );
        assert_eq!(
            from_str::<Date>(r#"Date::Rfc3339("2024-06-26T16:38:50Z")"#).unwrap(),
            Date::from_rfc3339("2024-06-26T16:38:50Z").unwrap()
        );
    }
}
