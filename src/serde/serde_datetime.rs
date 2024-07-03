// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use chrono::{DateTime, FixedOffset};

// currently the type `DateTime` is not a date type of `serde`
// (per: https://serde.rs/data-model.html)
// the `chrono::DateTime` will be serialized as a RFC3339 ordinary string, e.g.
// "2024-06-26T16:38:50+08:00"
// instead of the ASON date time format:
// d"2024-06-26T16:38:50+08:00"

// DEPRECATED
// // the workaround is wrap the Datetime into the enum `Date`, e.g.
// //
// // let date = from_str::<Date>(r#"Date::Rfc3339("2024-06-26T16:38:50+08:00")"#).unwrap();
// // let s = to_string(&Date::new(DateTime::parse_from_rfc3339("2024-06-26T16:38:50+08:00").unwrap()));

pub type SerdeDate = DateTime<FixedOffset>;

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::serde::{de::from_str, ser::to_string, serde_datetime::SerdeDate};

    #[test]
    fn test_serialize() {
        assert_eq!(
            to_string(&SerdeDate::parse_from_rfc3339("2024-06-26T16:38:50+08:00").unwrap())
                .unwrap(),
            r#""2024-06-26T16:38:50+08:00""#
        );

        assert_eq!(
            to_string(&SerdeDate::parse_from_rfc3339("2024-06-26T16:38:50Z").unwrap()).unwrap(),
            r#""2024-06-26T16:38:50Z""#
        );
    }

    #[test]
    fn test_deserialize() {
        assert_eq!(
            from_str::<SerdeDate>(r#""2024-06-26T16:38:50+08:00""#).unwrap(),
            SerdeDate::parse_from_rfc3339("2024-06-26T16:38:50+08:00").unwrap()
        );
        assert_eq!(
            from_str::<SerdeDate>(r#""2024-06-26T16:38:50Z""#).unwrap(),
            SerdeDate::parse_from_rfc3339("2024-06-26T16:38:50Z").unwrap()
        );
    }
}
