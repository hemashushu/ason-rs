// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use serde::{self, Deserialize, Deserializer, Serializer};

pub fn serialize<S>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    // return a string such as "aa:bb:cc:dd..."

    let s = bytes
        .iter()
        .map(|item| format!("{:02x}", item))
        .collect::<Vec<String>>()
        .join(":");
    serializer.serialize_str(&s)
}

pub fn deserialize<'de, D>(
    deserializer: D,
) -> Result<Vec<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    let _s = String::deserialize(deserializer)?;
    // TODO
    let bytes = vec![0u8];
    Ok(bytes)
}
