// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub mod error;
pub mod process;
pub mod serde;

pub use process::parser::from_str;
pub use process::writer::to_string;

// pub use de::{from_str, Deserializer};
// pub use error::{Error, Result};
// pub use ser::{to_string, Serializer};