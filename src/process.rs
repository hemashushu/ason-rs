// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub mod lexer;
pub mod parser;
pub mod peekable_iterator;
pub mod writer;

use chrono::{DateTime, FixedOffset};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberLiteral {
    Byte(i8),
    UByte(u8),
    Short(i16),
    UShort(u16),
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
    Float(f32),
    Double(f64),
}

#[derive(Debug, PartialEq)]
pub struct NameValuePair {
    pub name: String,
    pub value: Box<AsonNode>,
}

#[derive(Debug, PartialEq)]
pub struct VariantItem {
    pub name: String,
    pub value: Option<Box<AsonNode>>,
}

#[derive(Debug, PartialEq)]
pub enum AsonNode {
    Number(NumberLiteral),
    Boolean(bool),
    Char(char),
    String_(String),
    Date(DateTime<FixedOffset>),
    Variant(VariantItem),
    ByteData(Vec<u8>),
    Array(Vec<AsonNode>),
    Tuple(Vec<AsonNode>),
    Object(Vec<NameValuePair>),
}
