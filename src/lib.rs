// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

mod formatter;
mod lexer;
mod parser;
mod peekable_iterator;

use std::fmt::Display;

use chrono::{DateTime, FixedOffset};
use formatter::format;
use lexer::lex;
use parser::parse;
use peekable_iterator::PeekableIterator;

// decimal numbers:
// 123
// 123.456
// -123
// 1.23e4
// 1.23e+4
// 1.23e-4
// -1.23e4
// 123K         // with suffix
// 123Mi        // with 1024-base suffix, equivalent to `123MB`
// 123m         // with fractional suffix
// -123n        // with minus sign
// 123u@double  // with fractional suffix and data type
//
// hex numbers:
// 0xabcd
// -0xaabb
// 0xabcd@uint
//
// hex floating-point numbers:
// 0x1.23p4
// 0x1.23p+4
// 0x1.23p-4
// -0x1.23
// 0x1.23p4@double
//
// binary numbers:
// 0b0011
// -0b1100
// 0b0011@ushort
//
// default integer numbers: int(i32)
// default floating-point numbers: float(f32)
//
// data type names:
// - int, uint          (i32/u32)
// - long, ulong        (i64/u64)
// - byte, ubyte        (i8/u8)
// - short, ushort      (i16/u16)
// - float, double      (f32/f64)
//
// avaliable in XiaoXuan Lang but not in ASON
// - addr
// - uaddr
//
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
    name: String,
    value: Box<AsonNode>,
}

#[derive(Debug, PartialEq)]
pub enum AsonNode {
    Number(NumberLiteral),
    Boolean(bool),
    Char(char),
    String_(String),
    Date(DateTime<FixedOffset>),
    ByteData(Vec<u8>),
    //
    Array(Vec<AsonNode>),
    Tuple(Vec<AsonNode>),
    Object(Vec<NameValuePair>),
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_owned(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

pub fn parse_from_str(s: &str) -> Result<AsonNode, ParseError> {
    let mut chars = s.chars();
    let mut char_iter = PeekableIterator::new(&mut chars, 3);
    let tokens = lex(&mut char_iter)?;
    let mut token_iter = tokens.into_iter();
    let mut peekable_token_iter = PeekableIterator::new(&mut token_iter, 2);
    parse(&mut peekable_token_iter)
}

pub fn format_to_string(n: &AsonNode) -> String {
    format(n)
}
