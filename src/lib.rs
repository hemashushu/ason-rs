// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

mod lexer;
mod parser;
mod peekable_iterator;
mod writer;

use std::fmt::Display;

use chrono::{DateTime, FixedOffset};
use lexer::{filter, lex};
use peekable_iterator::PeekableIterator;

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

pub fn parse(s: &str) -> Result<AsonNode, ParseError> {
    let mut chars = s.chars();
    let mut char_iter = PeekableIterator::new(&mut chars, 3);
    let tokens = lex(&mut char_iter)?;
    let effective_tokens = filter(tokens);
    let mut token_iter = effective_tokens.into_iter();
    let mut peekable_token_iter = PeekableIterator::new(&mut token_iter, 2);
    parser::parse(&mut peekable_token_iter)
}

pub fn write(n: &AsonNode) -> String {
    writer::write(n)
}
