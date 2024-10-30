// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::Display;

use chrono::{DateTime, FixedOffset};

use crate::location::Location;

#[derive(Debug, PartialEq)]
pub enum Token {
    // includes `\n` and `\r\n`
    NewLine,

    // `,`
    Comma,
    // `:`
    Colon,

    // {
    LeftBrace,
    // }
    RightBrace,
    // [
    LeftBracket,
    // ]
    RightBracket,
    // (
    LeftParen,
    // )
    RightParen,

    // `+`
    Plus,
    // `-`
    Minus,

    // [a-zA-Z0-9_] and '\u{a0}' - '\u{d7ff}' and '\u{e000}' - '\u{10ffff}'
    // used for object field/key name
    Identifier(String),

    // ASON has a few keywords: `true`, `false`, `Inf (Inf_f32, Inf_f64)` and `NaN (NaN_f32, NaN_f64)`,
    // but for simplicity, `true` and `false` will be converted
    // directly to `Token::Boolean`, while `NaN` and `Inf` will
    // be converted to `NumberLiteral::Float`
    // and `NumberLiternal::Double`.
    Boolean(bool),

    // includes the variant type name and member name, e.g.
    // `Option::None`
    // the "Option" is type name, and "None" is member name.
    Variant(String, String),

    Number(NumberToken),
    Char(char),
    String(String),
    Date(DateTime<FixedOffset>),
    ByteData(Vec<u8>),

    Comment(Comment),
}

// impl Token {
//     // for printing
//     pub fn get_description(&self) -> String {
//         match self {
//             Token::NewLine => "new line".to_owned(),
//             Token::Comma => "comma \",\"".to_owned(),
//             Token::Colon => "colon \":\"".to_owned(),
//             Token::LeftBrace => "left brace \"{\"".to_owned(),
//             Token::RightBrace => "right brace \"}\"".to_owned(),
//             Token::LeftBracket => "left bracket \"[\"".to_owned(),
//             Token::RightBracket => "right bracket \"]\"".to_owned(),
//             Token::LeftParen => "left parenthese \"(\"".to_owned(),
//             Token::RightParen => "right parenthese \")\"".to_owned(),
//             Token::Plus => "plus sign \"+\"".to_owned(),
//             Token::Minus => "minus sign \"-\"".to_owned(),
//             Token::Identifier(id) => format!("identifier \"{}\"", id),
//             Token::Boolean(b) => format!("boolean \"{}\"", b),
//             Token::Variant(t, m) => format!("variant \"{}::{}\"", t, m),
//             Token::Number(n) => format!(
//                 "number \"{}\"",
//                 match n {
//                     NumberToken::U8(v) => v.to_string(),
//                     NumberToken::I8(v) => v.to_string(),
//                     NumberToken::I16(v) => v.to_string(),
//                     NumberToken::U16(v) => v.to_string(),
//                     NumberToken::I32(v) => v.to_string(),
//                     NumberToken::U32(v) => v.to_string(),
//                     NumberToken::I64(v) => v.to_string(),
//                     NumberToken::U64(v) => v.to_string(),
//                     NumberToken::F32(v) => v.to_string(),
//                     NumberToken::F64(v) => v.to_string(),
//                 }
//             ),
//             Token::Char(c) => format!("char \"{}\"", c),
//             Token::String(_) => "string".to_owned(),
//             Token::Date(_) => "date".to_owned(),
//             Token::ByteData(_) => "byte data".to_owned(),
//             Token::Comment(_) => "comment".to_owned(),
//         }
//     }
// }

#[derive(Debug, PartialEq)]
pub enum NumberToken {
    // it is possible for literal to overflow for signed numbers,
    // such as `-128`, which consists of a negative/minus sign
    // and the number `128`.
    // minus token is not part of the number token,
    // and the number value 128 is out of range for `i8`,
    // so define the `i8` literal using `u8`.
    I8(u8),
    U8(u8),
    I16(u16),
    U16(u16),
    I32(u32),
    U32(u32),
    I64(u64),
    U64(u64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, PartialEq)]
pub enum Comment {
    // `//...`
    // note that the trailing '\n' or '\r\n' does not belong to line comment
    Line(String),

    // `/*...*/`
    Block(String),
}

#[derive(Debug, PartialEq)]
pub enum NumberType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

impl NumberType {
    pub fn from_str(s: &str) -> Result<Self, String> {
        let t = match s {
            "i8" => NumberType::I8,
            "i16" => NumberType::I16,
            "i32" => NumberType::I32,
            "i64" => NumberType::I64,
            "u8" => NumberType::U8,
            "u16" => NumberType::U16,
            "u32" => NumberType::U32,
            "u64" => NumberType::U64,
            "f32" => NumberType::F32,
            "f64" => NumberType::F64,
            _ => {
                return Err(format!("Invalid number type \"{}\".", s));
            }
        };

        Ok(t)
    }
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberType::I8 => write!(f, "i8"),
            NumberType::I16 => write!(f, "i16"),
            NumberType::I32 => write!(f, "i32"),
            NumberType::I64 => write!(f, "i64"),
            NumberType::U8 => write!(f, "u8"),
            NumberType::U16 => write!(f, "u16"),
            NumberType::U32 => write!(f, "u32"),
            NumberType::U64 => write!(f, "u64"),
            NumberType::F32 => write!(f, "f32"),
            NumberType::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Location,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Location) -> Self {
        Self { token, range }
    }

    pub fn from_position_and_length(token: Token, position: &Location, length: usize) -> Self {
        Self {
            token,
            range: Location::from_position_and_length(position, length),
        }
    }
}

impl Token {
    pub fn new_variant(type_name: &str, member_name: &str) -> Self {
        Token::Variant(type_name.to_owned(), member_name.to_owned())
    }

    pub fn new_identifier(s: &str) -> Self {
        Token::Identifier(s.to_owned())
    }

    pub fn new_string(s: &str) -> Self {
        Token::String(s.to_owned())
    }
}
