// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::{fmt::Display, ops::Neg};

use chrono::{DateTime, FixedOffset};

use crate::error::Error;

use super::lookaheaditer::LookaheadIter;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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

    // includes `\n`, `\r\n`, `\r`
    NewLine,
    // `,`
    Comma,
    // `:`
    Colon,

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
    String_(String),
    Date(DateTime<FixedOffset>),
    ByteData(Vec<u8>),

    Comment(Comment),
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

impl Token {
    pub fn new_variant(type_name: &str, member_name: &str) -> Self {
        Token::Variant(type_name.to_owned(), member_name.to_owned())
    }

    pub fn new_identifier(s: &str) -> Self {
        Token::Identifier(s.to_owned())
    }

    pub fn new_string(s: &str) -> Self {
        Token::String_(s.to_owned())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Comment {
    // `//...`
    Line(String),

    // `/*...*/`
    Block(String),

    // `/**...**/`
    #[cfg(feature = "doc_comment")]
    Document(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum NumberType {
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
    fn from_str(s: &str) -> Result<Self, Error> {
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
                return Err(Error::Message(format!("Invalid number type \"{}\".", s)));
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

pub fn lex(iter: &mut LookaheadIter<char>) -> Result<Vec<Token>, Error> {
    let mut tokens: Vec<Token> = vec![];

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            ' ' | '\t' => {
                // white space
                iter.next();
            }
            '\r' => {
                // `\r`

                if iter.equals(1, &'\n') {
                    // `\r\n`
                    iter.next();
                }

                iter.next();
                tokens.push(Token::NewLine);
            }
            '\n' => {
                iter.next();
                tokens.push(Token::NewLine);
            }
            ',' => {
                iter.next();
                tokens.push(Token::Comma);
            }
            ':' => {
                // note that this is the standalone colon.
                // it does not include the seperator "::", which is
                // exists in the middle of the variant full name.
                iter.next();
                tokens.push(Token::Colon);
            }
            '{' => {
                iter.next();
                tokens.push(Token::LeftBrace);
            }
            '}' => {
                iter.next();
                tokens.push(Token::RightBrace);
            }
            '[' => {
                iter.next();
                tokens.push(Token::LeftBracket);
            }
            ']' => {
                iter.next();
                tokens.push(Token::RightBracket);
            }
            '(' => {
                iter.next();
                tokens.push(Token::LeftParen);
            }
            ')' => {
                iter.next();
                tokens.push(Token::RightParen);
            }
            '+' => {
                iter.next();
                tokens.push(Token::Plus);
            }
            '-' => {
                iter.next();
                tokens.push(Token::Minus);
            }
            '0'..='9' => {
                // number
                tokens.push(lex_number(iter)?);
            }
            'h' if iter.equals(1, &'"') => {
                // hex byte data
                tokens.push(lex_byte_data_hexadecimal(iter)?);
            }
            // 'b' if iter.equals(1, &'"') => {
            //     // string byte data
            //     tokens.push(lex_byte_data_string(iter)?);
            // }
            'd' if iter.equals(1, &'"') => {
                // date
                tokens.push(lex_date(iter)?);
            }
            'r' if iter.equals(1, &'"') => {
                // raw string
                tokens.push(lex_raw_string(iter)?);
            }
            'r' if iter.equals(1, &'#') && iter.equals(2, &'"') => {
                // raw string with hash symbol
                tokens.push(lex_raw_string_with_hash_symbol(iter)?);
            }
            '"' => {
                if iter.equals(1, &'"') && iter.equals(2, &'"') {
                    // auto-trimmed string
                    tokens.push(lex_auto_trimmed_string(iter)?);
                } else {
                    // normal string
                    tokens.push(lex_string(iter)?);
                }
            }
            '\'' => {
                // char
                tokens.push(lex_char(iter)?);
            }
            '/' if iter.equals(1, &'/') => {
                // line comment
                tokens.push(lex_line_comment(iter)?);
            }
            '/' if iter.equals(1, &'*') => {
                #[cfg(feature = "doc_comment")]
                {
                    if iter.equals(2, &'*') {
                        // document comment
                        tokens.push(lex_document_comment(iter)?);
                    } else {
                        // block comment
                        tokens.push(lex_block_comment(iter)?);
                    }
                }

                #[cfg(not(feature = "doc_comment"))]
                {
                    // block comment
                    tokens.push(lex_block_comment(iter)?);
                }
            }
            'a'..='z' | 'A'..='Z' | '_' | '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                // identifier (the key name of struct/object) or keyword
                tokens.push(lex_identifier_or_keyword(iter)?);
            }
            _ => {
                return Err(Error::Message(format!("Unexpected char: {}", current_char)));
            }
        }
    }

    Ok(tokens)
}

fn lex_identifier_or_keyword(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // key_nameT  //
    // ^       ^__// to here
    // |__________// current char, i.e. the value of 'iter.peek(0)'
    //
    // T = terminator chars

    let mut name_string = String::new();
    let mut found_double_colon = false; // found the variant name separator "::"

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                name_string.push(*current_char);
                iter.next();
            }
            ':' if iter.equals(1, &':') => {
                found_double_colon = true;
                name_string.push_str("::");
                iter.next();
                iter.next();
            }
            '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                // A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than a surrogate code point.
                // This has a fixed numerical definition: code points are in the range 0 to 0x10FFFF,
                // inclusive. Surrogate code points, used by UTF-16, are in the range 0xD800 to 0xDFFF.
                //
                // check out:
                // https://doc.rust-lang.org/std/primitive.char.html
                //
                // CJK chars: '\u{4e00}'..='\u{9fff}'
                // for complete CJK chars, check out Unicode standard
                // Ch. 18.1 Han CJK Unified Ideographs
                //
                // summary:
                // Block Range Comment
                // CJK Unified Ideographs 4E00–9FFF Common
                // CJK Unified Ideographs Extension A 3400–4DBF Rare
                // CJK Unified Ideographs Extension B 20000–2A6DF Rare, historic
                // CJK Unified Ideographs Extension C 2A700–2B73F Rare, historic
                // CJK Unified Ideographs Extension D 2B740–2B81F Uncommon, some in current use
                // CJK Unified Ideographs Extension E 2B820–2CEAF Rare, historic
                // CJK Unified Ideographs Extension F 2CEB0–2EBEF Rare, historic
                // CJK Unified Ideographs Extension G 30000–3134F Rare, historic
                // CJK Unified Ideographs Extension H 31350–323AF Rare, historic
                // CJK Compatibility Ideographs F900–FAFF Duplicates, unifiable variants, corporate characters
                // CJK Compatibility Ideographs Supplement 2F800–2FA1F Unifiable variants
                //
                // https://www.unicode.org/versions/Unicode15.0.0/ch18.pdf
                // https://en.wikipedia.org/wiki/CJK_Unified_Ideographs
                // https://www.unicode.org/versions/Unicode15.0.0/
                //
                // see also
                // https://www.unicode.org/reports/tr31/tr31-37.html

                name_string.push(*current_char);
                iter.next();
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '/'
            | '\'' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(Error::Message(format!(
                    "Invalid char for identifier: {}",
                    *current_char
                )));
            }
        }
    }

    let token = if found_double_colon {
        let (type_name, member_name) = name_string.split_once("::").unwrap();
        Token::new_variant(type_name, member_name)
    } else {
        match name_string.as_str() {
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "NaN" | "NaN_f64" => Token::Number(NumberToken::F64(f64::NAN)), // the default floating-point type is f64
            "NaN_f32" => Token::Number(NumberToken::F32(f32::NAN)),
            "Inf" | "Inf_f64" => Token::Number(NumberToken::F64(f64::INFINITY)), // the default floating-point type is f64
            "Inf_f32" => Token::Number(NumberToken::F32(f32::INFINITY)),
            _ => Token::Identifier(name_string),
        }
    };

    Ok(token)
}

// ## number formats
//
// decimal:
//
// 123
// 123.456
// -123
// 1.23e4
// 1.23e+4
// 1.23e-4
// -1.23e4
//
// hexadecimal:
//
// 0xabcd
// -0xaabb
// 0xabcd_u32
//
// hexadecimal floating-point:
// refer: https://www.ibm.com/docs/en/i/7.5?topic=literals-hexadecimal-floating-point
//
// 0x1.23p4
// 0x1.23p+4
// 0x1.23p-4
// -0x1.23
// 0x1.23p4_f64
//
// binary:
//
// 0b0011
// -0b1100
// 0b0011_u16
//
// ## number data types:
//
// - i32, u32
// - i64, u64
// - i8,  u8
// - i16, u16
// - f32, f64
//
// the default integer number type is i32
// the default floating-point number type is f64

fn lex_number(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // 123456T  //
    // ^     ^__// to here
    // |________// current char

    if iter.equals(0, &'0') && iter.equals(1, &'b') {
        // '0b...'
        lex_number_binary(iter)
    } else if iter.equals(0, &'0') && iter.equals(1, &'x') {
        // '0x...'
        lex_number_hex(iter)
    } else {
        // '1234'
        // '1.23'
        lex_number_decimal(iter)
    }
}

fn lex_number_decimal(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // 123456T  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    let mut num_string = String::new();
    let mut num_type: Option<NumberType> = None; // "_ixx", "_uxx", "_fxx"
    let mut found_point = false;
    let mut found_e = false;

    // samples:
    //
    // 123
    // 3.14
    // 2.99e8
    // 2.99e+8
    // 6.672e-34

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            '0'..='9' => {
                // valid digits for decimal number
                num_string.push(*current_char);
                iter.next();
            }
            '_' => {
                iter.next();
            }
            '.' if !found_point => {
                found_point = true;
                num_string.push(*current_char);
                iter.next();
            }
            'e' if !found_e => {
                found_e = true;

                // 123e45
                // 123e+45
                // 123e-45
                if iter.equals(1, &'-') {
                    num_string.push_str("e-");
                    iter.next();
                    iter.next();
                } else if iter.equals(1, &'+') {
                    num_string.push_str("e+");
                    iter.next();
                    iter.next();
                } else {
                    num_string.push(*current_char);
                    iter.next();
                }
            }
            'i' | 'u' | 'f' if num_type.is_none() && matches!(iter.peek(1), Some('0'..='9')) => {
                num_type.replace(lex_number_type(iter)?);
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '/'
            | '\'' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(Error::Message(format!(
                    "Invalid char for decimal number: {}",
                    *current_char
                )));
            }
        }
    }

    // check syntax
    if num_string.ends_with('.') {
        return Err(Error::Message(format!(
            "A number can not ends with \".\": {}",
            num_string
        )));
    }

    if num_string.ends_with('e') {
        return Err(Error::Message(format!(
            "A number can not ends with \"e\": {}",
            num_string
        )));
    }

    let num_token: NumberToken;

    if let Some(nt) = num_type {
        // numbers with explicit type

        match nt {
            NumberType::I8 => {
                let v = num_string.parse::<u8>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I8(v);
            }
            NumberType::U8 => {
                let v = num_string.parse::<u8>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U8(v);
            }
            NumberType::I16 => {
                let v = num_string.parse::<u16>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I16(v);
            }
            NumberType::U16 => {
                let v = num_string.parse::<u16>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U16(v);
            }
            NumberType::I32 => {
                let v = num_string.parse::<u32>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I32(v);
            }
            NumberType::U32 => {
                let v = num_string.parse::<u32>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U32(v);
            }
            NumberType::I64 => {
                let v = num_string.parse::<u64>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I64(v);
            }
            NumberType::U64 => {
                let v = num_string.parse::<u64>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U64(v);
            }
            NumberType::F32 => {
                let v = num_string.parse::<f32>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to f32 floating-point number, error: {}",
                        num_string, e
                    ))
                })?;

                // overflow when parsing from string
                if v.is_infinite() {
                    return Err(Error::Message(format!(
                        "F32 floating point number overflow: {}.",
                        num_string
                    )));
                }

                num_token = NumberToken::F32(v);
            }
            NumberType::F64 => {
                let v = num_string.parse::<f64>().map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to f64 floating-point number, error: {}",
                        num_string, e
                    ))
                })?;

                // overflow when parsing from string
                if v.is_infinite() {
                    return Err(Error::Message(format!(
                        "F64 floating point number overflow: {}.",
                        num_string
                    )));
                }

                num_token = NumberToken::F64(v);
            }
        }
    } else if found_point || found_e {
        // the default floating-point number type is f64

        let v = num_string.parse::<f64>().map_err(|e| {
            Error::Message(format!(
                "Can not convert \"{}\" to f64 floating-point number, error: {}",
                num_string, e
            ))
        })?;

        // overflow when parsing from string
        if v.is_infinite() {
            return Err(Error::Message(format!(
                "F64 floating point number overflow: {}.",
                num_string
            )));
        }

        num_token = NumberToken::F64(v);
    } else {
        // the default integer number type is i32

        let v = num_string.parse::<u32>().map_err(|e| {
            Error::Message(format!(
                "Can not convert \"{}\" to i32 integer number, error: {}",
                num_string, e
            ))
        })?;

        num_token = NumberToken::I32(v);
    }

    Ok(Token::Number(num_token))
}

fn lex_number_type(iter: &mut LookaheadIter<char>) -> Result<NumberType, Error> {
    // ixxT  //
    // ^  ^__// to here
    // |_____// current char
    //
    // i = i/u/f
    // x = 0..=9
    // T = terminator chars

    let mut num_type = String::new();

    let c = iter.next().unwrap(); // consume the char 'i/u/f'
    num_type.push(c);

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            '0'..='9' => {
                // valid char for type name
                num_type.push(*current_char);
                iter.next();
            }
            _ => {
                break;
            }
        }
    }

    NumberType::from_str(&num_type)
}

fn lex_number_hex(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // 0xaabbT  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    // consume '0x'
    iter.next();
    iter.next();

    let mut num_string = String::new();
    let mut num_type: Option<NumberType> = None;

    let mut found_point: bool = false;
    let mut found_p: bool = false;

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            'i' | 'u'
                if num_type.is_none()
                    && !found_point
                    && !found_p
                    && matches!(iter.peek(1), Some('0'..='9')) =>
            {
                // only 'i' and 'u' are allowed for hexadecimal integer numbers
                num_type.replace(lex_number_type(iter)?);
            }
            'f' if num_type.is_none() && found_p && matches!(iter.peek(1), Some('0'..='9')) => {
                // 'f' must be followed by '.' or 'p' directly or indirectly
                num_type.replace(lex_number_type(iter)?);
            }
            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                // valid digits for hex number
                num_string.push(*current_char);
                iter.next();
            }
            '_' => {
                iter.next();
            }
            '.' if !found_point => {
                // it is hex floating point literal
                found_point = true;
                num_string.push(*current_char);
                iter.next();
            }
            'p' if !found_p => {
                // it is hex floating point literal
                found_p = true;

                // 0x0.123p45
                // 0x0.123p+45
                // 0x0.123p-45
                if iter.equals(1, &'-') {
                    num_string.push_str("p-");
                    iter.next();
                    iter.next();
                } else if iter.equals(1, &'+') {
                    num_string.push_str("p+");
                    iter.next();
                    iter.next();
                } else {
                    num_string.push(*current_char);
                    iter.next();
                }
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '/'
            | '\'' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(Error::Message(format!(
                    "Invalid char for hexadecimal number: {}",
                    *current_char
                )));
            }
        }
    }

    if num_string.is_empty() {
        return Err(Error::Message("Incomplete hexadecimal number".to_owned()));
    }

    let num_token: NumberToken;

    if found_point || found_p {
        // the default type for floating-point is f64
        let mut to_f64 = true;

        if let Some(nt) = num_type {
            match nt {
                NumberType::F32 => {
                    to_f64 = false;
                }
                NumberType::F64 => {
                    to_f64 = true;
                }
                _ => {
                    return Err(Error::Message(format!(
                        "Only number type \"f32\" and \"f64\" are allowed for hexadecimal floating-point numbers, current type: {}",
                        nt
                    )));
                }
            }
        }

        num_string.insert_str(0, "0x");

        if to_f64 {
            let v = hexfloat2::parse::<f64>(&num_string).map_err(|_| {
                Error::Message(format!(
                    "Can not convert \"{}\" to f64 floating-point number.",
                    num_string
                ))
            })?;

            num_token = NumberToken::F64(v)
        } else {
            let v = hexfloat2::parse::<f32>(&num_string).map_err(|_| {
                Error::Message(format!(
                    "Can not convert \"{}\" to f32 floating-point number.",
                    num_string
                ))
            })?;

            num_token = NumberToken::F32(v)
        };
    } else if let Some(nt) = num_type {
        match nt {
            NumberType::I8 => {
                let v = u8::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I8(v);
            }
            NumberType::U8 => {
                let v = u8::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U8(v);
            }
            NumberType::I16 => {
                let v = u16::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I16(v);
            }
            NumberType::U16 => {
                let v = u16::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U16(v);
            }
            NumberType::I32 => {
                let v = u32::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I32(v);
            }
            NumberType::U32 => {
                let v = u32::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U32(v);
            }
            NumberType::I64 => {
                let v = u64::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I64(v);
            }
            NumberType::U64 => {
                let v = u64::from_str_radix(&num_string, 16).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U64(v);
            }
            NumberType::F32 | NumberType::F64 => {
                // '0x..f32' and '0x..f64' would only be parsed
                // as ordinary hex digits
                unreachable!()
            }
        }
    } else {
        // default
        // convert to i32
        let v = u32::from_str_radix(&num_string, 16).map_err(|e| {
            Error::Message(format!(
                "Can not convert \"{}\" to i32 integer number, error: {}",
                num_string, e
            ))
        })?;

        num_token = NumberToken::I32(v);
    }

    Ok(Token::Number(num_token))
}

fn lex_number_binary(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // 0b1010T  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    // consume '0b'
    iter.next();
    iter.next();

    let mut num_string = String::new();
    let mut num_type: Option<NumberType> = None;

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            '0' | '1' => {
                // valid digits for binary number
                num_string.push(*current_char);
                iter.next();
            }
            '_' => {
                iter.next();
            }
            'i' | 'u' if num_type.is_none() && matches!(iter.peek(1), Some('0'..='9')) => {
                num_type.replace(lex_number_type(iter)?);
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '/'
            | '\'' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(Error::Message(format!(
                    "Invalid char for binary number: {}",
                    *current_char
                )));
            }
        }
    }

    if num_string.is_empty() {
        return Err(Error::Message("Incomplete binary number.".to_owned()));
    }

    let num_token: NumberToken;

    if let Some(nt) = num_type {
        match nt {
            NumberType::F32 | NumberType::F64 => {
                return Err(Error::Message(format!(
                    "Does not support binary floating point number: {}.",
                    num_string
                )));
            }
            NumberType::I8 => {
                let v = u8::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I8(v);
            }
            NumberType::U8 => {
                let v = u8::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u8 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U8(v);
            }
            NumberType::I16 => {
                let v = u16::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I16(v);
            }
            NumberType::U16 => {
                let v = u16::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u16 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U16(v);
            }
            NumberType::I32 => {
                let v = u32::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I32(v);
            }
            NumberType::U32 => {
                let v = u32::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u32 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U32(v);
            }
            NumberType::I64 => {
                let v = u64::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to i64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::I64(v);
            }
            NumberType::U64 => {
                let v = u64::from_str_radix(&num_string, 2).map_err(|e| {
                    Error::Message(format!(
                        "Can not convert \"{}\" to u64 integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::U64(v);
            }
        }
    } else {
        // default
        // convert to i32

        let v = u32::from_str_radix(&num_string, 2).map_err(|e| {
            Error::Message(format!(
                "Can not convert \"{}\" to i32 integer number, error: {}",
                num_string, e
            ))
        })?;

        num_token = NumberToken::I32(v);
    }

    Ok(Token::Number(num_token))
}

fn lex_char(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // 'a'?  //
    // ^  ^__// to here
    // |_____// current char

    iter.next(); // consume the left single quote

    let c: char;

    match iter.next() {
        Some(previous_char) => match previous_char {
            '\\' => {
                // escape chars
                match iter.next() {
                    Some(current_char) => {
                        match current_char {
                            '\\' => {
                                c = '\\';
                            }
                            '\'' => {
                                c = '\'';
                            }
                            '"' => {
                                // double quote does not necessary to be escaped
                                c = '"';
                            }
                            't' => {
                                // horizontal tabulation
                                c = '\t';
                            }
                            'r' => {
                                // carriage return (CR)
                                c = '\r';
                            }
                            'n' => {
                                // new line character (line feed, LF)
                                c = '\n';
                            }
                            '0' => {
                                // null char
                                c = '\0';
                            }
                            'u' => {
                                // unicode code point, e.g. '\u{2d}', '\u{6587}'
                                c = unescape_unicode(iter)?;
                            }
                            _ => {
                                return Err(Error::Message(format!(
                                    "Unsupported escape char: \"{}\"",
                                    current_char
                                )));
                            }
                        }
                    }
                    None => return Err(Error::Message("Incomplete escape char.".to_owned())),
                }
            }
            _ => {
                // ordinary char
                c = previous_char;
            }
        },
        None => return Err(Error::Message("Incomplete char.".to_owned())),
    }

    // consume the right single quote
    match iter.next() {
        Some('\'') => {
            // ok
        }
        _ => {
            return Err(Error::Message(
                "Missing end single quote for char.".to_owned(),
            ))
        }
    }

    Ok(Token::Char(c))
}

fn lex_string(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // "abc"?  //
    // ^    ^__// to here
    // |_______// current char

    iter.next(); // consume the left quote

    let mut ss = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '\\' => {
                    // escape chars
                    match iter.next() {
                        Some(current_char) => {
                            match current_char {
                                '\\' => {
                                    ss.push('\\');
                                }
                                '\'' => {
                                    ss.push('\'');
                                }
                                '"' => {
                                    ss.push('"');
                                }
                                't' => {
                                    // horizontal tabulation
                                    ss.push('\t');
                                }
                                'r' => {
                                    // carriage return (CR)
                                    ss.push('\r');
                                }
                                'n' => {
                                    // new line character (line feed, LF)
                                    ss.push('\n');
                                }
                                '0' => {
                                    // null char
                                    ss.push('\0');
                                }
                                'u' => {
                                    // unicode code point, e.g. '\u{2d}', '\u{6587}'
                                    ss.push(unescape_unicode(iter)?);
                                }
                                '\n' => {
                                    // multiple-line string
                                    let _ = consume_leading_whitespaces(iter)?;
                                }
                                '\r' if iter.equals(0, &'\n') => {
                                    // multiple-line string
                                    iter.next();
                                    let _ = consume_leading_whitespaces(iter)?;
                                }
                                _ => {
                                    return Err(Error::Message(format!(
                                        "Unsupported escape char: \"{}\"",
                                        current_char
                                    )));
                                }
                            }
                        }
                        None => return Err(Error::Message("Incomplete escape char.".to_owned())),
                    }
                }
                '"' => {
                    // end of the string
                    break;
                }
                _ => {
                    // ordinary char
                    ss.push(previous_char);
                }
            },
            None => return Err(Error::Message("Missing end quote for string.".to_owned())),
        }
    }

    Ok(Token::String_(ss))
}

/// return the amount of leading whitespaces
fn consume_leading_whitespaces(iter: &mut LookaheadIter<char>) -> Result<usize, Error> {
    // \nssssS  //
    //   ^   ^__// to here ('s' = whitespace, i.e. [ \t], 'S' = not whitespace)
    //   |______// current char

    let mut count = 0;
    loop {
        match iter.peek(0) {
            Some(next_char) if next_char == &' ' || next_char == &'\t' => {
                count += 1;
                iter.next();
            }
            None => return Err(Error::Message("Expect the non-whitespace text.".to_owned())),
            _ => break,
        }
    }

    Ok(count)
}

fn skip_leading_whitespaces(iter: &mut LookaheadIter<char>, max_whitespaces: usize) {
    for _ in 0..max_whitespaces {
        match iter.peek(0) {
            Some(next_char) if next_char == &' ' || next_char == &'\t' => {
                iter.next();
            }
            _ => break,
        }
    }
}

fn unescape_unicode(iter: &mut LookaheadIter<char>) -> Result<char, Error> {
    // \u{6587}?  //
    //   ^     ^__// to here
    //   |________// current char

    // comsume char '{'
    if !matches!(iter.next(), Some(c) if c == '{') {
        return Err(Error::Message(
            "Missing left brace for unicode escape sequence.".to_owned(),
        ));
    }

    let mut codepoint_string = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '}' => break,
                '0'..='9' | 'a'..='f' | 'A'..='F' => codepoint_string.push(previous_char),
                _ => {
                    return Err(Error::Message(format!(
                        "Invalid character for unicode escape sequence: {}",
                        previous_char
                    )));
                }
            },
            None => {
                return Err(Error::Message(
                    "Missing right brace for unicode escape sequence.".to_owned(),
                ));
            }
        }

        if codepoint_string.len() > 5 {
            return Err(Error::Message(
                "Only max 5 hexadecimal digits are allowed for unicode point code.".to_owned(),
            ));
        }
    }

    let codepoint = u32::from_str_radix(&codepoint_string, 16).unwrap();

    if let Some(unic) = char::from_u32(codepoint) {
        // valid code point:
        // 0 to 0x10FFFF, inclusive
        //
        // ref:
        // https://doc.rust-lang.org/std/primitive.char.html
        Ok(unic)
    } else {
        Err(Error::Message("Invalid unicode code point.".to_owned()))
    }
}

fn lex_raw_string(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // r"abc"?  //
    // ^     ^__// to here
    // |________// current char

    iter.next(); // consume char 'r'
    iter.next(); // consume the quote

    let mut raw_string = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '"' => {
                    // end of the string
                    break;
                }
                _ => {
                    // ordinary char
                    raw_string.push(previous_char);
                }
            },
            None => return Err(Error::Message("Missing end quote for string.".to_owned())),
        }
    }

    Ok(Token::String_(raw_string))
}

fn lex_raw_string_with_hash_symbol(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // r#"abc"#?  //
    // ^       ^__// to here
    // |__________// current char

    iter.next(); // consume char 'r'
    iter.next(); // consume the hash
    iter.next(); // consume the quote

    let mut raw_string = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '"' if iter.equals(0, &'#') => {
                    // end of the string
                    iter.next(); // consume the hash
                    break;
                }
                _ => {
                    // ordinary char
                    raw_string.push(previous_char);
                }
            },
            None => return Err(Error::Message("Missing end quote for string.".to_owned())),
        }
    }

    Ok(Token::String_(raw_string))
}

fn lex_auto_trimmed_string(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // """                    //
    // ^  auto-trimmed string //
    // |  ...                 //
    // |  """?                //
    // |     ^________________// to here ('?' = any chars or EOF)
    // |______________________// current char

    iter.next(); // consume char "
    iter.next(); // consume char "
    iter.next(); // consume char "

    if iter.equals(0, &'\n') {
        iter.next();
    } else if iter.equals(0, &'\r') && iter.equals(1, &'\n') {
        iter.next();
        iter.next();
    } else {
        return Err(Error::Message(
            "The content of auto-trimmed string should start on a new line.".to_owned(),
        ));
    }

    let leading_whitespace_count = consume_leading_whitespaces(iter)?;
    let mut total_string = String::new();
    let mut line_leading = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => {
                match previous_char {
                    '\n' => {
                        total_string.push('\n');
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespace_count);
                    }
                    '\r' if iter.equals(0, &'\n') => {
                        iter.next(); // consume '\n'

                        total_string.push_str("\r\n");
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespace_count);
                    }
                    '"' if line_leading.trim().is_empty()
                        && iter.equals(0, &'"')
                        && iter.equals(1, &'"') =>
                    {
                        iter.next(); // consume '"'
                        iter.next(); // consume '"'
                        break;
                    }
                    _ => {
                        total_string.push(previous_char);
                        line_leading.push(previous_char);
                    }
                }
            }
            None => {
                return Err(Error::Message(
                    "Missing the ending marker for the auto-trimmed string.".to_owned(),
                ));
            }
        }
    }

    Ok(Token::String_(total_string.trim_end().to_owned()))
}

fn lex_date(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // d"2024-03-16T16:30:50+08:00"?  //
    // ^                           ^__// to here
    // |______________________________// current char

    iter.next(); // consume the char 'd'
    iter.next(); // consume left quote

    let mut date_string = String::new();

    loop {
        match iter.next() {
            Some(c) => match c {
                '"' => {
                    // end of the date time string
                    break;
                }
                '0'..='9' | '-' | ':' | ' ' | 't' | 'T' | 'z' | 'Z' | '+' => {
                    date_string.push(c);
                }
                _ => {
                    return Err(Error::Message(format!("Invalid char for date time: {}", c)));
                }
            },
            None => return Err(Error::Message("Incomplete date time.".to_owned())),
        }
    }

    if date_string.len() < 19 {
        return Err(Error::Message(format!(
            "Incorrect date time (format: YYYY-MM-DD HH:mm:ss) string: {}",
            date_string
        )));
    }

    if date_string.len() == 19 {
        date_string.push('z');
    }

    let rfc3339 = DateTime::parse_from_rfc3339(&date_string).map_err(|_| {
        Error::Message(format!(
            "Can not parse the string into datetime: {}",
            date_string
        ))
    })?;

    Ok(Token::Date(rfc3339))
}

fn lex_byte_data_hexadecimal(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // h"00 11 aa bb"?  //
    // ^             ^__// to here
    // |________________// current char

    let mut bytes: Vec<u8> = Vec::new();
    // let mut byte_buf = String::with_capacity(2);
    let mut chars: [char; 2] = ['0', '0'];

    iter.next(); // consume char 'h'
    iter.next(); // consume quote '"'

    let consume_whitespace_if_exits = |t: &mut LookaheadIter<char>| loop {
        match t.peek(0) {
            Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
                t.next();
            }
            _ => {
                break;
            }
        }
    };

    let consume_withspaces = |t: &mut LookaheadIter<char>| -> Result<(), Error> {
        let mut found: bool = false;
        loop {
            match t.peek(0) {
                Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
                    t.next();
                    found = true;
                }
                _ => {
                    if found {
                        break;
                    } else {
                        return Err(Error::Message(
                            "Expect whitespace between hexadecimal byte data digits.".to_owned(),
                        ));
                    }
                }
            }
        }
        Ok(())
    };

    loop {
        consume_whitespace_if_exits(iter);

        if iter.equals(0, &'"') {
            break;
        }

        for idx in 0..2usize {
            match iter.next() {
                Some(previous_char) => match previous_char {
                    'a'..='f' | 'A'..='F' | '0'..='9' => {
                        chars[idx] = previous_char;
                    }
                    _ => {
                        return Err(Error::Message(format!(
                            "Invalid digit for hexadecimal byte data: {}",
                            previous_char
                        )));
                    }
                },
                None => {
                    return Err(Error::Message(
                        "Incomplete hexadecimal byte data.".to_owned(),
                    ))
                }
            }
        }

        let byte_string = String::from_iter(chars);
        let byte_number = u8::from_str_radix(&byte_string, 16).unwrap();
        bytes.push(byte_number);

        if iter.equals(0, &'"') {
            break;
        }

        consume_withspaces(iter)?; // consume at lease one whitespace
    }

    iter.next(); // consume '"'

    Ok(Token::ByteData(bytes))
}

// fn lex_byte_data_string(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
//     // b"..."?  //
//     // ^     ^__// to here
//     // |________// current char
//
//     let s = consume_string(iter)?;
//     let v = s.as_bytes().to_vec();
//     Ok(Token::ByteData(v))
// }

fn lex_line_comment(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // xx...[\r]\n?  //
    // ^          ^__// to here ('?' = any char or EOF)
    // |_____________// current char
    //
    // x = '/'

    iter.next(); // consume char '/'
    iter.next(); // consume char '/'

    let mut comment_string = String::new();

    while let Some(previous_char) = iter.next() {
        // ignore all chars except '\n' or '\r\n'
        // note that the line comment includes the ending new line chars (\n or \r\n),
        // so there is NO `Token::NewLine` follows the line comment.

        if previous_char == '\n' {
            break;
        } else if previous_char == '\r' && iter.equals(0, &'\n') {
            iter.next(); // consume char '\n'
            break;
        }

        comment_string.push(previous_char);
    }

    Ok(Token::Comment(Comment::Line(comment_string)))
}

fn lex_block_comment(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // /*...*/?  //
    // ^      ^__// to here
    // |_________// current char

    iter.next(); // consume char '/'
    iter.next(); // consume char '*'

    let mut comment_string = String::new();
    let mut pairs = 1;

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '/' if iter.equals(0, &'*') => {
                    // nested block comment
                    comment_string.push_str("/*");
                    iter.next();
                    pairs += 1;
                }
                '*' if iter.equals(0, &'/') => {
                    iter.next();
                    pairs -= 1;

                    // check pairs
                    if pairs == 0 {
                        break;
                    } else {
                        comment_string.push_str("*/");
                    }
                }
                _ => {
                    // ignore all chars except "/*" and "*/"
                    // note that line comments within block comments are ignored.
                    comment_string.push(previous_char);
                }
            },
            None => return Err(Error::Message("Incomplete block comment.".to_owned())),
        }
    }

    Ok(Token::Comment(Comment::Block(comment_string)))
}

#[cfg(feature = "doc_comment")]
fn lex_document_comment(iter: &mut LookaheadIter<char>) -> Result<Token, Error> {
    // /**...**/?  //
    // ^        ^__// to here
    // |___________// current char

    iter.next(); // consume char '/'
    iter.next(); // consume char '*'
    iter.next(); // consume char '*'

    let mut comment_string = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '*' if iter.equals(0, &'*') && iter.equals(1, &'/') => {
                    iter.next();
                    iter.next();

                    break;
                }
                _ => {
                    // ignore all chars except "**/"
                    // note that line comments and block comments within block comments are ignored.
                    // document comment does not support nested.
                    comment_string.push(previous_char);
                }
            },
            None => return Err(Error::Message("Incomplete document comment.".to_owned())),
        }
    }

    Ok(Token::Comment(Comment::Document(comment_string)))
}

// - remove all comments.
// - convert commas into newlines
// - combine multiple continuous newlines into one newline.
// - remove the '+' tokens in front of numbers (includes `Inf`).
// - apple the '-' tokens into numbers (includes `Inf`).
// - checks if the signed number is overflowed
// - remove document leading newline and tailing newline.
pub fn normalize(tokens: Vec<Token>) -> Result<Vec<Token>, Error> {
    let mut normalized_tokens = vec![];

    let mut into = tokens.into_iter();
    let mut iter = LookaheadIter::new(&mut into, 2);

    // remove the leading new-lines and comments of document
    while let Some(Token::NewLine) | Some(Token::Comment(_)) = iter.peek(0) {
        // consume newlines and comments
        iter.next();
    }

    while let Some(current_token) = iter.peek(0) {
        match current_token {
            Token::Comment(_) => {
                // consume comments
                iter.next();
            }
            Token::NewLine | Token::Comma => {
                iter.next();
                normalized_tokens.push(Token::NewLine);

                // - treat commas as newlines
                // - consume multiple continuous newlines

                while let Some(Token::NewLine) | Some(Token::Comma) | Some(Token::Comment(_)) =
                    iter.peek(0)
                {
                    iter.next();
                }
            }
            Token::Plus => {
                match iter.peek(1) {
                    Some(Token::Number(num)) => {
                        match num {
                            NumberToken::F32(f) if f.is_nan() => {
                                return Err(Error::Message(
                                    "The plus sign cannot be added to NaN.".to_owned(),
                                ));
                            }
                            NumberToken::F64(f) if f.is_nan() => {
                                return Err(Error::Message(
                                    "The plus sign cannot be added to NaN.".to_owned(),
                                ));
                            }
                            _ => {
                                // consume the plus sign
                                iter.next();
                            }
                        }
                    }
                    Some(_) => {
                        return Err(Error::Message(
                            "The plus sign cannot be added to other than numbers.".to_owned(),
                        ))
                    }
                    None => return Err(Error::Message("Unexpected end of document.".to_owned())),
                }
            }
            Token::Minus => {
                match iter.peek(1) {
                    Some(Token::Number(num)) => {
                        match num {
                            NumberToken::F32(v) => {
                                if v.is_nan() {
                                    return Err(Error::Message(
                                        "The minus sign cannot be added to NaN.".to_owned(),
                                    ));
                                } else {
                                    // consume the minus sign and the number literal token
                                    let token = Token::Number(NumberToken::F32(v.neg()));
                                    iter.next();
                                    iter.next();
                                    normalized_tokens.push(token);
                                }
                            }
                            NumberToken::F64(v) => {
                                if v.is_nan() {
                                    return Err(Error::Message(
                                        "The minus sign cannot be added to NaN.".to_owned(),
                                    ));
                                } else {
                                    // consume the minus sign and the number literal token
                                    let token = Token::Number(NumberToken::F64(v.neg()));
                                    iter.next();
                                    iter.next();
                                    normalized_tokens.push(token);
                                }
                            }
                            NumberToken::I8(v) => {
                                // consume the minus sign and the number literal token
                                let token = Token::Number(NumberToken::I8(
                                    format!("-{}", v).parse::<i8>().map_err(|e| {
                                        Error::Message(format!(
                                            "Can not convert \"{}\" to negative, error: {}",
                                            v, e
                                        ))
                                    })? as u8,
                                ));
                                iter.next();
                                iter.next();
                                normalized_tokens.push(token);
                            }
                            NumberToken::I16(v) => {
                                // consume the minus sign and the number literal token
                                let token = Token::Number(NumberToken::I16(
                                    format!("-{}", v).parse::<i16>().map_err(|e| {
                                        Error::Message(format!(
                                            "Can not convert \"{}\" to negative, error: {}",
                                            v, e
                                        ))
                                    })? as u16,
                                ));
                                iter.next();
                                iter.next();
                                normalized_tokens.push(token);
                            }
                            NumberToken::I32(v) => {
                                // consume the minus sign and the number literal token
                                let token = Token::Number(NumberToken::I32(
                                    format!("-{}", v).parse::<i32>().map_err(|e| {
                                        Error::Message(format!(
                                            "Can not convert \"{}\" to negative, error: {}",
                                            v, e
                                        ))
                                    })? as u32,
                                ));
                                iter.next();
                                iter.next();
                                normalized_tokens.push(token);
                            }
                            NumberToken::I64(v) => {
                                // consume the minus sign and the number literal token
                                let token = Token::Number(NumberToken::I64(
                                    format!("-{}", v).parse::<i64>().map_err(|e| {
                                        Error::Message(format!(
                                            "Can not convert \"{}\" to negative, error: {}",
                                            v, e
                                        ))
                                    })? as u64,
                                ));
                                iter.next();
                                iter.next();
                                normalized_tokens.push(token);
                            }
                            NumberToken::U8(_)
                            | NumberToken::U16(_)
                            | NumberToken::U32(_)
                            | NumberToken::U64(_) => {
                                return Err(Error::Message(
                                    "The minus sign cannot be added to unsigned numbers."
                                        .to_owned(),
                                ))
                            }
                        }
                    }
                    Some(_) => {
                        return Err(Error::Message(
                            "The minus sign cannot be added to other than numbers.".to_owned(),
                        ))
                    }
                    None => return Err(Error::Message("Unexpected end of document.".to_owned())),
                }
            }
            Token::Number(NumberToken::I8(v)) if *v > i8::MAX as u8 => {
                // check signed number overflow
                return Err(Error::Message(format!(
                    "The byte integer number {} is overflowed.",
                    v
                )));
            }
            Token::Number(NumberToken::I16(v)) if *v > i16::MAX as u16 => {
                // check signed number overflow
                return Err(Error::Message(format!(
                    "The short integer number {} is overflowed.",
                    v
                )));
            }
            Token::Number(NumberToken::I32(v)) if *v > i32::MAX as u32 => {
                // check signed number overflow
                return Err(Error::Message(format!(
                    "The integer number {} is overflowed.",
                    v
                )));
            }
            Token::Number(NumberToken::I64(v)) if *v > i64::MAX as u64 => {
                // check signed number overflow
                return Err(Error::Message(format!(
                    "The long integer number {} is overflowed.",
                    v
                )));
            }
            _ => {
                let token = iter.next().unwrap();
                normalized_tokens.push(token);
            }
        }
    }

    // remove the trailing newline token of document
    if let Some(Token::NewLine) = normalized_tokens.last() {
        normalized_tokens.pop();
    }

    Ok(normalized_tokens)
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use pretty_assertions::assert_eq;

    use crate::{
        error::Error,
        lexer::{Comment, NumberToken},
        lookaheaditer::LookaheadIter,
    };

    use super::{lex, normalize, Token};

    fn lex_from_str(s: &str) -> Result<Vec<Token>, Error> {
        let mut chars = s.chars();
        let mut iter = LookaheadIter::new(&mut chars, 3);
        lex(&mut iter)
    }

    fn lex_and_normalize_from_str(s: &str) -> Result<Vec<Token>, Error> {
        normalize(lex_from_str(s)?)
    }

    #[test]
    fn test_lex_white_spaces() {
        assert_eq!(lex_from_str("  ").unwrap(), vec![]);

        assert_eq!(
            lex_from_str("()").unwrap(),
            vec![Token::LeftParen, Token::RightParen]
        );

        assert_eq!(
            lex_from_str("(  )").unwrap(),
            vec![Token::LeftParen, Token::RightParen]
        );

        assert_eq!(
            lex_from_str("(\t\r\n\n\r)").unwrap(),
            vec![
                Token::LeftParen,
                Token::NewLine,
                Token::NewLine,
                Token::NewLine,
                Token::RightParen,
            ]
        );
    }

    #[test]
    fn test_lex_punctuations() {
        assert_eq!(
            lex_from_str(":{[],()}+-").unwrap(),
            vec![
                Token::Colon,
                Token::LeftBrace,
                Token::LeftBracket,
                Token::RightBracket,
                Token::Comma,
                Token::LeftParen,
                Token::RightParen,
                Token::RightBrace,
                Token::Plus,
                Token::Minus
            ]
        );
    }

    #[test]
    fn test_lex_identifier() {
        assert_eq!(
            lex_from_str("name").unwrap(),
            vec![Token::new_identifier("name")]
        );

        assert_eq!(
            lex_from_str("(name)").unwrap(),
            vec![
                Token::LeftParen,
                Token::new_identifier("name"),
                Token::RightParen,
            ]
        );

        assert_eq!(
            lex_from_str("( a )").unwrap(),
            vec![
                Token::LeftParen,
                Token::new_identifier("a"),
                Token::RightParen,
            ]
        );

        assert_eq!(
            lex_from_str("a__b__c").unwrap(),
            vec![Token::new_identifier("a__b__c")]
        );

        assert_eq!(
            lex_from_str("foo bar").unwrap(),
            vec![Token::new_identifier("foo"), Token::new_identifier("bar")]
        );

        assert_eq!(
            lex_from_str("αβγ 文字 🍞🥛").unwrap(),
            vec![
                Token::new_identifier("αβγ"),
                Token::new_identifier("文字"),
                Token::new_identifier("🍞🥛"),
            ]
        );

        // err: starts with number
        assert!(matches!(lex_from_str("1abc"), Err(Error::Message(_))));

        // err: invalid char
        assert!(matches!(lex_from_str("abc&xyz"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_variant() {
        assert_eq!(
            lex_from_str("Option::None").unwrap(),
            vec![Token::new_variant("Option", "None")]
        );

        assert_eq!(
            lex_from_str("Option::Some(123)").unwrap(),
            vec![
                Token::new_variant("Option", "Some"),
                Token::LeftParen,
                Token::Number(NumberToken::I32(123)),
                Token::RightParen,
            ]
        );

        assert_eq!(
            lex_from_str("value: Result::Ok(456)").unwrap(),
            vec![
                Token::new_identifier("value"),
                Token::Colon,
                Token::new_variant("Result", "Ok"),
                Token::LeftParen,
                Token::Number(NumberToken::I32(456)),
                Token::RightParen,
            ]
        );
    }

    #[test]
    fn test_lex_keyword() {
        assert_eq!(lex_from_str("true").unwrap(), vec![Token::Boolean(true)]);

        assert_eq!(lex_from_str("false").unwrap(), vec![Token::Boolean(false)]);

        assert_eq!(
            lex_from_str("true false").unwrap(),
            vec![Token::Boolean(true), Token::Boolean(false)]
        );

        assert_eq!(
            lex_from_str("Inf Inf_f32 Inf_f64").unwrap(),
            vec![
                Token::Number(NumberToken::F64(f64::INFINITY)),
                Token::Number(NumberToken::F32(f32::INFINITY)),
                Token::Number(NumberToken::F64(f64::INFINITY)),
            ]
        );

        let nans = lex_from_str("NaN NaN_f32 NaN_f64").unwrap();
        assert!(matches!(nans[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));
        assert!(matches!(nans[1], Token::Number(NumberToken::F32(v)) if v.is_nan()));
        assert!(matches!(nans[2], Token::Number(NumberToken::F64(v)) if v.is_nan()));

        assert_eq!(
            lex_from_str("Inf_i32").unwrap(),
            vec![Token::new_identifier("Inf_i32")]
        );

        assert_eq!(
            lex_from_str("NaN_i32").unwrap(),
            vec![Token::new_identifier("NaN_i32")]
        );
    }

    #[test]
    fn test_lex_decimal_number() {
        assert_eq!(
            lex_from_str("(211)").unwrap(),
            vec![
                Token::LeftParen,
                Token::Number(NumberToken::I32(211)),
                Token::RightParen,
            ]
        );

        assert_eq!(
            lex_from_str("211").unwrap(),
            vec![Token::Number(NumberToken::I32(211))]
        );

        assert_eq!(
            lex_from_str("-2017").unwrap(),
            vec![Token::Minus, Token::Number(NumberToken::I32(2017))]
        );

        assert_eq!(
            lex_from_str("+2024").unwrap(),
            vec![Token::Plus, Token::Number(NumberToken::I32(2024))]
        );

        assert_eq!(
            lex_from_str("223_211").unwrap(),
            vec![Token::Number(NumberToken::I32(223_211))]
        );

        assert_eq!(
            lex_from_str("223 211").unwrap(),
            vec![
                Token::Number(NumberToken::I32(223)),
                Token::Number(NumberToken::I32(211)),
            ]
        );

        // err: integer number overflow
        assert!(matches!(
            lex_from_str("4_294_967_296"),
            Err(Error::Message(_))
        ));

        // err: invalid char for decimal number
        assert!(matches!(lex_from_str("123XYZ"), Err(Error::Message(_))));
    }

    #[allow(clippy::approx_constant)]
    #[test]
    fn test_lex_decimal_number_floating_point() {
        assert_eq!(
            lex_from_str("3.14").unwrap(),
            vec![Token::Number(NumberToken::F64(3.14))]
        );

        assert_eq!(
            lex_from_str("+1.414").unwrap(),
            vec![Token::Plus, Token::Number(NumberToken::F64(1.414))]
        );

        assert_eq!(
            lex_from_str("-2.718").unwrap(),
            vec![Token::Minus, Token::Number(NumberToken::F64(2.718))]
        );

        assert_eq!(
            lex_from_str("2.998e8").unwrap(),
            vec![Token::Number(NumberToken::F64(2.998e8))]
        );

        assert_eq!(
            lex_from_str("2.998e+8").unwrap(),
            vec![Token::Number(NumberToken::F64(2.998e+8))]
        );

        assert_eq!(
            lex_from_str("6.626e-34").unwrap(),
            vec![Token::Number(NumberToken::F64(6.626e-34))]
        );

        // err: unsupports start with dot
        assert!(matches!(lex_from_str(".123"), Err(Error::Message(_))));

        // err: multiple points
        assert!(matches!(lex_from_str("1.23.456"), Err(Error::Message(_))));

        // err: multiple 'e' (exps)
        assert!(matches!(lex_from_str("1e2e3"), Err(Error::Message(_))));

        // err: incomplete floating point number
        assert!(matches!(lex_from_str("123."), Err(Error::Message(_))));

        // err: incomplete 'e'
        assert!(matches!(lex_from_str("123e"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_decimal_number_with_explicit_type() {
        assert_eq!(
            lex_from_str("11i8").unwrap(),
            vec![Token::Number(NumberToken::I8(11))]
        );

        assert_eq!(
            lex_from_str("11_i8").unwrap(),
            vec![Token::Number(NumberToken::I8(11))]
        );

        assert_eq!(
            lex_from_str("11__i8").unwrap(),
            vec![Token::Number(NumberToken::I8(11))]
        );

        // byte
        {
            assert_eq!(
                lex_from_str("127_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(127))]
            );

            assert_eq!(
                lex_from_str("255_u8").unwrap(),
                vec![Token::Number(NumberToken::U8(255))]
            );

            // err: unsigned overflow
            assert!(matches!(lex_from_str("256_u8"), Err(Error::Message(_))));
        }

        // short
        {
            assert_eq!(
                lex_from_str("32767_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(32767))]
            );

            assert_eq!(
                lex_from_str("65535_u16").unwrap(),
                vec![Token::Number(NumberToken::U16(65535))]
            );

            // err: unsigned overflow
            assert!(matches!(lex_from_str("65536_u16"), Err(Error::Message(_))));
        }

        // int
        {
            assert_eq!(
                lex_from_str("2_147_483_647_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(2_147_483_647i32 as u32))]
            );

            assert_eq!(
                lex_from_str("4_294_967_295_u32").unwrap(),
                vec![Token::Number(NumberToken::U32(std::u32::MAX))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("4_294_967_296_u32"),
                Err(Error::Message(_))
            ));
        }

        // long
        {
            assert_eq!(
                lex_from_str("9_223_372_036_854_775_807_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    9_223_372_036_854_775_807i64 as u64
                )),]
            );

            assert_eq!(
                lex_from_str("18_446_744_073_709_551_615_u64").unwrap(),
                vec![Token::Number(NumberToken::U64(std::u64::MAX))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("18_446_744_073_709_551_616_u64"),
                Err(Error::Message(_))
            ));
        }

        // float
        {
            assert_eq!(
                lex_from_str("3.402_823_5e+38_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(3.402_823_5e38f32))]
            );

            assert_eq!(
                lex_from_str("1.175_494_4e-38_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(1.175_494_4e-38f32))]
            );

            // err: overflow
            assert!(matches!(lex_from_str("3.4e39_f32"), Err(Error::Message(_))));
        }

        // double
        {
            assert_eq!(
                lex_from_str("1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(
                    1.797_693_134_862_315_7e308_f64
                )),]
            );

            assert_eq!(
                lex_from_str("2.2250738585072014e-308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(2.2250738585072014e-308f64)),]
            );

            // err: overflow
            assert!(matches!(
                lex_from_str("1.8e309_f64"),
                Err(Error::Message(_))
            ));
        }
    }

    #[test]
    fn test_lex_hex_number() {
        assert_eq!(
            lex_from_str("0xabcd").unwrap(),
            vec![Token::Number(NumberToken::I32(0xabcd))]
        );

        assert_eq!(
            lex_from_str("-0xaabb").unwrap(),
            vec![Token::Minus, Token::Number(NumberToken::I32(0xaabb))]
        );

        assert_eq!(
            lex_from_str("+0xccdd").unwrap(),
            vec![Token::Plus, Token::Number(NumberToken::I32(0xccdd))]
        );

        // err: hex number overflow
        assert!(matches!(
            lex_from_str("0x1_0000_0000"),
            Err(Error::Message(_))
        ));

        // err: invalid char for hex number
        assert!(matches!(lex_from_str("0x1234xyz"), Err(Error::Message(_))));

        // err: incomplete hex number
        assert!(matches!(lex_from_str("0x"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_hex_number_with_explicit_type() {
        assert_eq!(
            lex_from_str("0x7f_i8").unwrap(),
            vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
        );

        assert_eq!(
            lex_from_str("0xff_u8").unwrap(),
            vec![Token::Number(NumberToken::U8(0xff_u8))]
        );

        // err: unsigned overflow
        assert!(matches!(lex_from_str("0x1_ff_u8"), Err(Error::Message(_))));

        assert_eq!(
            lex_from_str("0x7fff_i16").unwrap(),
            vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
        );

        assert_eq!(
            lex_from_str("0xffff_u16").unwrap(),
            vec![Token::Number(NumberToken::U16(0xffff_u16))]
        );

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("0x1_ffff_u16"),
            Err(Error::Message(_))
        ));

        assert_eq!(
            lex_from_str("0x7fff_ffff_i32").unwrap(),
            vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
        );

        assert_eq!(
            lex_from_str("0xffff_ffff_u32").unwrap(),
            vec![Token::Number(NumberToken::U32(0xffff_ffff_u32))]
        );

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("0x1_ffff_ffff_u32"),
            Err(Error::Message(_))
        ));

        assert_eq!(
            lex_from_str("0x7fff_ffff_ffff_ffff_i64").unwrap(),
            vec![Token::Number(NumberToken::I64(
                0x7fff_ffff_ffff_ffff_i64 as u64
            ))]
        );

        assert_eq!(
            lex_from_str("0xffff_ffff_ffff_ffff_u64").unwrap(),
            vec![Token::Number(NumberToken::U64(0xffff_ffff_ffff_ffff_u64))]
        );

        // note: it is not hex floating pointer number
        assert_eq!(
            lex_from_str("0xaa_f32").unwrap(),
            vec![Token::Number(NumberToken::I32(0xaaf32))]
        );

        // note: it is not hex floating pointer number
        assert_eq!(
            lex_from_str("0xbb_f64").unwrap(),
            vec![Token::Number(NumberToken::I32(0xbbf64))]
        );

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("0x1_ffff_ffff_ffff_ffff_u64"),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_hex_number_floating_point() {
        // default type is f64
        assert_eq!(
            lex_from_str("0x1.4p3").unwrap(),
            vec![Token::Number(NumberToken::F64(10f64))]
        );

        // 3.1415927f32
        assert_eq!(
            lex_from_str("0x1.921fb6p1f32").unwrap(),
            vec![Token::Number(NumberToken::F32(std::f32::consts::PI))]
        );

        // 2.718281828459045f64
        assert_eq!(
            lex_from_str("0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![Token::Number(NumberToken::F64(std::f64::consts::E))]
        );

        // https://observablehq.com/@jrus/hexfloat
        assert_eq!(
            lex_from_str("0x1.62e42fefa39efp-1_f64").unwrap(),
            vec![Token::Number(NumberToken::F64(std::f64::consts::LN_2))]
        );

        // err: incorrect number type
        assert!(matches!(
            lex_from_str("0x1.23p4_i32"),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_binary_number() {
        assert_eq!(
            lex_from_str("0b1100").unwrap(),
            vec![Token::Number(NumberToken::I32(0b1100))]
        );

        assert_eq!(
            lex_from_str("-0b1010").unwrap(),
            vec![Token::Minus, Token::Number(NumberToken::I32(0b1010))]
        );

        assert_eq!(
            lex_from_str("+0b0101").unwrap(),
            vec![Token::Plus, Token::Number(NumberToken::I32(0b0101))]
        );

        // err: does not support binary floating point
        assert!(matches!(lex_from_str("0b11.1"), Err(Error::Message(_))));

        // err: binary number overflow
        assert!(matches!(
            lex_from_str("0b1_0000_0000_0000_0000_0000_0000_0000_0000"),
            Err(Error::Message(_))
        ));

        // err: invalid char for binary number
        assert!(matches!(lex_from_str("0b10xyz"), Err(Error::Message(_))));

        // err: incomplete binary number
        assert!(matches!(lex_from_str("0b"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_binary_number_with_explicit_type() {
        // byte
        {
            assert_eq!(
                lex_from_str("0b0111_1111_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                lex_from_str("0b1111_1111_u8").unwrap(),
                vec![Token::Number(NumberToken::U8(0xff_u8))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("0b1_1111_1111_u8"),
                Err(Error::Message(_))
            ));
        }

        // short
        {
            assert_eq!(
                lex_from_str("0b0111_1111_1111_1111_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                lex_from_str("0b1111_1111_1111_1111_u16").unwrap(),
                vec![Token::Number(NumberToken::U16(0xffff_u16))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("0b1_1111_1111_1111_1111_u16"),
                Err(Error::Message(_))
            ));
        }

        // int
        {
            assert_eq!(
                lex_from_str("0b0111_1111_1111_1111__1111_1111_1111_1111_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                lex_from_str("0b1111_1111_1111_1111__1111_1111_1111_1111_u32").unwrap(),
                vec![Token::Number(NumberToken::U32(0xffff_ffff_u32))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("0b1_1111_1111_1111_1111__1111_1111_1111_1111_u32"),
                Err(Error::Message(_))
            ));
        }

        // long
        {
            assert_eq!(
                lex_from_str("0b0111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(0x7fff_ffff_ffff_ffff_i64 as u64))]
            );

            assert_eq!(
                lex_from_str("0b1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_u64").unwrap(),
                vec![Token::Number(NumberToken::U64(0xffff_ffff_ffff_ffff_u64))]
            );

            // err: unsigned overflow
            assert!(matches!(
                lex_from_str("0b1_1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_u64"),
                Err(Error::Message(_))
            ));
        }

        // err: does not support binary floating pointer number
        assert!(matches!(lex_from_str("0b11_f32"), Err(Error::Message(_))));

        // err: does not support binary floating pointer number
        assert!(matches!(lex_from_str("0b11_f64"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_char() {
        assert_eq!(lex_from_str("'a'").unwrap(), vec![Token::Char('a')]);

        assert_eq!(
            lex_from_str("('a')").unwrap(),
            vec![Token::LeftParen, Token::Char('a'), Token::RightParen]
        );

        assert_eq!(
            lex_from_str("'a' 'z'").unwrap(),
            vec![Token::Char('a'), Token::Char('z')]
        );

        // CJK
        assert_eq!(lex_from_str("'文'").unwrap(), vec![Token::Char('文')]);

        // emoji
        assert_eq!(lex_from_str("'😊'").unwrap(), vec![Token::Char('😊')]);

        // escape char `\r`
        assert_eq!(lex_from_str("'\\r'").unwrap(), vec![Token::Char('\r')]);

        // escape char `\n`
        assert_eq!(lex_from_str("'\\n'").unwrap(), vec![Token::Char('\n')]);

        // escape char `\t`
        assert_eq!(lex_from_str("'\\t'").unwrap(), vec![Token::Char('\t')]);

        // escape char `\\`
        assert_eq!(lex_from_str("'\\\\'").unwrap(), vec![Token::Char('\\')]);

        // escape char `\'`
        assert_eq!(lex_from_str("'\\\''").unwrap(), vec![Token::Char('\'')]);

        // escape char `"`
        assert_eq!(lex_from_str("'\\\"'").unwrap(), vec![Token::Char('"')]);

        // escape char `\0`
        assert_eq!(lex_from_str("'\\0'").unwrap(), vec![Token::Char('\0')]);

        // escape char, unicode
        assert_eq!(lex_from_str("'\\u{2d}'").unwrap(), vec![Token::Char('-')]);

        // escape char, unicode
        assert_eq!(
            lex_from_str("'\\u{6587}'").unwrap(),
            vec![Token::Char('文')]
        );

        // err: unsupported escape char \v
        assert!(matches!(lex_from_str("'\\v'"), Err(Error::Message(_))));

        // err: unsupported hex escape "\x.."
        assert!(matches!(lex_from_str("'\\x33'"), Err(Error::Message(_))));

        // err: incomplete escape string
        assert!(matches!(lex_from_str("'a\\'"), Err(Error::Message(_))));

        // err: invalid unicode code point
        assert!(matches!(
            lex_from_str("'\\u{110000}'"),
            Err(Error::Message(_))
        ));

        // err: invalid unicode escape sequence
        assert!(matches!(
            lex_from_str("'\\u{12mn}''"),
            Err(Error::Message(_))
        ));

        // err: missing left brace for unicode escape sequence
        assert!(matches!(lex_from_str("'\\u1234'"), Err(Error::Message(_))));

        // err: missing right brace for unicode escape sequence
        assert!(matches!(lex_from_str("'\\u{1234'"), Err(Error::Message(_))));

        // err: missing right quote
        assert!(matches!(lex_from_str("'a"), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_string() {
        assert_eq!(
            lex_from_str(r#""abc""#).unwrap(),
            vec![Token::new_string("abc")]
        );

        assert_eq!(
            lex_from_str(r#"("abc")"#).unwrap(),
            vec![
                Token::LeftParen,
                Token::new_string("abc"),
                Token::RightParen,
            ]
        );

        assert_eq!(
            lex_from_str(r#""abc" "xyz""#).unwrap(),
            vec![Token::new_string("abc"), Token::new_string("xyz")]
        );

        assert_eq!(
            lex_from_str("\"abc\"\n\n\"xyz\"").unwrap(),
            vec![
                Token::new_string("abc"),
                Token::NewLine,
                Token::NewLine,
                Token::new_string("xyz"),
            ]
        );

        // unicode
        assert_eq!(
            lex_from_str(
                r#"
                "abc文字😊"
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("abc文字😊"),
                Token::NewLine,
            ]
        );

        // empty string
        assert_eq!(lex_from_str("\"\"").unwrap(), vec![Token::new_string("")]);

        // escape chars
        assert_eq!(
            lex_from_str(
                r#"
                "\r\n\t\\\"\'\u{2d}\u{6587}\0"
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("\r\n\t\\\"\'-文\0"),
                Token::NewLine,
            ]
        );

        // err: unsupported escape char \v
        assert!(matches!(
            lex_from_str(
                r#"
                "abc\vxyz"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: unsupported hex escape "\x.."
        assert!(matches!(
            lex_from_str(
                r#"
                "abc\x33xyz"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: incomplete escape string
        assert!(matches!(lex_from_str(r#""abc\"#), Err(Error::Message(_))));

        // err: invalid unicode code point
        assert!(matches!(
            lex_from_str(
                r#"
                "abc\u{110000}xyz"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: invalid unicode escape sequence
        assert!(matches!(
            lex_from_str(
                r#"
                "abc\u{12mn}xyz"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: missing left brace for unicode escape sequence
        assert!(matches!(
            lex_from_str(
                r#"
                "abc\u1234}xyz"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: missing right brace for unicode escape sequence
        assert!(matches!(
            lex_from_str(r#""abc\u{1234"#),
            Err(Error::Message(_))
        ));

        // err: missing right quote
        assert!(matches!(
            lex_from_str(
                r#"
                "abc
                "#
            ),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_multiple_line_string() {
        assert_eq!(
            lex_from_str("\"abc\ndef\n    uvw\r\n\t  \txyz\"").unwrap(),
            vec![Token::new_string("abc\ndef\n    uvw\r\n\t  \txyz")]
        );

        // the tailing '\' should escapes the new-line chars
        assert_eq!(
            lex_from_str("\"abc\\\ndef\\\n    uvw\\\r\n\t  \txyz\"").unwrap(),
            vec![Token::new_string("abcdefuvwxyz")]
        );

        // the tailing '\' should escapes the new-line chars and trim the leading white-spaces
        assert_eq!(
            lex_from_str("\"\\\n  \t  \"").unwrap(),
            vec![Token::new_string("")]
        );

        // err: missing right quote
        assert!(matches!(
            lex_from_str("\"abc\\\n    "),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_law_string() {
        assert_eq!(
            lex_from_str(
                "r\"abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz\""
            )
            .unwrap(),
            vec![Token::new_string(
                "abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz"
            )]
        );

        // err: missing right quote
        assert!(matches!(lex_from_str("r\"abc    "), Err(Error::Message(_))));
    }

    #[test]
    fn test_lex_law_string_with_hash() {
        assert_eq!(
            lex_from_str(
                "r#\"abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz quote: \"foo\"\"#"
            )
                .unwrap(),
            vec![Token::new_string(
                "abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz quote: \"foo\""
            )]
        );

        // err: missing the ending marker
        assert!(matches!(
            lex_from_str("r#\"abc    "),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_auto_trimmed_string() {
        assert_eq!(
            lex_from_str(
                r#"
                """
                one
                  two
                    three
                end
                """
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("one\n  two\n    three\nend"),
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
                """
                one
              two
            three
                end
                """
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("one\ntwo\nthree\nend"),
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
                """
                    one\\\"\t\r\n\u{1234}

                    end
                """
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("one\\\\\\\"\\t\\r\\n\\u{1234}\n\nend"),
                Token::NewLine,
            ]
        );

        // test the ending mark (""") does not start in a new line

        assert_eq!(
            lex_from_str(
                r#"
                """
                    one"""
                    two
                """
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::new_string("one\"\"\"\ntwo"),
                Token::NewLine,
            ]
        );

        // test inline
        assert_eq!(
            lex_from_str(
                r#"
                11 """
                    abc
                """ 13
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(11)),
                Token::new_string("abc"),
                Token::Number(NumberToken::I32(13)),
                Token::NewLine,
            ]
        );

        // err: the content does not start on a new line
        assert!(matches!(
            lex_from_str(
                r#"
                """hello
                """
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: the ending marker does not start on a new line
        assert!(matches!(
            lex_from_str(
                r#"
            """
            hello"""
            "#
            ),
            Err(Error::Message(_))
        ));

        // err: missing the ending marker
        assert!(matches!(
            lex_from_str(
                r#"
                """
                hello
                "#
            ),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_byte_data_hexadecimal() {
        assert_eq!(
            lex_from_str(
                r#"
                h""
                "#
            )
            .unwrap(),
            vec![Token::NewLine, Token::ByteData(vec![]), Token::NewLine]
        );

        assert_eq!(
            lex_from_str(
                r#"
                h"11"
                "#
            )
            .unwrap(),
            vec![Token::NewLine, Token::ByteData(vec![0x11]), Token::NewLine,]
        );

        assert_eq!(
            lex_from_str(
                r#"
                h"11 13 17 19"
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
                Token::NewLine,
            ]
        );

        // assert_eq!(
        //     lex_from_str(
        //         r#"
        //         h"11-13-1719"
        //         "#
        //     )
        //     .unwrap(),
        //     vec![
        //         Token::NewLine,
        //         Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
        //         Token::NewLine,
        //     ]
        // );

        // assert_eq!(
        //     lex_from_str(
        //         r#"
        //         h"11:13:1719"
        //         "#
        //     )
        //     .unwrap(),
        //     vec![
        //         Token::NewLine,
        //         Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
        //         Token::NewLine,
        //     ]
        // );

        assert_eq!(
            lex_from_str(
                "
                h\"  11\t  13\r17\n  19  \"
                "
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
                Token::NewLine,
            ]
        );

        // err: no whitespace
        assert!(matches!(
            lex_from_str(
                r#"
                h"11 13 1719"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: incomplete byte string, the amount of digits should be even
        assert!(matches!(
            lex_from_str(
                r#"
                h"11 13 17 1"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: invalid char for byte string
        assert!(matches!(
            lex_from_str(
                r#"
                h"11 13 17 1z"
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: missing the ending quote
        assert!(matches!(
            lex_from_str(
                r#"
                h"11 13 17 19
                "#
            ),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_line_comment() {
        assert_eq!(
            lex_from_str(
                r#"
                7 //11
                13 17// 19 23
                // 29
                31// 37
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::Comment(Comment::Line("11".to_owned())),
                Token::Number(NumberToken::I32(13)),
                Token::Number(NumberToken::I32(17)),
                Token::Comment(Comment::Line(" 19 23".to_owned())),
                Token::Comment(Comment::Line(" 29".to_owned())),
                Token::Number(NumberToken::I32(31)),
                Token::Comment(Comment::Line(" 37".to_owned())),
                // note that the line comment includes the ending new line chars (\n or \r\n),
                // so there is NO `Token::NewLine` follows the line comment.
            ]
        );
    }

    #[test]
    fn test_lex_block_comment() {
        assert_eq!(
            lex_from_str(
                r#"
                7 /* 11 13 */ 17
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::Comment(Comment::Block(" 11 13 ".to_owned())),
                Token::Number(NumberToken::I32(17)),
                Token::NewLine,
            ]
        );

        // nested block comment
        assert_eq!(
            lex_from_str(
                r#"
                7 /* 11 /* 13 */ 17 */ 19
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::Comment(Comment::Block(" 11 /* 13 */ 17 ".to_owned())),
                Token::Number(NumberToken::I32(19)),
                Token::NewLine,
            ]
        );

        // line comment chars "//" within the block comment
        assert_eq!(
            lex_from_str(
                r#"
                7 /* 11 // 13 17 */ 19
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::Comment(Comment::Block(" 11 // 13 17 ".to_owned())),
                Token::Number(NumberToken::I32(19)),
                Token::NewLine,
            ]
        );

        // document comment chars (""") within the block comment
        assert_eq!(
            lex_from_str(
                r#"
                7 /* 11
                """
                abc
                """
                13 */ 19
                "#
                .lines()
                .map(&str::trim_start)
                .map(&str::to_owned)
                .collect::<Vec<String>>()
                .join("\n")
                .as_str()
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::Comment(Comment::Block(" 11\n\"\"\"\nabc\n\"\"\"\n13 ".to_owned())),
                Token::Number(NumberToken::I32(19)),
                Token::NewLine,
            ]
        );

        // err: unpaired, missing the ending pair
        assert!(matches!(
            lex_from_str(
                r#"
                7 /* 11 /* 13 */ 17
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: unpaired
        assert!(matches!(
            lex_from_str(
                r#"
                7 */ 11
                "#
            ),
            Err(Error::Message(_))
        ));
    }

    #[cfg(feature = "doc_comment")]
    #[test]
    fn test_lex_document_comment() {
        assert_eq!(
            lex_from_str(
                r#"
                /** one two **/
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Comment(Comment::Document(" one two ".to_owned())),
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
/** one ** */ /** /* two
**/
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Comment(Comment::Document(" one ** */ /** /* two\n".to_owned())),
                Token::NewLine
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
/* one **/
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::Comment(Comment::Block(" one *".to_owned())),
                Token::NewLine
            ]
        );

        // err: incorrect ending marker
        assert!(matches!(
            lex_from_str(
                r#"
                /**
                hello
                */
                "#
            ),
            Err(Error::Message(_))
        ));

        // err: missing the ending marker
        assert!(matches!(
            lex_from_str(
                r#"
                /**
                hello
                "#
            ),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_datetime() {
        let expect_date1 = DateTime::parse_from_rfc3339("2024-03-16T16:30:50+08:00").unwrap();
        let expect_date2 = DateTime::parse_from_rfc3339("2024-03-16T16:30:50Z").unwrap();

        assert_eq!(
            lex_from_str("d\"2024-03-16T16:30:50+08:00\"").unwrap(),
            vec![Token::Date(expect_date1)]
        );

        assert_eq!(
            lex_from_str("d\"2024-03-16T16:30:50Z\"").unwrap(),
            vec![Token::Date(expect_date2)]
        );

        assert_eq!(
            lex_from_str("d\"2024-03-16T16:30:50z\"").unwrap(),
            vec![Token::Date(expect_date2)]
        );

        assert_eq!(
            lex_from_str("d\"2024-03-16T16:30:50\"").unwrap(),
            vec![Token::Date(expect_date2)]
        );

        assert_eq!(
            lex_from_str("d\"2024-03-16t16:30:50\"").unwrap(),
            vec![Token::Date(expect_date2)]
        );

        assert_eq!(
            lex_from_str("d\"2024-03-16 16:30:50\"").unwrap(),
            vec![Token::Date(expect_date2)]
        );

        // err: missing time
        assert!(matches!(
            lex_from_str("d\"16:30:50\""),
            Err(Error::Message(_))
        ));

        // err: missing date
        assert!(matches!(
            lex_from_str("d\"2024-03-16\""),
            Err(Error::Message(_))
        ));

        // err: not YYYY-MM-DD HH:mm:ss
        assert!(matches!(
            lex_from_str("d\"2024-3-16 4:30:50\""),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_lex_compositive_tokens() {
        assert_eq!(
            lex_from_str(
                r#"
                {id: 123, name: "foo"}
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::LeftBrace,
                Token::new_identifier("id"),
                Token::Colon,
                Token::Number(NumberToken::I32(123)),
                Token::Comma,
                Token::new_identifier("name"),
                Token::Colon,
                Token::new_string("foo"),
                Token::RightBrace,
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
                [123,456,789,]
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::LeftBracket,
                Token::Number(NumberToken::I32(123)),
                Token::Comma,
                Token::Number(NumberToken::I32(456)),
                Token::Comma,
                Token::Number(NumberToken::I32(789)),
                Token::Comma,
                Token::RightBracket,
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
                (123 "foo" true) // line comment
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::LeftParen,
                Token::Number(NumberToken::I32(123)),
                Token::new_string("foo"),
                Token::Boolean(true),
                // Token::Keyword("true".to_owned()),
                Token::RightParen,
                Token::Comment(Comment::Line(" line comment".to_owned())),
            ]
        );

        assert_eq!(
            lex_from_str(
                r#"
                {
                    a: [1,2,3]
                    b: (false, d"2000-01-01 10:10:10")
                    c: {id: 11}
                }
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::LeftBrace, // {
                Token::NewLine,
                Token::new_identifier("a"),
                Token::Colon,
                Token::LeftBracket, // [
                Token::Number(NumberToken::I32(1)),
                Token::Comma,
                Token::Number(NumberToken::I32(2)),
                Token::Comma,
                Token::Number(NumberToken::I32(3)),
                Token::RightBracket, // ]
                Token::NewLine,
                Token::new_identifier("b"),
                Token::Colon,
                Token::LeftParen, // (
                Token::Boolean(false),
                // Token::Keyword("false".to_owned()),
                Token::Comma,
                Token::Date(DateTime::parse_from_rfc3339("2000-01-01 10:10:10Z").unwrap()),
                Token::RightParen, // )
                Token::NewLine,
                Token::new_identifier("c"),
                Token::Colon,
                Token::LeftBrace, // {
                Token::new_identifier("id"),
                Token::Colon,
                Token::Number(NumberToken::I32(11)),
                Token::RightBrace, // }
                Token::NewLine,
                Token::RightBrace, // }
                Token::NewLine,
            ]
        );
    }

    #[test]
    fn test_normalize_new_lines_and_commas() {
        assert_eq!(
            lex_from_str(
                r#"
                [1,2,

                3

                ,
                // comment
                ,

                ]
                "#
            )
            .unwrap(),
            vec![
                Token::NewLine,
                Token::LeftBracket,
                Token::Number(NumberToken::I32(1)),
                Token::Comma,
                Token::Number(NumberToken::I32(2)),
                Token::Comma,
                Token::NewLine,
                Token::NewLine,
                Token::Number(NumberToken::I32(3)),
                Token::NewLine,
                Token::NewLine,
                Token::Comma,
                Token::NewLine,
                Token::Comment(Comment::Line(" comment".to_owned())),
                Token::Comma,
                Token::NewLine,
                Token::NewLine,
                Token::RightBracket,
                Token::NewLine,
            ]
        );

        assert_eq!(
            lex_and_normalize_from_str(
                r#"
                    [1,2,

                    3

                    ,
                    // comma
                    ,

                    ]
                    "#
            )
            .unwrap(),
            vec![
                Token::LeftBracket,
                Token::Number(NumberToken::I32(1)),
                Token::NewLine,
                Token::Number(NumberToken::I32(2)),
                Token::NewLine,
                Token::Number(NumberToken::I32(3)),
                Token::NewLine,
                Token::RightBracket,
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus() {
        // default int
        {
            assert_eq!(
                lex_and_normalize_from_str("+11").unwrap(),
                vec![Token::Number(NumberToken::I32(11))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-13").unwrap(),
                vec![Token::Number(NumberToken::I32(-13_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("2_147_483_648"),
                Err(Error::Message(_))
            ));

            // err: plus sign is added to non-numbers
            assert!(matches!(
                lex_and_normalize_from_str("+true"),
                Err(Error::Message(_))
            ));

            // err: minus sign is added to non-numbers
            assert!(matches!(
                lex_and_normalize_from_str("-true"),
                Err(Error::Message(_))
            ));
        }

        // byte
        {
            assert_eq!(
                lex_and_normalize_from_str("+127_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(127))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-128_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-128_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("128_i8"),
                Err(Error::Message(_))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_and_normalize_from_str("-129_i8"),
                Err(Error::Message(_))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-1_u8"),
                Err(Error::Message(_))
            ));
        }

        // short
        {
            assert_eq!(
                lex_and_normalize_from_str("+32767_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(32767))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-32768_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-32768_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("32768_i16"),
                Err(Error::Message(_))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_and_normalize_from_str("-32769_i16"),
                Err(Error::Message(_))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-1_u16"),
                Err(Error::Message(_))
            ));
        }

        {
            assert_eq!(
                lex_and_normalize_from_str("+2_147_483_647_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(2_147_483_647i32 as u32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-2_147_483_648_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-2_147_483_648i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("2_147_483_648_i32"),
                Err(Error::Message(_))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_and_normalize_from_str("-2_147_483_649_i32"),
                Err(Error::Message(_))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-1_u32"),
                Err(Error::Message(_))
            ));
        }

        // long
        {
            assert_eq!(
                lex_and_normalize_from_str("+9_223_372_036_854_775_807_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    9_223_372_036_854_775_807i64 as u64
                )),]
            );

            assert_eq!(
                lex_and_normalize_from_str("-9_223_372_036_854_775_808_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -9_223_372_036_854_775_808i64 as u64
                )),]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("9_223_372_036_854_775_808_i64"),
                Err(Error::Message(_))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_and_normalize_from_str("-9_223_372_036_854_775_809_i64"),
                Err(Error::Message(_))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-1_u64"),
                Err(Error::Message(_))
            ));
        }
    }

    // check 'NaN' and 'Inf' also
    #[test]
    fn test_normalize_plus_and_minus_floating_point_number() {
        assert_eq!(
            lex_and_normalize_from_str("+3.402_823_5e+38").unwrap(),
            vec![Token::Number(NumberToken::F64(3.402_823_5e38f64))]
        );

        assert_eq!(
            lex_and_normalize_from_str("-3.402_823_5e+38").unwrap(),
            vec![Token::Number(NumberToken::F64(-3.402_823_5e38f64))]
        );

        // 0
        {
            assert_eq!(
                lex_and_normalize_from_str("0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            // +0 == -0
            assert_eq!(
                lex_and_normalize_from_str("-0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );
        }

        // NaN
        {
            let t = lex_and_normalize_from_str("NaN").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));
        }

        // Inf
        {
            assert_eq!(
                lex_and_normalize_from_str("Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::NEG_INFINITY))]
            );
        }

        // err: +NaN
        assert!(matches!(
            lex_and_normalize_from_str("+NaN"),
            Err(Error::Message(_))
        ));

        // err: -NaN
        assert!(matches!(
            lex_and_normalize_from_str("-NaN"),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_number_with_explicit_type() {
        // single precision
        {
            assert_eq!(
                lex_and_normalize_from_str("+1.602_176_6e-19_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(1.602_176_6e-19f32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-1.602_176_6e-19_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(-1.602_176_6e-19f32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            // +0 == -0
            assert_eq!(
                lex_and_normalize_from_str("-0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            let t = lex_and_normalize_from_str("NaN_f32").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F32(v)) if v.is_nan()));

            assert_eq!(
                lex_and_normalize_from_str("Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::NEG_INFINITY))]
            );

            // err: +NaN
            assert!(matches!(
                lex_and_normalize_from_str("+NaN_f32"),
                Err(Error::Message(_))
            ));

            // err: -NaN
            assert!(matches!(
                lex_and_normalize_from_str("-NaN_f32"),
                Err(Error::Message(_))
            ));
        }

        // double
        {
            assert_eq!(
                lex_and_normalize_from_str("1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(
                    1.797_693_134_862_315_7e308_f64
                )),]
            );

            assert_eq!(
                lex_and_normalize_from_str("-1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(
                    -1.797_693_134_862_315_7e308_f64
                )),]
            );

            assert_eq!(
                lex_and_normalize_from_str("0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            // +0 == -0
            assert_eq!(
                lex_and_normalize_from_str("-0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            let t = lex_and_normalize_from_str("NaN_f64").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));

            assert_eq!(
                lex_and_normalize_from_str("Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("+Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::NEG_INFINITY))]
            );

            // err: +NaN
            assert!(matches!(
                lex_and_normalize_from_str("+NaN_f64"),
                Err(Error::Message(_))
            ));

            // err: -NaN
            assert!(matches!(
                lex_and_normalize_from_str("-NaN_f64"),
                Err(Error::Message(_))
            ));
        }
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_hex_number() {
        // byte
        {
            assert_eq!(
                lex_and_normalize_from_str("+0x7f_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0x80_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0x80_i8"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0x1_u8"),
                Err(Error::Message(_))
            ));
        }

        // short
        {
            assert_eq!(
                lex_and_normalize_from_str("+0x7fff_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0x8000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0x8000_i16"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0x1_u16"),
                Err(Error::Message(_))
            ));
        }

        // int
        {
            assert_eq!(
                lex_and_normalize_from_str("+0x7fff_ffff_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0x8000_0000_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0x8000_0000_i32"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0x1_u32"),
                Err(Error::Message(_))
            ));
        }

        // long
        {
            assert_eq!(
                lex_and_normalize_from_str("+0x7fff_ffff_ffff_ffff_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    0x7fff_ffff_ffff_ffff_i64 as u64
                ))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0x8000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -0x8000_0000_0000_0000_i64 as u64
                ))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0x8000_0000_0000_0000_i64"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0x1_u64"),
                Err(Error::Message(_))
            ));
        }
    }

    #[test]
    fn test_normalize_plus_and_minus_hex_floating_point_number() {
        // 3.1415927f32
        assert_eq!(
            lex_and_normalize_from_str("+0x1.921fb6p1f32").unwrap(),
            vec![Token::Number(NumberToken::F32(std::f32::consts::PI))]
        );

        // -2.718281828459045f64
        assert_eq!(
            lex_and_normalize_from_str("-0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![Token::Number(NumberToken::F64(-std::f64::consts::E))]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_binary_number() {
        // byte
        {
            assert_eq!(
                lex_and_normalize_from_str("0b0111_1111_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0b1000_0000_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0b1000_0000_i8"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0b1_u8"),
                Err(Error::Message(_))
            ));
        }

        // short
        {
            assert_eq!(
                lex_and_normalize_from_str("+0b0111_1111_1111_1111_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0b1000_0000_0000_0000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0b1000_0000_0000_0000_i16"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0b1_u16"),
                Err(Error::Message(_))
            ));
        }

        // int
        {
            assert_eq!(
                lex_and_normalize_from_str("+0b0111_1111_1111_1111__1111_1111_1111_1111_i32")
                    .unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0b1000_0000_0000_0000__0000_0000_0000_0000_i32")
                    .unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0b1_u32"),
                Err(Error::Message(_))
            ));
        }

        // long
        {
            assert_eq!(
                lex_and_normalize_from_str("0b0111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(0x7fff_ffff_ffff_ffff_i64 as u64))]
            );

            assert_eq!(
                lex_and_normalize_from_str("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(-0x8000_0000_0000_0000_i64 as u64))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_and_normalize_from_str("0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64"),
                Err(Error::Message(_))
            ));

            // err: unsigned with minus sign
            assert!(matches!(
                lex_and_normalize_from_str("-0b1_u64"),
                Err(Error::Message(_))
            ));
        }
    }
}
