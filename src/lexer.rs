// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use core::num;
use std::fmt::format;

use crate::{peekable_iterator::PeekableIterator, ParseError};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    LeftParen,    // (
    RightParen,   // )

    NewLine, // \n, \r\n, \r
    Comma,   // ,
    Colon,   // :

    Name(String), // [a-zA-Z0-9_]
    Number(NumberToken),
    Boolean(bool),
    Char(char),
    String_(String),
    Date(String),
    ByteData(Vec<u8>),

    // avaliable in XiaoXuan Lang but not in ASON:
    //
    // - complex numbers:
    //   - 1+2i
    //   - 3i
    //
    // - rational numbers:
    //   - r1/3
    //   - r7/22
    //
    // - bits:
    //   - 4'b1010
    //   - 8'd170 (=8'b1010_1010)
    //   - 16'xaabb
    Comment(CommentToken),
}

// decimal numbers:
// 123
// 123.456
// -123
// 1.23e4
// -1.23e-4
// 123K         // with suffix
// 123Mi        // with 1024-base suffix, equivalent to 123MB
// 123m         // with fractional suffix
// -123n        // with minus sign
// 123u@double  // with fractional suffix and data type
//
// hex numbers:
// 0xabcd
// -0xaabb
//
// binary numbers:
// 0b0011
// -0b1100
//
// hex floating-point numbers:
// 0x1.23p4
// 0x1.23p-4
// -0x1.23
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
#[derive(Debug, PartialEq, Clone)]
pub enum NumberToken {
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

#[derive(Debug, PartialEq, Clone)]
pub enum CommentToken {
    // `// ...`
    Line(String),

    // `/* ... */`
    Block(String),

    // """
    // ...
    // """
    Document(String),
}

pub fn lex(iter: &mut PeekableIterator<char>) -> Result<Vec<Token>, ParseError> {
    let mut tokens: Vec<Token> = vec![];

    while let Some(current_char) = iter.peek(0) {
        match current_char {
            ' ' | '\t' => {
                // white space
                iter.next();
            }
            '\r' => {
                // \r\n or \r
                if iter.look_ahead_equals(1, &'\n') {
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
            '0'..='9' | '-' => {
                // number
                // because there is no operator in ASON, therefor the minus sign '-'
                // can be parsed as partition of number.
                tokens.push(lex_number(iter)?);
            }
            'h' if iter.look_ahead_equals(1, &'"') => {
                // hex byte data
                tokens.push(lex_hex_byte_data(iter)?);
            }
            'd' if iter.look_ahead_equals(1, &'"') => {
                // date
                tokens.push(lex_date(iter)?);
            }
            'r' if iter.look_ahead_equals(1, &'"') => {
                // raw string
                tokens.push(lex_raw_string(iter)?);
            }
            'r' if iter.look_ahead_equals(1, &'#') && iter.look_ahead_equals(2, &'"') => {
                // raw string variant 1
                tokens.push(lex_raw_string_variant(iter)?);
            }
            'r' if iter.look_ahead_equals(1, &'|') && iter.look_ahead_equals(2, &'"') => {
                // raw string variant 2: auto-trimmed string
                tokens.push(lex_auto_trimmed_string(iter)?);
            }
            '"' => {
                if iter.look_ahead_equals(1, &'"') && iter.look_ahead_equals(2, &'"') {
                    // document comment
                    tokens.push(lex_document_comment(iter)?);
                } else {
                    // string
                    tokens.push(lex_string(iter)?);
                }
            }
            '\'' => {
                // char
                tokens.push(lex_char(iter)?);
            }
            '/' if iter.look_ahead_equals(1, &'/') => {
                // line comment
                tokens.push(lex_line_comment(iter)?);
            }
            '/' if iter.look_ahead_equals(1, &'*') => {
                // block comment
                tokens.push(lex_block_comment(iter)?);
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // name or keyword
                tokens.push(lex_name_or_keyword(iter)?);
            }
            '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                // name or keyword
                tokens.push(lex_name_or_keyword(iter)?);
            }
            _ => {
                return Err(ParseError::new(&format!(
                    "Unexpected char: {}",
                    current_char
                )))
            }
        }
    }

    Ok(tokens)
}

fn lex_name_or_keyword(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // key_nameT  //
    // ^       ^__// to here
    // |__________// current char, i.e. the value of 'iter.peek(0)'
    //
    // T = terminator chars

    let mut name_string = String::new();

    while let Some(current_char) = iter.peek(0) {
        match *current_char {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                name_string.push(*current_char);
                iter.next();
            }
            // ':' if iter.look_ahead_equals(1, &':') => {
            //     name_string.push_str("::");
            //     iter.next();
            //     iter.next();
            // }
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
                return Err(ParseError::new(&format!(
                    "Invalid char for key name: {}",
                    *current_char
                )))
            }
        }
    }

    let token = match name_string.as_str() {
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        _ => Token::Name(name_string),
    };

    Ok(token)
}

fn lex_number(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // 123456T  //
    // ^     ^__// to here
    // |________// current char
    //
    //
    // floating-point number
    //
    // 3.14
    // 2.99e8
    // 2.99e+8
    // 6.672e-34
    //
    // the number may also be hex or binary, e.g.
    // 0b1100
    // 0xaabb
    // 0x1.23p4
    // 0x1.23p+4
    // 0x1.23p-4

    let is_negative = if let Some('-') = iter.peek(0) {
        // consume the minus sign '-'
        iter.next();
        true
    } else {
        false
    };

    if iter.look_ahead_equals(0, &'0') && iter.look_ahead_equals(1, &'b') {
        // '0b...'
        lex_number_binary(iter, is_negative)
    } else if iter.look_ahead_equals(0, &'0') && iter.look_ahead_equals(1, &'x') {
        // '0x...'
        lex_number_hex(iter, is_negative)
    } else {
        // '1234'
        // '1.23'
        lex_number_decimal(iter, is_negative)
    }
}

fn lex_number_decimal(
    iter: &mut PeekableIterator<char>,
    is_negative: bool,
) -> Result<Token, ParseError> {
    // 123456T  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    let mut num_string = String::new();
    let mut num_prefix: Option<char> = None;
    let mut num_type: Option<String> = None;

    let mut found_point = false;
    let mut found_e = false;
    let mut found_binary_prefix = false;

    // samples:
    //
    // 123
    // 3.14
    // 2.99e8
    // 2.99e+8
    // 6.672e-34

    while let Some(current_char) = iter.peek(0) {
        match *current_char {
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
                if iter.look_ahead_equals(1, &'-') {
                    num_string.push_str("e-");
                    iter.next();
                    iter.next();
                } else if iter.look_ahead_equals(1, &'+') {
                    num_string.push_str("e+");
                    iter.next();
                    iter.next();
                } else {
                    num_string.push(*current_char);
                    iter.next();
                }
            }
            '@' if num_type.is_none() => {
                num_type.replace(lex_number_type(iter)?);
            }
            'E' | 'P' | 'T' | 'G' | 'M' | 'K' if num_prefix.is_none() => {
                if iter.look_ahead_equals(1, &'i') || iter.look_ahead_equals(1, &'B') {
                    // https://en.wikipedia.org/wiki/Binary_prefix
                    found_binary_prefix = true;
                    num_prefix.replace(*current_char);
                    iter.next();
                    iter.next();
                } else {
                    // https://en.wikipedia.org/wiki/Unit_prefix
                    num_prefix.replace(*current_char);
                    iter.next();
                }
            }
            'm' | 'u' | 'n' | 'p' | 'f' | 'a' if num_prefix.is_none() => {
                num_prefix.replace(*current_char);
                iter.next();
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '/'
            | '\'' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(ParseError::new(&format!(
                    "Invalid char for decimal number: {}",
                    *current_char
                )))
            }
        }
    }

    // convert to primitive numbers

    let has_integer_unit_prefix = |p: Option<char>| -> bool {
        if let Some(c) = p {
            match c {
                'E' | 'P' | 'T' | 'G' | 'M' | 'K' => true,
                _ => false,
            }
        } else {
            false
        }
    };

    let has_fraction_unit_prefix = |p: Option<char>| -> bool {
        if let Some(c) = p {
            match c {
                'm' | 'u' | 'n' | 'p' | 'f' | 'a' => true,
                _ => false,
            }
        } else {
            false
        }
    };

    let get_integer_unit_prefix_value = |p: Option<char>| -> u64 {
        if let Some(c) = p {
            if found_binary_prefix {
                match c {
                    'E' => 2_u64.pow(60),
                    'P' => 2_u64.pow(50),
                    'T' => 2_u64.pow(40),
                    'G' => 2_u64.pow(30),
                    'M' => 2_u64.pow(20),
                    'K' => 2_u64.pow(10),
                    _ => unreachable!(),
                }
            } else {
                match c {
                    'E' => 10_u64.pow(18),
                    'P' => 10_u64.pow(15),
                    'T' => 10_u64.pow(12),
                    'G' => 10_u64.pow(9),
                    'M' => 10_u64.pow(6),
                    'K' => 10_u64.pow(3),
                    _ => unreachable!(),
                }
            }
        } else {
            unreachable!()
        }
    };

    let get_fraction_unit_prefix_value = |p: Option<char>| -> f32 {
        if let Some(c) = p {
            match c {
                'a' => 10_f32.powi(18),
                'f' => 10_f32.powi(15),
                'p' => 10_f32.powi(12),
                'n' => 10_f32.powi(9),
                'u' => 10_f32.powi(6),
                'm' => 10_f32.powi(3),
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    };

    let mut num_token: NumberToken;

    if let Some(ty) = num_type {
        if has_integer_unit_prefix(num_prefix) && ty.as_str() != "ulong" {
            return Err(ParseError::new(&format!("Only \"ulong\" type numbers can contain integer unit prefix, the current number type: {}", ty)));
        }

        if has_fraction_unit_prefix(num_prefix) && ty.as_str() != "float" {
            return Err(ParseError::new(&format!("Only \"float\" type numbers can contain fraction unit prefix, the current number type: {}", ty)));
        }

        match ty.as_str() {
            "byte" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let v = num_string.parse::<i8>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to byte number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::Byte(v);
            }
            "ubyte" => {
                if is_negative {
                    return Err(ParseError::new(
                        "Unsigned number with minus sign is not allowed.",
                    ));
                }

                let v = num_string.parse::<u8>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to unsigned byte number, error: {}",
                        num_string, e
                    ))
                })?;
                num_token = NumberToken::UByte(v);
            }
            "short" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let v = num_string.parse::<i16>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to short integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::Short(v);
            }
            "ushort" => {
                if is_negative {
                    return Err(ParseError::new(
                        "Unsigned number with minus sign is not allowed.",
                    ));
                }

                let v = num_string.parse::<u16>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to unsigned short integer number, error: {}",
                        num_string, e
                    ))
                })?;
                num_token = NumberToken::UShort(v);
            }
            "int" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let v = num_string.parse::<i32>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::Int(v);
            }
            "uint" => {
                if is_negative {
                    return Err(ParseError::new(
                        "Unsigned number with minus sign is not allowed.",
                    ));
                }

                let v = num_string.parse::<u32>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to unsigned integer number, error: {}",
                        num_string, e
                    ))
                })?;
                num_token = NumberToken::UInt(v);
            }
            "long" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let v = num_string.parse::<i64>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to long integer number, error: {}",
                        num_string, e
                    ))
                })?;

                num_token = NumberToken::Long(v);
            }
            "ulong" => {
                if is_negative {
                    return Err(ParseError::new(
                        "Unsigned number with minus sign is not allowed.",
                    ));
                }

                let mut v = num_string.parse::<u64>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to unsigned long integer number, error: {}",
                        num_string, e
                    ))
                })?;

                if has_integer_unit_prefix(num_prefix) {
                    v = v
                        .checked_mul(get_integer_unit_prefix_value(num_prefix))
                        .ok_or(ParseError::new(&format!(
                            "Unsigned long integer number is overflow: {}{}",
                            num_string,
                            num_prefix.unwrap()
                        )))?;
                }

                num_token = NumberToken::ULong(v);
            }
            "float" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let mut v = num_string.parse::<f32>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to floating-point number, error: {}",
                        num_string, e
                    ))
                })?;

                if v.is_infinite() {
                    return Err(ParseError::new("Floating point number overflow."));
                }

                if v.is_nan() {
                    return Err(ParseError::new(
                        "Does not support NaN floating point numbers.",
                    ));
                }

                // note: -0.0 == 0f32 and +0.0 == 0f32
                if is_negative && v == 0f32 {
                    return Err(ParseError::new(
                        "Negative floating-point number 0 is not allowed.",
                    ));
                }

                if has_fraction_unit_prefix(num_prefix) {
                    v = v / get_fraction_unit_prefix_value(num_prefix);
                }

                if is_negative && v == 0f32 {
                    v = 0f32;
                }

                num_token = NumberToken::Float(v);
            }
            "double" => {
                if is_negative {
                    num_string.insert(0, '-');
                }

                let v = num_string.parse::<f64>().map_err(|e| {
                    ParseError::new(&format!(
                        "Can not convert \"{}\" to double precision floating-point number, error: {}",
                        num_string, e
                    ))
                })?;

                if v.is_infinite() {
                    return Err(ParseError::new("Floating point number overflow."));
                }

                if v.is_nan() {
                    return Err(ParseError::new(
                        "Does not support NaN floating point numbers.",
                    ));
                }

                if is_negative && v == 0f64 {
                    return Err(ParseError::new(
                        "Negative floating-point number 0 is not allowed.",
                    ));
                }

                num_token = NumberToken::Double(v);
            }
            _ => {
                unreachable!()
            }
        }
    } else {
        if has_integer_unit_prefix(num_prefix) {
            // u64
            if is_negative {
                return Err(ParseError::new(
                    "Number with both minus sign and unit prefix is not allowed.",
                ));
            }

            let mut v = num_string.parse::<u64>().map_err(|e| {
                ParseError::new(&format!(
                    "Can not convert \"{}\" to unsigned long integer number, error: {}",
                    num_string, e
                ))
            })?;

            v = v
                .checked_mul(get_integer_unit_prefix_value(num_prefix))
                .ok_or(ParseError::new(&format!(
                    "Unsigned long integer number is overflow: {}{}",
                    num_string,
                    num_prefix.unwrap()
                )))?;

            num_token = NumberToken::ULong(v);
        } else if has_fraction_unit_prefix(num_prefix) {
            // f32
            if is_negative {
                num_string.insert(0, '-');
            }

            let mut v = num_string.parse::<f32>().map_err(|e| {
                ParseError::new(&format!(
                    "Can not convert \"{}\" to floating-point number, error: {}",
                    num_string, e
                ))
            })?;

            if v.is_infinite() {
                return Err(ParseError::new("Floating point number overflow."));
            }

            if v.is_nan() {
                return Err(ParseError::new(
                    "Does not support NaN floating point numbers.",
                ));
            }

            // note: -0.0 == 0f32 and +0.0 == 0f32
            if is_negative && v == 0f32 {
                return Err(ParseError::new(
                    "Negative floating-point number 0 is not allowed.",
                ));
            }

            v = v / get_fraction_unit_prefix_value(num_prefix);

            if is_negative && v == 0f32 {
                v = 0f32;
            }

            num_token = NumberToken::Float(v);
        } else if found_point || found_e {
            // f32
            if is_negative {
                num_string.insert(0, '-');
            }

            let v = num_string.parse::<f32>().map_err(|e| {
                ParseError::new(&format!(
                    "Can not convert \"{}\" to floating-point number, error: {}",
                    num_string, e
                ))
            })?;

            if v.is_infinite() {
                return Err(ParseError::new("Floating point number overflow."));
            }

            if v.is_nan() {
                return Err(ParseError::new(
                    "Does not support NaN floating point numbers.",
                ));
            }

            if is_negative && v == 0f32 {
                return Err(ParseError::new(
                    "Negative floating-point number 0 is not allowed.",
                ));
            }

            num_token = NumberToken::Float(v);
        } else {
            // the default number data type is i32
            if is_negative {
                num_string.insert(0, '-');
            }

            let v = num_string.parse::<i32>().map_err(|e| {
                ParseError::new(&format!(
                    "Can not convert \"{}\" to integer number, error: {}",
                    num_string, e
                ))
            })?;

            num_token = NumberToken::Int(v);
        }
    }

    Ok(Token::Number(num_token))
}

fn lex_number_type(iter: &mut PeekableIterator<char>) -> Result<String, ParseError> {
    // @floatT  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    iter.next(); // consume the char '@'

    let mut num_type = String::new();

    while let Some(current_char) = iter.peek(0) {
        match *current_char {
            'a'..='z' => {
                // valid char for type name
                num_type.push(*current_char);
                iter.next();
            }
            _ => {
                break;
            }
        }
    }

    match num_type.as_str() {
        "int" | "uint" | "long" | "ulong" | "byte" | "ubyte" | "short" | "ushort" | "float"
        | "double" => Ok(num_type),
        _ => Err(ParseError::new(&format!(
            "Invalid number type: {}",
            num_type
        ))),
    }
}

fn lex_number_binary(iter: &mut PeekableIterator<char>, is_neg: bool) -> Result<Token, ParseError> {
    // 0b1010T  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    // consume '0b'
    iter.next();
    iter.next();

    let mut num_string = String::new();

    if is_neg {
        num_string.push('-');
    }

    while let Some(current_char) = iter.peek(0) {
        match *current_char {
            '0' | '1' | '_' => {
                // valid digits for binary number
                num_string.push(*current_char);
                iter.next();
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '/' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(ParseError::new(&format!(
                    "Invalid char for binary number: {}",
                    *current_char
                )))
            }
        }
    }

    if num_string.is_empty() {
        Err(ParseError::new("Incomplete binary number"))
    } else {
        // Ok(Token::Number(NumberToken::Binary(num_string)))
        // todo
        Ok(Token::Number(NumberToken::Int(0)))
    }
}

fn lex_number_hex(iter: &mut PeekableIterator<char>, is_neg: bool) -> Result<Token, ParseError> {
    // 0xaabbT  //
    // ^     ^__// to here
    // |________// current char
    //
    // T = terminator chars

    // consume '0x'
    iter.next();
    iter.next();

    let mut is_floating_point_number: bool = false;
    let mut num_string = String::new();

    while let Some(current_char) = iter.peek(0) {
        match *current_char {
            '0'..='9' | 'a'..='f' | 'A'..='F' | '_' => {
                // valid digits for hex number
                num_string.push(*current_char);
                iter.next();
            }
            '.' | 'p' => {
                // it is hex floating point literal
                is_floating_point_number = true;
                num_string.push(*current_char);
                iter.next();
            }
            '+' | '-' if is_floating_point_number => {
                num_string.push(*current_char);
                iter.next();
            }
            ' ' | '\t' | '\r' | '\n' | '(' | ')' | '/' | '"' => {
                // terminator chars
                break;
            }
            _ => {
                return Err(ParseError::new(&format!(
                    "Invalid char for hexadecimal number: {}",
                    *current_char
                )))
            }
        }
    }

    // todo
    // if num_string.is_empty() {
    //     Err(ParseError::new("Incomplete hex number"))
    // } else {
    //     #[allow(clippy::collapsible_else_if)]
    //     let num = if is_floating_point_number {
    //         if is_neg {
    //             NumberToken::HexFloat(format!("-0x{}", num_string))
    //         } else {
    //             NumberToken::HexFloat(format!("0x{}", num_string))
    //         }
    //     } else {
    //         if is_neg {
    //             NumberToken::Hex(format!("-{}", num_string))
    //         } else {
    //             NumberToken::Hex(num_string)
    //         }
    //     };
    //     Ok(Token::Number(num))
    // }

    Ok(Token::Number(NumberToken::Int(0)))
}

fn lex_string(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // "abc"?  //
    // ^    ^__// to here
    // |_______// current char

    iter.next(); // consume the quote

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
                                    ss.push(lex_string_unescape_unicode(iter)?);
                                }
                                '\n' => {
                                    // multiple-line string
                                    let _ = consume_leading_whitespaces(iter)?;
                                }
                                '\r' if iter.look_ahead_equals(0, &'\n') => {
                                    // multiple-line string
                                    iter.next();
                                    let _ = consume_leading_whitespaces(iter)?;
                                }
                                _ => {
                                    return Err(ParseError::new(&format!(
                                        "Unsupported escape char: \"{}\"",
                                        current_char
                                    )))
                                }
                            }
                        }
                        None => return Err(ParseError::new("Incomplete escape char.")),
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
            None => return Err(ParseError::new("Missing end quote for string.")),
        }
    }

    Ok(Token::String_(ss))
}

// return the amount of leading whitespaces
fn consume_leading_whitespaces(iter: &mut PeekableIterator<char>) -> Result<usize, ParseError> {
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
            None => return Err(ParseError::new("Expect the string content.")),
            _ => break,
        }
    }

    Ok(count)
}

fn skip_leading_whitespaces(iter: &mut PeekableIterator<char>, whitespaces: usize) {
    for _ in 0..whitespaces {
        match iter.peek(0) {
            Some(next_char) if next_char == &' ' || next_char == &'\t' => {
                iter.next();
            }
            _ => break,
        }
    }
}

fn lex_string_unescape_unicode(iter: &mut PeekableIterator<char>) -> Result<char, ParseError> {
    // \u{6587}?  //
    //   ^     ^__// to here
    //   |________// current char

    // comsume char '{'
    if !matches!(iter.next(), Some(c) if c == '{') {
        return Err(ParseError::new(
            "Missing left brace for unicode escape sequence.",
        ));
    }

    let mut codepoint_string = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '}' => break,
                '0'..='9' | 'a'..='f' | 'A'..='F' => codepoint_string.push(previous_char),
                _ => {
                    return Err(ParseError::new(&format!(
                        "Invalid character for unicode escape sequence: {}",
                        previous_char
                    )))
                }
            },
            None => {
                return Err(ParseError::new(
                    "Missing right brace for unicode escape sequence.",
                ))
            }
        }

        if codepoint_string.len() > 5 {
            return Err(ParseError::new(
                "The value of unicode point code is to large.",
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
        Err(ParseError::new("Invalid unicode code point."))
    }
}

fn lex_raw_string(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // r"abc"?  //
    // ^     ^__// to here
    // |________// current char

    iter.next(); // consume char 'r'
    iter.next(); // consume the quote

    let mut ss = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '"' => {
                    // end of the string
                    break;
                }
                _ => {
                    // ordinary char
                    ss.push(previous_char);
                }
            },
            None => return Err(ParseError::new("Missing end quote for string.")),
        }
    }

    Ok(Token::String_(ss))
}

fn lex_raw_string_variant(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // r#"abc"#?  //
    // ^       ^__// to here
    // |__________// current char

    iter.next(); // consume char 'r'
    iter.next(); // consume the hash
    iter.next(); // consume the quote

    let mut ss = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                '"' if iter.look_ahead_equals(0, &'#') => {
                    // end of the string
                    iter.next(); // consume the hash
                    break;
                }
                _ => {
                    // ordinary char
                    ss.push(previous_char);
                }
            },
            None => return Err(ParseError::new("Missing end quote for string.")),
        }
    }

    Ok(Token::String_(ss))
}

fn lex_auto_trimmed_string(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // r|"                    //
    // ^  auto-trimmed string //
    // |  "|\n?               //
    // |      ^_______________// to here ('?' = any chars or EOF)
    // |______________________// current char

    iter.next(); // consume char r
    iter.next(); // consume char |
    iter.next(); // consume char "

    if iter.look_ahead_equals(0, &'\n') {
        iter.next();
    } else if iter.look_ahead_equals(0, &'\r') && iter.look_ahead_equals(1, &'\n') {
        iter.next();
        iter.next();
    } else {
        return Err(ParseError::new(
            "The content of auto-trimmed string should start on a new line.",
        ));
    }

    let leading_whitespaces = consume_leading_whitespaces(iter)?;
    let mut ss = String::new();
    let mut line_leading = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => {
                match previous_char {
                    '\n' => {
                        ss.push('\n');
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespaces);
                    }
                    '\r' if iter.look_ahead_equals(0, &'\n') => {
                        iter.next(); // consume '\n'

                        ss.push_str("\r\n");
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespaces);
                    }
                    '"' if line_leading.trim().is_empty() && iter.look_ahead_equals(0, &'|') => {
                        iter.next(); // consume '|'
                        break;
                    }
                    _ => {
                        ss.push(previous_char);
                        line_leading.push(previous_char);
                    }
                }
            }
            None => {
                return Err(ParseError::new(
                    "Missing the ending marker for the auto-trimmed string.",
                ))
            }
        }
    }

    Ok(Token::String_(ss.trim_end().to_owned()))
}

fn lex_document_comment(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // """                  //
    // ^  document comment  //
    // |  """\n?            //
    // |       ^____________// to here ('?' = any chars or EOF)
    // |____________________// current char

    // consume 3 chars (""")
    iter.next();
    iter.next();
    iter.next();

    if iter.look_ahead_equals(0, &'\n') {
        iter.next();
    } else if iter.look_ahead_equals(0, &'\r') && iter.look_ahead_equals(1, &'\n') {
        iter.next();
        iter.next();
    } else {
        return Err(ParseError::new(
            "The content of document comment should start on a new line.",
        ));
    }

    let leading_whitespaces = consume_leading_whitespaces(iter)?;
    let mut ss = String::new();
    let mut line_leading = String::new();

    loop {
        match iter.next() {
            Some(previous_char) => {
                match previous_char {
                    '\n' => {
                        ss.push('\n');
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespaces);
                    }
                    '\r' if iter.look_ahead_equals(0, &'\n') => {
                        iter.next(); // consume '\n'

                        ss.push_str("\r\n");
                        line_leading.clear();
                        skip_leading_whitespaces(iter, leading_whitespaces);
                    }
                    '"' if line_leading.trim().is_empty()
                        && iter.look_ahead_equals(0, &'"')
                        && iter.look_ahead_equals(1, &'"') =>
                    {
                        iter.next(); // consume '"'
                        iter.next(); // consume '"'

                        // only (""") which occupies a single line, is considered to be
                        // the ending mark of a paragraph string.
                        // note that the ending marker includes the new line symbol (\n or \r\n),
                        // i.e., the ("""\n) or ("""\r\n), so there is NO `Token::NewLine` follows
                        // the ending marker.
                        if iter.look_ahead_equals(0, &'\n') {
                            iter.next();
                            break;
                        } else if iter.look_ahead_equals(0, &'\r')
                            && iter.look_ahead_equals(1, &'\n')
                        {
                            iter.next();
                            iter.next();
                            break;
                        } else {
                            // it's not a valid ending mark.
                            ss.push_str("\"\"\"");
                        }
                    }
                    _ => {
                        ss.push(previous_char);
                        line_leading.push(previous_char);
                    }
                }
            }
            None => {
                return Err(ParseError::new(
                    "Missing the ending marker for the paragraph string.",
                ))
            }
        }
    }

    Ok(Token::Comment(CommentToken::Document(
        ss.trim_end().to_owned(),
    )))
}

fn lex_hex_byte_data(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // h"0011aabb"?  //
    // ^          ^__// to here
    // |_____________// current char

    let mut bytes: Vec<u8> = Vec::new();
    let mut byte_buf = String::with_capacity(2);

    iter.next(); // consume char 'h'
    iter.next(); // consume quote '"'

    loop {
        match iter.next() {
            Some(previous_char) => {
                match previous_char {
                    ' ' | '\t' | '\r' | '\n' | '-' | ':' => {
                        // ignore the separator and whitespace chars
                    }
                    '"' => {
                        if !byte_buf.is_empty() {
                            return Err(ParseError::new("Incomplete byte string."));
                        } else {
                            break;
                        }
                    }
                    'a'..='f' | 'A'..='F' | '0'..='9' => {
                        byte_buf.push(previous_char);

                        if byte_buf.len() == 2 {
                            let byte = u8::from_str_radix(&byte_buf, 16).unwrap();
                            bytes.push(byte);
                            byte_buf.clear();
                        }
                    }
                    _ => {
                        return Err(ParseError::new(&format!(
                            "Invalid char for byte string: {}",
                            previous_char
                        )))
                    }
                }
            }
            None => return Err(ParseError::new("Missing end quote for byte string.")),
        }
    }

    Ok(Token::ByteData(bytes))
}

fn lex_date(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    todo!()
}

fn lex_char(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    todo!()
}

fn lex_line_comment(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // xx...[\r]\n?  //
    // ^          ^__// to here ('?' = any char or EOF)
    // |_____________// current char
    //
    // x = '/'

    iter.next(); // consume char '/'
    iter.next(); // consume char '/'

    let mut ss = String::new();

    while let Some(previous_char) = iter.next() {
        // ignore all chars except '\n' or '\r\n'
        // note that the line comment includes the ending new line symbol (\n or \r\n),
        // so there is NO `Token::NewLine` follows the line comment.

        if previous_char == '\n' {
            break;
        } else if previous_char == '\r' && iter.look_ahead_equals(0, &'\n') {
            iter.next(); // consume char '\n'
            break;
        }

        ss.push(previous_char);
    }

    Ok(Token::Comment(CommentToken::Line(ss)))
}

fn lex_block_comment(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
    // x*...*x?  //
    // ^      ^__// to here
    // |_________// current char
    //
    // x = '/'

    iter.next(); // consume char '/'
    iter.next(); // consume char '*'

    let mut ss = String::new();
    let mut pairs = 1;

    loop {
        match iter.next() {
            Some(previous_char) => match previous_char {
                // '(' if iter.look_ahead_equals(0, &';') => {
                '/' if iter.look_ahead_equals(0, &'*') => {
                    // nested block comment
                    // ss.push_str("(;");
                    ss.push_str("/*");
                    iter.next();
                    pairs += 1;
                }
                // ';' if iter.look_ahead_equals(0, &')') => {
                '*' if iter.look_ahead_equals(0, &'/') => {
                    iter.next();
                    pairs -= 1;

                    // check pairs
                    if pairs == 0 {
                        break;
                    } else {
                        // ss.push_str(";)");
                        ss.push_str("*/");
                    }
                }
                _ => {
                    // ignore all chars except "(;" and ";)"
                    // note that line comments within block comments are ignored.
                    ss.push(previous_char);
                }
            },
            None => return Err(ParseError::new("Incomplete block comment.")),
        }
    }

    Ok(Token::Comment(CommentToken::Block(ss)))
}

// fn lex_symbol(iter: &mut PeekableIterator<char>) -> Result<Token, ParseError> {
//     // localT  //
//     // ^    ^__// to here
//     // |_______// current char
//     //
//     // T = terminator chars
//
//     let mut sym_string = String::new();
//
//     while let Some(current_char) = iter.peek(0) {
//         match *current_char {
//             '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
//                 sym_string.push(*current_char);
//                 iter.next();
//             }
//             '.' => {
//                 // e.g. 'i32.add'
//                 sym_string.push(*current_char);
//                 iter.next();
//             }
//             '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
//                 sym_string.push(*current_char);
//                 iter.next();
//             }
//             ' ' | '\t' | '\r' | '\n' | '(' | ')' | '/' | '"' => {
//                 // terminator chars
//                 break;
//             }
//             _ => {
//                 return Err(ParseError::new(&format!(
//                     "Invalid char for symbol: {}",
//                     *current_char
//                 )))
//             }
//         }
//     }
//
//     Ok(Token::Symbol(sym_string))
// }

impl Token {
    pub fn new_name(s: &str) -> Self {
        Token::Name(s.to_owned())
    }

    pub fn new_string(s: &str) -> Self {
        Token::String_(s.to_owned())
    }

    pub fn new_date(s: &str) -> Self {
        Token::Date(s.to_owned())
    }

    pub fn new_byte_data(slice: &[u8]) -> Self {
        Token::ByteData(slice.to_vec())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        lexer::{CommentToken, NumberToken, Token},
        peekable_iterator::PeekableIterator,
        ParseError,
    };

    use super::lex;

    fn lex_from_str(s: &str) -> Result<Vec<Token>, ParseError> {
        let mut chars = s.chars();
        let mut iter = PeekableIterator::new(&mut chars, 3);
        lex(&mut iter)
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
                Token::RightParen
            ]
        );
    }

    #[test]
    fn test_symbols() {
        assert_eq!(
            lex_from_str(":{[],()}").unwrap(),
            vec![
                Token::Colon,
                Token::LeftBrace,
                Token::LeftBracket,
                Token::RightBracket,
                Token::Comma,
                Token::LeftParen,
                Token::RightParen,
                Token::RightBrace
            ]
        );
    }

    #[test]
    fn test_lex_key_name() {
        assert_eq!(lex_from_str("name").unwrap(), vec![Token::new_name("name")]);

        assert_eq!(
            lex_from_str("(name)").unwrap(),
            vec![Token::LeftParen, Token::new_name("name"), Token::RightParen]
        );

        assert_eq!(
            lex_from_str("( a )").unwrap(),
            vec![Token::LeftParen, Token::new_name("a"), Token::RightParen]
        );

        assert_eq!(
            lex_from_str("a__b__c").unwrap(),
            vec![Token::new_name("a__b__c"),]
        );

        assert_eq!(
            lex_from_str("foo bar").unwrap(),
            vec![Token::new_name("foo"), Token::new_name("bar"),]
        );

        assert_eq!(
            lex_from_str("αβγ 文字 🍞🥛").unwrap(),
            vec![
                Token::new_name("αβγ"),
                Token::new_name("文字"),
                Token::new_name("🍞🥛"),
            ]
        );

        // err: starts with number
        assert!(matches!(
            lex_from_str("1abc"),
            Err(ParseError { message: _ })
        ));

        // err: invalid char
        assert!(matches!(
            lex_from_str("abc&xyz"),
            Err(ParseError { message: _ })
        ));
    }

    #[test]
    fn test_lex_keyword() {
        assert_eq!(lex_from_str("true").unwrap(), vec![Token::Boolean(true)]);

        assert_eq!(lex_from_str("false").unwrap(), vec![Token::Boolean(false)]);

        assert_eq!(
            lex_from_str("true false").unwrap(),
            vec![Token::Boolean(true), Token::Boolean(false)]
        );
    }

    #[test]
    fn test_lex_decimal_number() {
        assert_eq!(
            lex_from_str("(211)").unwrap(),
            vec![
                Token::LeftParen,
                Token::Number(NumberToken::Int(211)),
                Token::RightParen
            ]
        );

        assert_eq!(
            lex_from_str("211").unwrap(),
            vec![Token::Number(NumberToken::Int(211)),]
        );

        assert_eq!(
            lex_from_str("-2017").unwrap(),
            vec![Token::Number(NumberToken::Int(-2017)),]
        );

        assert_eq!(
            lex_from_str("223_211").unwrap(),
            vec![Token::Number(NumberToken::Int(223_211))]
        );

        assert_eq!(
            lex_from_str("223 211").unwrap(),
            vec![
                Token::Number(NumberToken::Int(223)),
                Token::Number(NumberToken::Int(211))
            ]
        );

        assert_eq!(
            lex_from_str("3.14").unwrap(),
            vec![Token::Number(NumberToken::Float(3.14))]
        );

        assert_eq!(
            lex_from_str("-2.718").unwrap(),
            vec![Token::Number(NumberToken::Float(-2.718))]
        );

        assert_eq!(
            lex_from_str("123.").unwrap(),
            vec![Token::Number(NumberToken::Float(123f32))]
        );

        assert_eq!(
            lex_from_str("2.998e8").unwrap(),
            vec![Token::Number(NumberToken::Float(2.998e8))]
        );

        assert_eq!(
            lex_from_str("2.998e+8").unwrap(),
            vec![Token::Number(NumberToken::Float(2.998e+8))]
        );

        assert_eq!(
            lex_from_str("6.626e-34").unwrap(),
            vec![Token::Number(NumberToken::Float(6.626e-34))]
        );

        // err: invalid char for decimal number
        assert!(matches!(
            lex_from_str("123XYZ"),
            Err(ParseError { message: _ })
        ));

        // err: unsupports plus sign
        assert!(matches!(
            lex_from_str("+123456"),
            Err(ParseError { message: _ })
        ));

        // err: unsupports start with dot
        assert!(matches!(
            lex_from_str(".123"),
            Err(ParseError { message: _ })
        ));

        // err: multiple points
        assert!(matches!(
            lex_from_str("1.23.456"),
            Err(ParseError { message: _ })
        ));

        // err: multiple 'e' (exps)
        assert!(matches!(
            lex_from_str("1e2e3"),
            Err(ParseError { message: _ })
        ));

        // err: incomplete 'e'
        assert!(matches!(
            lex_from_str("123e"),
            Err(ParseError { message: _ })
        ));

        //         // 3.1415927f32
        //         assert_eq!(
        //             lex_from_str("0x1.921fb6p1").unwrap(),
        //             vec![Token::new_hex_float_number("0x1.921fb6p1")]
        //         );
        //
        //         // 2.718281828459045f64
        //         assert_eq!(
        //             lex_from_str("0x1.5bf0a8b145769p+1").unwrap(),
        //             vec![Token::new_hex_float_number("0x1.5bf0a8b145769p+1")]
        //         );
        //         assert_eq!(
        //             lex_from_str("0x1.23p-4").unwrap(),
        //             vec![Token::new_hex_float_number("0x1.23p-4")]
        //         );
        //

        //
        //         assert_eq!(
        //             lex_from_str("0x1234abcd").unwrap(),
        //             vec![Token::new_hex_number("1234abcd")]
        //         );
        //
        //         assert_eq!(
        //             lex_from_str("0b00110101").unwrap(),
        //             vec![Token::new_bin_number("00110101")]
        //         );
        //
        //         assert_eq!(
        //             lex_from_str("11 0x11 0b11").unwrap(),
        //             vec![
        //                 Token::new_dec_number("11"),
        //                 Token::new_hex_number("11"),
        //                 Token::new_bin_number("11")
        //             ]
        //         );
        //
        //         assert_eq!(
        //             lex_from_str("-0xaabb").unwrap(),
        //             vec![Token::new_hex_number("-aabb")]
        //         );
        //
        //         assert_eq!(
        //             lex_from_str("-0b1010").unwrap(),
        //             vec![Token::new_bin_number("-1010")]
        //         );
        //
        //         assert_eq!(
        //             lex_from_str("-0x1.921fb6p1").unwrap(),
        //             vec![Token::new_hex_float_number("-0x1.921fb6p1")]
        //         );

        //
        //         // err: incomplete hex number
        //         assert!(matches!(lex_from_str("0x"), Err(ParseError { message: _ })));
        //
        //         // err: invalid char for hex number
        //         assert!(matches!(
        //             lex_from_str("0x123xyz"),
        //             Err(ParseError { message: _ })
        //         ));
        //
        //         // err: incomplete binary number
        //         assert!(matches!(lex_from_str("0b"), Err(ParseError { message: _ })));
        //
        //         // err: invalid char for binary number
        //         assert!(matches!(
        //             lex_from_str("0b1234"),
        //             Err(ParseError { message: _ })
        //         ));
        //
        //         // err: unsupported binary number expression
        //         assert!(matches!(
        //             lex_from_str("0b00_11.0101"),
        //             Err(ParseError { message: _ })
        //         ));
    }

    #[test]
    fn test_lex_decimal_number_with_type() {
        assert_eq!(
            lex_from_str("127@byte").unwrap(),
            vec![Token::Number(NumberToken::Byte(127)),]
        );

        assert_eq!(
            lex_from_str("-128@byte").unwrap(),
            vec![Token::Number(NumberToken::Byte(-128)),]
        );

        assert_eq!(
            lex_from_str("255@ubyte").unwrap(),
            vec![Token::Number(NumberToken::UByte(255)),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("128@byte"),
            Err(ParseError { message: _ })
        ));

        // err: negative overflow
        assert!(matches!(
            lex_from_str("-129@byte"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("256@ubyte"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned number with minus sign
        assert!(matches!(
            lex_from_str("-1@ubyte"),
            Err(ParseError { message: _ })
        ));

        assert_eq!(
            lex_from_str("32767@short").unwrap(),
            vec![Token::Number(NumberToken::Short(32767)),]
        );

        assert_eq!(
            lex_from_str("-32768@short").unwrap(),
            vec![Token::Number(NumberToken::Short(-32768)),]
        );

        assert_eq!(
            lex_from_str("65535@ushort").unwrap(),
            vec![Token::Number(NumberToken::UShort(65535)),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("32768@short"),
            Err(ParseError { message: _ })
        ));

        // err: negative overflow
        assert!(matches!(
            lex_from_str("-32769@short"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("65536@ushort"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned number with minus sign
        assert!(matches!(
            lex_from_str("-1@ushort"),
            Err(ParseError { message: _ })
        ));

        assert_eq!(
            lex_from_str("2_147_483_647@int").unwrap(),
            vec![Token::Number(NumberToken::Int(2_147_483_647i32)),]
        );

        assert_eq!(
            lex_from_str("-2_147_483_648@int").unwrap(),
            vec![Token::Number(NumberToken::Int(-2_147_483_648i32)),]
        );

        assert_eq!(
            lex_from_str("4_294_967_295@uint").unwrap(),
            vec![Token::Number(NumberToken::UInt(std::u32::MAX)),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("2_147_483_648@int"),
            Err(ParseError { message: _ })
        ));

        // err: negative overflow
        assert!(matches!(
            lex_from_str("-2_147_483_649@int"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("4_294_967_296@uint"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned number with minus sign
        assert!(matches!(
            lex_from_str("-1@uint"),
            Err(ParseError { message: _ })
        ));

        assert_eq!(
            lex_from_str("9_223_372_036_854_775_807@long").unwrap(),
            vec![Token::Number(NumberToken::Long(
                9_223_372_036_854_775_807i64
            )),]
        );

        assert_eq!(
            lex_from_str("-9_223_372_036_854_775_808@long").unwrap(),
            vec![Token::Number(NumberToken::Long(
                -9_223_372_036_854_775_808i64
            )),]
        );

        assert_eq!(
            lex_from_str("18_446_744_073_709_551_615@ulong").unwrap(),
            vec![Token::Number(NumberToken::ULong(std::u64::MAX)),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("9_223_372_036_854_775_808@long"),
            Err(ParseError { message: _ })
        ));

        // err: negative overflow
        assert!(matches!(
            lex_from_str("-9_223_372_036_854_775_809@long"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned overflow
        assert!(matches!(
            lex_from_str("18_446_744_073_709_551_616@ulong"),
            Err(ParseError { message: _ })
        ));

        // err: unsigned number with minus sign
        assert!(matches!(
            lex_from_str("-1@ulong"),
            Err(ParseError { message: _ })
        ));

        assert_eq!(
            lex_from_str("3.40282347e+38@float").unwrap(),
            vec![Token::Number(NumberToken::Float(3.40282347e+38f32)),]
        );

        assert_eq!(
            lex_from_str("-3.40282347e+38@float").unwrap(),
            vec![Token::Number(NumberToken::Float(-3.40282347e+38f32)),]
        );

        assert_eq!(
            lex_from_str("1.17549435e-38@float").unwrap(),
            vec![Token::Number(NumberToken::Float(1.17549435e-38f32)),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("3.4e39@float"),
            Err(ParseError { message: _ })
        ));

        // err: -0.0
        assert!(matches!(
            lex_from_str("-0@float"),
            Err(ParseError { message: _ })
        ));

        // err: NaN
        assert!(matches!(
            lex_from_str("NaN@float"),
            Err(ParseError { message: _ })
        ));

        // err: +Inf
        assert!(matches!(
            lex_from_str("+Inf@float"),
            Err(ParseError { message: _ })
        ));

        // err: -Inf
        assert!(matches!(
            lex_from_str("-Inf@float"),
            Err(ParseError { message: _ })
        ));

        assert_eq!(
            lex_from_str("1.7976931348623157e+308@double").unwrap(),
            vec![Token::Number(NumberToken::Double(
                1.7976931348623157e+308f64
            )),]
        );

        assert_eq!(
            lex_from_str("-1.7976931348623157e+308@double").unwrap(),
            vec![Token::Number(NumberToken::Double(
                -1.7976931348623157e+308f64
            )),]
        );

        assert_eq!(
            lex_from_str("2.2250738585072014e-308@double").unwrap(),
            vec![Token::Number(NumberToken::Double(
                2.2250738585072014e-308f64
            )),]
        );

        // err: overflow
        assert!(matches!(
            lex_from_str("1.8e309@double"),
            Err(ParseError { message: _ })
        ));

        // err: -0.0
        assert!(matches!(
            lex_from_str("-0@double"),
            Err(ParseError { message: _ })
        ));

        // err: NaN
        assert!(matches!(
            lex_from_str("NaN@double"),
            Err(ParseError { message: _ })
        ));

        // err: +Inf
        assert!(matches!(
            lex_from_str("+Inf@double"),
            Err(ParseError { message: _ })
        ));

        // err: -Inf
        assert!(matches!(
            lex_from_str("-Inf@double"),
            Err(ParseError { message: _ })
        ));
    }

    #[test]
    fn test_lex_decimal_number_with_metric_prefix() {
        assert_eq!(
            lex_from_str("1K").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(3))),]
        );

        assert_eq!(
            lex_from_str("1M").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(6))),]
        );

        assert_eq!(
            lex_from_str("1G").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(9))),]
        );

        assert_eq!(
            lex_from_str("1T").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(12))),]
        );

        assert_eq!(
            lex_from_str("1P").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(15))),]
        );

        assert_eq!(
            lex_from_str("1E").unwrap(),
            vec![Token::Number(NumberToken::ULong(10_u64.pow(18))),]
        );

        // binary prefix

        assert_eq!(
            lex_from_str("1Ki").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(10))),]
        );

        assert_eq!(
            lex_from_str("1Mi").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(20))),]
        );

        assert_eq!(
            lex_from_str("1Gi").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(30))),]
        );

        assert_eq!(
            lex_from_str("1Ti").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(40))),]
        );

        assert_eq!(
            lex_from_str("1Pi").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(50))),]
        );

        assert_eq!(
            lex_from_str("1Ei").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(60))),]
        );

        // alternative binary prefix

        assert_eq!(
            lex_from_str("1KB").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(10))),]
        );

        assert_eq!(
            lex_from_str("1MB").unwrap(),
            vec![Token::Number(NumberToken::ULong(2_u64.pow(20))),]
        );

        // fraction prefix

        assert_eq!(
            lex_from_str("1m").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(3))),]
        );

        assert_eq!(
            lex_from_str("1u").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(6))),]
        );

        assert_eq!(
            lex_from_str("1n").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(9))),]
        );

        assert_eq!(
            lex_from_str("1p").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(12))),]
        );

        assert_eq!(
            lex_from_str("1f").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(15))),]
        );

        assert_eq!(
            lex_from_str("1a").unwrap(),
            vec![Token::Number(NumberToken::Float(1_f32 / 10_f32.powi(18))),]
        );

        // both prefix and number type
        assert_eq!(
            lex_from_str("1K@ulong").unwrap(),
            vec![Token::Number(NumberToken::ULong(1000)),]
        );

        assert_eq!(
            lex_from_str("1Ki@ulong").unwrap(),
            vec![Token::Number(NumberToken::ULong(1024)),]
        );

        assert_eq!(
            lex_from_str("1m@float").unwrap(),
            vec![Token::Number(NumberToken::Float(0.001)),]
        );

        // err: invalid prefix
        assert!(matches!(lex_from_str("1Z"), Err(ParseError { message: _ })));

        // err: incorrect type
        assert!(matches!(
            lex_from_str("1K@int"),
            Err(ParseError { message: _ })
        ));

        // err: incorrect type
        assert!(matches!(
            lex_from_str("1Ki@short"),
            Err(ParseError { message: _ })
        ));

        // err: incorrect type
        assert!(matches!(
            lex_from_str("1m@double"),
            Err(ParseError { message: _ })
        ));
    }

    //     #[test]
    //     fn test_lex_string() {
    //         assert_eq!(lex_from_str("\"\"").unwrap(), vec![Token::new_string("")]);
    //
    //         assert_eq!(
    //             lex_from_str("\"abc\"").unwrap(),
    //             vec![Token::new_string("abc")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("(\"abc\")").unwrap(),
    //             vec![
    //                 Token::LeftParen,
    //                 Token::new_string("abc"),
    //                 Token::RightParen
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("\"abc\" \"xyz\"").unwrap(),
    //             vec![Token::new_string("abc"), Token::new_string("xyz"),]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("\"abc\"\n\n\"xyz\"").unwrap(),
    //             vec![
    //                 Token::new_string("abc"),
    //                 Token::NewLine,
    //                 Token::NewLine,
    //                 Token::new_string("xyz"),
    //             ]
    //         );
    //
    //         // unicode
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             "abc文字😊"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("abc文字😊"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // escape chars
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             "\r\n\t\\\"\u{2d}\u{6587}\0"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("\r\n\t\\\"-文\0"),
    //                 Token::NewLine
    //             ]
    //         );
    //
    //         // err: unsupported escape char \v
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc\vxyz"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: unsupported hex escape "\x.."
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc\x33xyz"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: incomplete escape string
    //         assert!(matches!(
    //             lex_from_str(r#""abc\"#),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: invalid unicode code point
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc\u{110000}xyz"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: invalid unicode escape sequence
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc\u{12mn}xyz"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing left brace for unicode escape sequence
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc\u1234}xyz"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing right brace for unicode escape sequence
    //         assert!(matches!(
    //             lex_from_str(r#""abc\u{1234"#),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing right quote
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             "abc
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_multiple_line_string() {
    //         assert_eq!(
    //             lex_from_str("\"abc\ndef\n    uvw\r\n\t  \txyz\"").unwrap(),
    //             vec![Token::new_string("abc\ndef\n    uvw\r\n\t  \txyz")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("\"abc\\\ndef\\\n    uvw\\\r\n\t  \txyz\"").unwrap(),
    //             vec![Token::new_string("abcdefuvwxyz")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("\"\\\n  \t  \"").unwrap(),
    //             vec![Token::new_string("")]
    //         );
    //
    //         // err: missing right quote
    //         assert!(matches!(
    //             lex_from_str("\"abc\\\n    "),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_law_string() {
    //         assert_eq!(
    //             lex_from_str(
    //                 "r\"abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz\""
    //             )
    //             .unwrap(),
    //             vec![Token::new_string(
    //                 "abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz"
    //             )]
    //         );
    //
    //         // err: missing right quote
    //         assert!(matches!(
    //             lex_from_str("r\"abc    "),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_law_string_varaint() {
    //         assert_eq!(
    //             lex_from_str(
    //                 "r#\"abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz quote: \"foo\"\"#"
    //             )
    //             .unwrap(),
    //             vec![Token::new_string(
    //                 "abc\ndef\n    uvw\r\n\t escape: \\r\\n\\t\\\\ unicode: \\u{1234} xyz quote: \"foo\""
    //             )]
    //         );
    //
    //         // err: missing the ending marker
    //         assert!(matches!(
    //             lex_from_str("r#\"abc    "),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_auto_trimmed_string() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             r|"
    //             one
    //               two
    //                 three
    //             end
    //             "|
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("one\n  two\n    three\nend"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             r|"
    //             one
    //           two
    //         three
    //             end
    //             "|
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("one\ntwo\nthree\nend"),
    //                 Token::NewLine
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             r|"
    //                 one\\\"\t\r\n\u{1234}
    //
    //                 end
    //             "|
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("one\\\\\\\"\\t\\r\\n\\u{1234}\n\nend"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // test the ending symbol ("|) does not start in a new line
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             r|"
    //                 one"|
    //                 two
    //             "|
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_string("one\"|\ntwo"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // test inline
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             11 r|"
    //                 abc
    //             "| 13
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::Number(NumberToken::Decimal("11".to_owned())),
    //                 Token::new_string("abc"),
    //                 Token::Number(NumberToken::Decimal("13".to_owned())),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // err: the content does not start on a new line
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             r|"hello"|
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: the ending marker does not start on a new line
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //         r|"
    //         hello"|
    //         "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing the ending marker
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             r|"
    //             hello
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_byte_data() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             h""
    //             "#
    //             )
    //             .unwrap(),
    //             vec![Token::NewLine, Token::ByteData(vec![]), Token::NewLine,]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             h"11131719"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             h"11 13 1719"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             h"11-13-1719"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             h"11:13:1719"
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 "
    //             h\"1113\n17\t19\"
    //             "
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::ByteData(vec![0x11, 0x13, 0x17, 0x19]),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // err: incomplete byte string, the amount of digits should be even
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             h"1113171"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: invalid char for byte string
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             h"1113171z"
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing the ending quote
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             h"11131719
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_line_comment() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             7 //11
    //             13 17// 19 23
    //             // 29
    //             31// 37
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_dec_number("7"),
    //                 Token::Comment(CommentToken::Line("11".to_owned())),
    //                 Token::new_dec_number("13"),
    //                 Token::new_dec_number("17"),
    //                 Token::Comment(CommentToken::Line(" 19 23".to_owned())),
    //                 Token::Comment(CommentToken::Line(" 29".to_owned())),
    //                 Token::new_dec_number("31"),
    //                 Token::Comment(CommentToken::Line(" 37".to_owned())),
    //                 // note that the line comment includes the ending new line symbol (\n or \r\n),
    //                 // so there is NO `Token::NewLine` follows the line comment.
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_lex_block_comment() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             7 /* 11 13 */ 17
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_dec_number("7"),
    //                 Token::Comment(CommentToken::Block(" 11 13 ".to_owned())),
    //                 Token::new_dec_number("17"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // nested block comment
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             7 /* 11 /* 13 */ 17 */ 19
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_dec_number("7"),
    //                 Token::Comment(CommentToken::Block(" 11 /* 13 */ 17 ".to_owned())),
    //                 Token::new_dec_number("19"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // line comment symbol "//" within the block comment
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             7 /* 11 // 13 17 */ 19
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_dec_number("7"),
    //                 Token::Comment(CommentToken::Block(" 11 // 13 17 ".to_owned())),
    //                 Token::new_dec_number("19"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // document comment symbol (""") within the block comment
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             7 /* 11
    //             """
    //             abc
    //             """
    //             13 */ 19
    //             "#
    //                 .lines()
    //                 .map(&str::trim_start)
    //                 .map(&str::to_owned)
    //                 .collect::<Vec<String>>()
    //                 .join("\n")
    //                 .as_str()
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::new_dec_number("7"),
    //                 Token::Comment(CommentToken::Block(
    //                     " 11\n\"\"\"\nabc\n\"\"\"\n13 ".to_owned()
    //                 )),
    //                 Token::new_dec_number("19"),
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         // err: unpaired, missing the ending pair
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             7 /* 11 /* 13 */ 17
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: unpaired
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             7 */ 11
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_document_comment() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             """
    //             one
    //               two
    //                 three
    //             end
    //             """
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::Comment(CommentToken::Document(
    //                     "one\n  two\n    three\nend".to_owned()
    //                 )),
    //                 // note that the ending marker includes the new line symbol (\n or \r\n),
    //                 // i.e., the ("""\n) or ("""\r\n), so there is NO `Token::NewLine` follows
    //                 // the ending marker.
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             """
    //             one
    //           two
    //         three
    //             end
    //             """
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::Comment(CommentToken::Document("one\ntwo\nthree\nend".to_owned()))
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             """
    //                 one\\\"\t\r\n\u{1234}
    //
    //                 end
    //             """
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::Comment(CommentToken::Document(
    //                     "one\\\\\\\"\\t\\r\\n\\u{1234}\n\nend".to_owned()
    //                 ))
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             """
    //                 one"""
    //                 """two
    //                 """"
    //                 end
    //             """
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::Comment(CommentToken::Document(
    //                     "one\"\"\"\n\"\"\"two\n\"\"\"\"\nend".to_owned()
    //                 ))
    //             ]
    //         );
    //
    //         // err: the content does not start on a new line
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             """hello"""
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: the ending marker does not start on a new line
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //         """
    //         hello"""
    //         "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: the ending marker does not occupy the whole line
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             """
    //             hello
    //             """world
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: missing the ending marker
    //         assert!(matches!(
    //             lex_from_str(
    //                 r#"
    //             """
    //             hello
    //             "#
    //             ),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_symbol() {
    //         assert_eq!(
    //             lex_from_str("name").unwrap(),
    //             vec![Token::new_symbol("name")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("i32.imm").unwrap(),
    //             vec![Token::new_symbol("i32.imm")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("i32.div_s").unwrap(),
    //             vec![Token::new_symbol("i32.div_s")]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("(name)").unwrap(),
    //             vec![
    //                 Token::LeftParen,
    //                 Token::new_symbol("name"),
    //                 Token::RightParen
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("( a )").unwrap(),
    //             vec![Token::LeftParen, Token::new_symbol("a"), Token::RightParen]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("foo bar").unwrap(),
    //             vec![Token::new_symbol("foo"), Token::new_symbol("bar"),]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str("αβγ 文字 🍞🥛").unwrap(),
    //             vec![
    //                 Token::new_symbol("αβγ"),
    //                 Token::new_symbol("文字"),
    //                 Token::new_symbol("🍞🥛"),
    //             ]
    //         );
    //
    //         // err: invalid symbol
    //         assert!(matches!(
    //             lex_from_str("1abc"),
    //             Err(ParseError { message: _ })
    //         ));
    //
    //         // err: invalid char for symbol
    //         assert!(matches!(
    //             lex_from_str("abc+xyz"),
    //             Err(ParseError { message: _ })
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_shebang() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"#!/bin/ancl
    //                 name
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::Shebang("/bin/ancl".to_owned()),
    //                 Token::new_symbol("name"),
    //                 Token::NewLine
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_lex_assembly_text() {
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (local $a i32)
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("local"),
    //                 Token::new_identifier("a"),
    //                 Token::new_symbol("i32"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (i32.imm 211)
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::new_dec_number("211"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (i32.imm 0x223) // line comment
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::new_hex_number("223"),
    //                 Token::RightParen,
    //                 Token::Comment(CommentToken::Line(" line comment".to_owned())),
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (i32.imm /* block comment */ 0x11)
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::Comment(CommentToken::Block(" block comment ".to_owned())),
    //                 Token::new_hex_number("11"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (i32.imm /* nested /* block comment */*/ 0x11_22)
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::Comment(CommentToken::Block(
    //                     " nested /* block comment */".to_owned()
    //                 )),
    //                 Token::new_hex_number("11_22"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (i32.div_s          // multiple lines
    //                 (i32.imm 11)    // left-hand-side
    //                 (i32.imm /*right hand side*/ 17)
    //             )
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.div_s"),
    //                 Token::Comment(CommentToken::Line(" multiple lines".to_owned())),
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::new_dec_number("11"),
    //                 Token::RightParen,
    //                 Token::Comment(CommentToken::Line(" left-hand-side".to_owned())),
    //                 Token::LeftParen,
    //                 Token::new_symbol("i32.imm"),
    //                 Token::Comment(CommentToken::Block("right hand side".to_owned())),
    //                 Token::new_dec_number("17"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"
    //             (import
    //                 (module
    //                     (local "math")
    //                     (function $add "add" (param $lhs i32))
    //                 )
    //             )
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("import"),
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("module"),
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("local"),
    //                 Token::new_string("math"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //                 Token::LeftParen,
    //                 Token::new_symbol("function"),
    //                 Token::new_identifier("add"),
    //                 Token::new_string("add"),
    //                 Token::LeftParen,
    //                 Token::new_symbol("param"),
    //                 Token::new_identifier("lhs"),
    //                 Token::new_symbol("i32"),
    //                 Token::RightParen,
    //                 Token::RightParen,
    //                 Token::NewLine,
    //                 Token::RightParen,
    //                 Token::NewLine,
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             lex_from_str(
    //                 r#"#!/bin/ancl
    //                 (module)
    //             "#
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::Shebang("/bin/ancl".to_owned()),
    //                 Token::LeftParen,
    //                 Token::new_symbol("module"),
    //                 Token::RightParen,
    //                 Token::NewLine,
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_filter() {
    //         assert_eq!(
    //             filter(
    //                 &lex_from_str(
    //                     r#"#!/bin/ancl
    //             /* block comment */ 11 // line comment
    //             """
    //             document comment
    //             """
    //             13
    //             "#
    //                 )
    //                 .unwrap()
    //             ),
    //             vec![Token::new_dec_number("11"), Token::new_dec_number("13"),]
    //         );
    //     }
}
