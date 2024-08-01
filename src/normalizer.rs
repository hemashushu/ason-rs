// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{
    error::Error,
    forwarditer::ForwardIter,
    lexer::{NumberToken, Token, TokenIter, TokenWithLocationRange},
};

pub struct NormalizedTokenIter<'a> {
    upstream: &'a mut ForwardIter<'a, Result<TokenWithLocationRange, Error>>,
}

impl<'a> Iterator for NormalizedTokenIter<'a> {
    type Item = Result<TokenWithLocationRange, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        normalize(self)
    }
}

// - remove all comments.
// - convert commas into newlines
// - combine multiple continuous newlines into one newline.
// - remove the '+' tokens in front of numbers (includes `Inf`).
// - apple the '-' tokens into numbers (includes `Inf`).
// - checks if the signed number is overflowed
// - remove document leading newline and tailing newline.
fn normalize(iter: &mut NormalizedTokenIter) -> Option<Result<TokenWithLocationRange, Error>> {
    let mut normalized_tokens = vec![];

    let mut into = tokens.into_iter();
    let mut iter = ForwardIter::new(&mut into, 2);

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