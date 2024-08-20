// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::{fmt::Display, ops::Neg};

use chrono::{DateTime, FixedOffset};

use crate::{
    error::Error,
    lexer::{NumberToken, Token, TokenIter, TokenWithRange},
    location::Range,
    peekableiter::PeekableIter,
};

pub struct ClearTokenIter<'a> {
    upstream: &'a mut dyn Iterator<Item = Result<TokenWithRange, Error>>,
}

impl<'a> ClearTokenIter<'a> {
    pub fn new(upstream: &'a mut dyn Iterator<Item = Result<TokenWithRange, Error>>) -> Self {
        Self { upstream }
    }
}

impl<'a> Iterator for ClearTokenIter<'a> {
    type Item = Result<TokenWithRange, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        clear(self)
    }
}

// - remove all comments.
fn clear(iter: &mut ClearTokenIter) -> Option<Result<TokenWithRange, Error>> {
    loop {
        match iter.upstream.next() {
            Some(result) => {
                match &result {
                    Ok(TokenWithRange {
                        token: Token::Comment(_),
                        range: _,
                    }) => {
                        // consume comments
                    }
                    _ => {
                        return Some(result);
                    }
                }
            }
            None => {
                return None;
            }
        }
    }
}

pub struct NormalizedTokenIter<'a> {
    upstream: &'a mut PeekableIter<'a, Result<TokenWithRange, Error>>,
}

impl<'a> NormalizedTokenIter<'a> {
    pub fn new(upstream: &'a mut PeekableIter<'a, Result<TokenWithRange, Error>>) -> Self {
        Self { upstream }
    }
}

impl<'a> Iterator for NormalizedTokenIter<'a> {
    type Item = Result<TokenWithRange, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        normalize(self)
    }
}

// - combine multiple continuous newlines into one newline.
//   rules:
//     + comma + blanks -> comma
//     + blanks + comma -> comma
//     + blanks + comma + blanks -> comma
//     + comma + comment + comma -> comma + comma
//     + blanks + comment + blanks -> blank
//     + blanks -> blank
// - remove the '+' tokens in front of numbers (includes `Inf`).
// - apple the '-' tokens to numbers (includes `Inf`).
// - checks if the signed number is overflowed
//   note that the lexer does not check the valid range of a signed integer
//   because in the lexing phase the lexer only extracts tokens and does not
//   check the validity of a combination of tokens.
//   i.e., the integer does not know if it is preceded by a plus or minus sign.
//   for example, "128" is an invalid i8, but "-128" is a valid i8.
//   so the validity rnange of an integer can only be checked in the normalization
//   phase after combining the plus or minus sign and the number of tokens.
fn normalize(iter: &mut NormalizedTokenIter) -> Option<Result<TokenWithRange, Error>> {
    loop {
        match iter.upstream.next() {
            Some(result) => {
                match &result {
                    Ok(token_with_range) => {
                        let TokenWithRange {
                            token,
                            range: current_range,
                        } = token_with_range;

                        let mut start_range = Some(*current_range);
                        let mut end_range = start_range;

                        match token {
                            Token::NewLine => {
                                // consume continuous newlines
                                while let Some(Ok(TokenWithRange {
                                    token: Token::NewLine,
                                    range: current_range,
                                })) = iter.upstream.peek(0)
                                {
                                    end_range = Some(*current_range);
                                    iter.upstream.next();
                                }

                                // found ','
                                if let Some(Ok(TokenWithRange {
                                    token: Token::Comma,
                                    range: current_range,
                                })) = iter.upstream.peek(0)
                                {
                                    // consume comma
                                    start_range = Some(*current_range);
                                    end_range = start_range;
                                    iter.upstream.next();

                                    // consume trailing continuous newlines
                                    while let Some(Ok(TokenWithRange {
                                        token: Token::NewLine,
                                        range: _,
                                    })) = iter.upstream.peek(0)
                                    {
                                        iter.upstream.next();
                                    }

                                    return Some(Ok(TokenWithRange::new(
                                        Token::Comma,
                                        Range::from_range_pair(
                                            &start_range.unwrap(),
                                            &end_range.unwrap(),
                                        ),
                                    )));
                                } else {
                                    return Some(Ok(TokenWithRange::new(
                                        Token::NewLine,
                                        Range::from_range_pair(
                                            &start_range.unwrap(),
                                            &end_range.unwrap(),
                                        ),
                                    )));
                                }
                            }
                            Token::Comma => {
                                // consume continuous newlines
                                while let Some(Ok(TokenWithRange {
                                    token: Token::NewLine,
                                    range: _,
                                })) = iter.upstream.peek(0)
                                {
                                    iter.upstream.next();
                                }

                                return Some(Ok(TokenWithRange::new(
                                    Token::Comma,
                                    Range::from_range_pair(
                                        &start_range.unwrap(),
                                        &end_range.unwrap(),
                                    ),
                                )));
                            }
                            Token::Plus => {
                                match iter.upstream.peek(0) {
                                    Some(Ok(TokenWithRange {
                                        token: Token::Number(num),
                                        range: _,
                                    })) => {
                                        match num {
                                            NumberToken::F32(f) if f.is_nan() => {
                                                return Some(Err(Error::MessageWithRange(
                                                    "The plus sign cannot be applied to NaN."
                                                        .to_owned(),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )));
                                            }
                                            NumberToken::F64(f) if f.is_nan() => {
                                                return Some(Err(Error::MessageWithRange(
                                                    "The plus sign cannot be applied to NaN."
                                                        .to_owned(),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )));
                                            }
                                            _ => {
                                                // consumes the current token (i.e., the plus sign),
                                                // but it's already done and nothing needs to be done here.
                                            }
                                        }
                                    }
                                    Some(_) => {
                                        return Some(Err(Error::MessageWithRange(
                                            "The plus sign cannot be applied to data other than numbers."
                                                .to_owned(),Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                        )));
                                    }
                                    None => {
                                        return Some(Err(Error::MessageWithRange(
                                            "Unexpected end of document.".to_owned(),
                                            Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                        )));
                                    }
                                }
                            }
                            Token::Minus => {
                                match iter.upstream.peek(0) {
                                    Some(Ok(TokenWithRange {
                                        token: Token::Number(num),
                                        range: _,
                                    })) => {
                                        match num {
                                            NumberToken::F32(v) => {
                                                if v.is_nan() {
                                                    return Some(Err(Error::MessageWithRange(
                                                        "The minus sign cannot be applied to NaN."
                                                            .to_owned(),
                                                        Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                    )));
                                                } else {
                                                    let token =
                                                        Token::Number(NumberToken::F32(v.neg()));

                                                    // consume the minus sign (already done) and the number literal token
                                                    iter.upstream.next();

                                                    return Some(Ok(TokenWithRange::new(
                                                        token,
                                                        Range::new(0, 0, 0, 0, 0),
                                                    ))); // todo:: range
                                                }
                                            }
                                            NumberToken::F64(v) => {
                                                if v.is_nan() {
                                                    return Some(Err(Error::MessageWithRange(
                                                        "The minus sign cannot be applied to NaN."
                                                            .to_owned(),
                                                        Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                    )));
                                                } else {
                                                    let token =
                                                        Token::Number(NumberToken::F64(v.neg()));

                                                    // consume the minus sign (alredy done) and the number literal token
                                                    iter.upstream.next();

                                                    return Some(Ok(TokenWithRange::new(
                                                        token,
                                                        Range::new(0, 0, 0, 0, 0),
                                                    ))); // todo:: range
                                                }
                                            }
                                            NumberToken::I8(v) => {
                                                let r = format!("-{}", v).parse::<i8>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative i8, reason: {}",
                                                        v, e
                                                    ),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )
                                                });

                                                match r {
                                                    Ok(v) => {
                                                        let token =
                                                            Token::Number(NumberToken::I8(v as u8));

                                                        // consume the minus sign (already done) and the number literal token
                                                        iter.next();

                                                        return Some(Ok(TokenWithRange::new(
                                                            token,
                                                            Range::new(0, 0, 0, 0, 0),
                                                        ))); // todo:: range
                                                    }
                                                    Err(e) => {
                                                        return Some(Err(e));
                                                    }
                                                }
                                            }
                                            NumberToken::I16(v) => {
                                                let r = format!("-{}", v).parse::<i16>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative i16, reason: {}",
                                                        v, e
                                                    ),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )
                                                });

                                                match r {
                                                    Ok(v) => {
                                                        let token = Token::Number(
                                                            NumberToken::I16(v as u16),
                                                        );

                                                        // consume the minus sign (already done) and the number literal token
                                                        iter.next();

                                                        return Some(Ok(TokenWithRange::new(
                                                            token,
                                                            Range::new(0, 0, 0, 0, 0),
                                                        ))); // todo:: range
                                                    }
                                                    Err(e) => {
                                                        return Some(Err(e));
                                                    }
                                                }
                                            }
                                            NumberToken::I32(v) => {
                                                let r = format!("-{}", v).parse::<i32>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative, reason: {}",
                                                        v, e
                                                    ),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )
                                                });

                                                match r {
                                                    Ok(v) => {
                                                        let token = Token::Number(
                                                            NumberToken::I32(v as u32),
                                                        );

                                                        // consume the minus sign (already done) and the number literal token
                                                        iter.next();

                                                        return Some(Ok(TokenWithRange::new(
                                                            token,
                                                            Range::new(0, 0, 0, 0, 0),
                                                        ))); // todo:: range
                                                    }
                                                    Err(e) => {
                                                        return Some(Err(e));
                                                    }
                                                }
                                            }
                                            NumberToken::I64(v) => {
                                                let r = format!("-{}", v).parse::<i64>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative, reason: {}",
                                                        v, e
                                                    ),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )
                                                });

                                                match r {
                                                    Ok(v) => {
                                                        let token = Token::Number(
                                                            NumberToken::I64(v as u64),
                                                        );

                                                        // consume the minus sign (already done) and the number literal token
                                                        iter.next();

                                                        return Some(Ok(TokenWithRange::new(
                                                            token,
                                                            Range::new(0, 0, 0, 0, 0),
                                                        ))); // todo:: range
                                                    }
                                                    Err(e) => {
                                                        return Some(Err(e));
                                                    }
                                                }
                                            }
                                            NumberToken::U8(_)
                                            | NumberToken::U16(_)
                                            | NumberToken::U32(_)
                                            | NumberToken::U64(_) => {
                                                return Some(Err(Error::MessageWithRange(
                                                    "The minus sign cannot be applied to unsigned numbers."
                                                        .to_owned(),
                                                    Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                                )));
                                            }
                                        }
                                    }
                                    Some(_) => {
                                        return Some(Err(Error::MessageWithRange(
                                            "The minus sign cannot be applied to data other than numbers."
                                                .to_owned(),
                                            Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                        ))); // todo:: range
                                    }
                                    None => {
                                        return Some(Err(Error::MessageWithRange(
                                            "Unexpected end of document.".to_owned(),
                                            Range::new(0, 0, 0, 0, 0), // todo:: to update the range
                                        ))); // todo:: range
                                    }
                                }
                            }
                            Token::Number(NumberToken::I8(v)) if *v > i8::MAX as u8 => {
                                // check signed number overflow
                                return Some(Err(Error::MessageWithRange(
                                    format!("The byte integer number {} is overflowed.", v),
                                    start_range.unwrap(),
                                )));
                            }
                            Token::Number(NumberToken::I16(v)) if *v > i16::MAX as u16 => {
                                // check signed number overflow
                                return Some(Err(Error::MessageWithRange(
                                    format!("The short integer number {} is overflowed.", v),
                                    start_range.unwrap(),
                                )));
                            }
                            Token::Number(NumberToken::I32(v)) if *v > i32::MAX as u32 => {
                                // check signed number overflow
                                return Some(Err(Error::MessageWithRange(
                                    format!("The integer number {} is overflowed.", v),
                                    start_range.unwrap(),
                                )));
                            }
                            Token::Number(NumberToken::I64(v)) if *v > i64::MAX as u64 => {
                                // check signed number overflow
                                return Some(Err(Error::MessageWithRange(
                                    format!("The long integer number {} is overflowed.", v),
                                    start_range.unwrap(),
                                )));
                            }
                            _ => {
                                return Some(result);
                            }
                        };
                    }
                    Err(_) => {
                        return Some(result);
                    }
                }
            }
            None => {
                return None;
            }
        }
    }
}

pub struct TrimmedTokenIter<'a> {
    upstream: &'a mut PeekableIter<'a, Result<TokenWithRange, Error>>,
}

impl<'a> TrimmedTokenIter<'a> {
    pub fn new(upstream: &'a mut PeekableIter<'a, Result<TokenWithRange, Error>>) -> Self {
        // consume the first '\n of document
        if let Some(Ok(TokenWithRange {
            token: Token::NewLine,
            ..
        })) = upstream.peek(0)
        {
            upstream.next();
        }

        Self { upstream }
    }
}

impl<'a> Iterator for TrimmedTokenIter<'a> {
    type Item = Result<TokenWithRange, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        trim(self)
    }
}

// - remove document leading and tailing newlines.
fn trim(iter: &mut TrimmedTokenIter) -> Option<Result<TokenWithRange, Error>> {
    match iter.upstream.next() {
        Some(r) => {
            match &r {
                Ok(tl) => {
                    let TokenWithRange { token, .. } = tl;
                    match token {
                        Token::NewLine if iter.upstream.peek(0).is_none() => {
                            // it is the last '\n' of document
                            None
                        }
                        _ => Some(r),
                    }
                }
                Err(_) => Some(r),
            }
        }
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        error::Error,
        lexer::{CharsWithLocationIter, NumberToken, Token, TokenIter, TokenWithRange},
        location::Location,
        peekableiter::PeekableIter,
    };

    use super::{ClearTokenIter, NormalizedTokenIter, TrimmedTokenIter};

    fn lex_str_to_vec_with_location(s: &str) -> Result<Vec<TokenWithRange>, Error> {
        let mut chars = s.chars();
        let mut char_location_iter = CharsWithLocationIter::new(0, &mut chars);
        let mut peekable_char_location_iter = PeekableIter::new(&mut char_location_iter, 3);
        let mut token_iter = TokenIter::new(&mut peekable_char_location_iter);
        let mut clear_iter = ClearTokenIter::new(&mut token_iter);
        let mut peekable_clear_iter = PeekableIter::new(&mut clear_iter, 1);
        let mut normalized_iter = NormalizedTokenIter::new(&mut peekable_clear_iter);
        let mut peekable_normalized_iter = PeekableIter::new(&mut normalized_iter, 1);
        let trimmed_iter = TrimmedTokenIter::new(&mut peekable_normalized_iter);

        // do not use `iter.collect::<Vec<_>>()` because the `TokenIter` throws
        // exceptions though the function `next() -> Option<Result<...>>`,
        // the iterator wouldn't stop even if it encounters an error.
        let mut ts = vec![];
        for r in trimmed_iter {
            match r {
                Ok(r) => ts.push(r),
                Err(e) => return Err(e),
            }
        }

        Ok(ts)
    }

    fn lex_str_to_vec(s: &str) -> Result<Vec<Token>, Error> {
        let tokens = lex_str_to_vec_with_location(s)?
            .iter()
            .map(|e| e.token.to_owned())
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    #[test]
    fn test_normalize_blanks_commas_and_comments() {
        assert_eq!(
            // test:
            // - comma -> comma
            // - comma + blanks -> comma
            // - blanks + comma -> comma
            // - blanks + comma + blanks -> comma
            // - comma + comment + comma -> comma + comma
            // - blanks + comment + blanks -> blank
            // - blanks -> blank
            lex_str_to_vec(
                r#"
                    [1,2,

                    3

                    ,4

                    ,

                    5
                    ,
                    // comment between commas
                    ,
                    6

                    // comment between blank lines

                    7
                    8
                    ]

                    "#
            )
            .unwrap(),
            vec![
                Token::LeftBracket,
                Token::Number(NumberToken::I32(1)),
                Token::Comma,
                Token::Number(NumberToken::I32(2)),
                Token::Comma,
                Token::Number(NumberToken::I32(3)),
                Token::Comma,
                Token::Number(NumberToken::I32(4)),
                Token::Comma,
                Token::Number(NumberToken::I32(5)),
                Token::Comma,
                Token::Comma,
                Token::Number(NumberToken::I32(6)),
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::NewLine,
                Token::Number(NumberToken::I32(8)),
                Token::NewLine,
                Token::RightBracket,
            ]
        );

        // location

        // blanks -> blank
        assert_eq!(
            lex_str_to_vec_with_location("11\n \n  \n13").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_location_and_length(
                    Token::NewLine,
                    &Location::new(0, 2, 0, 2),
                    6
                ),
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new(0, 8, 3, 0),
                    2
                ),
            ]
        );

        // comma + blanks -> comma
        assert_eq!(
            lex_str_to_vec_with_location(",\n\n\n11").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Comma,
                    &Location::new(0, 0, 0, 0),
                    1
                ),
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new(0, 4, 3, 0),
                    2
                ),
            ]
        );

        // blanks + comma -> comma
        assert_eq!(
            lex_str_to_vec_with_location("11\n\n\n,").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_location_and_length(
                    Token::Comma,
                    &Location::new(0, 5, 3, 0),
                    1
                ),
            ]
        );

        // blanks + comma + blanks -> comma
        assert_eq!(
            lex_str_to_vec_with_location("11\n\n,\n\n13").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_location_and_length(
                    Token::Comma,
                    &Location::new(0, 4, 2, 0),
                    1
                ),
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new(0, 7, 4, 0),
                    2
                ),
            ]
        );

        // comma + comment + comma -> comma + comma
        assert_eq!(
            lex_str_to_vec_with_location(",//abc\n,").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Comma,
                    &Location::new(0, 0, 0, 0),
                    1
                ),
                TokenWithRange::from_location_and_length(
                    Token::Comma,
                    &Location::new(0, 7, 1, 0),
                    1
                ),
            ]
        );

        // blanks + comment + blanks -> blank
        assert_eq!(
            lex_str_to_vec_with_location("11\n\n//abc\n\n13").unwrap(),
            vec![
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_location_and_length(
                    Token::NewLine,
                    &Location::new(0, 2, 0, 2),
                    9
                ),
                TokenWithRange::from_location_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new(0, 11, 4, 0),
                    2
                ),
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus() {
        // implicit type, default int
        {
            assert_eq!(
                lex_str_to_vec("+11").unwrap(),
                vec![Token::Number(NumberToken::I32(11))]
            );

            assert_eq!(
                lex_str_to_vec("-13").unwrap(),
                vec![Token::Number(NumberToken::I32(-13_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("2_147_483_648"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-2_147_483_649_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: plus sign is added to non-numbers
            assert!(matches!(
                lex_str_to_vec("+true"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: minus sign is added to non-numbers
            assert!(matches!(
                lex_str_to_vec("-true"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // byte
        {
            assert_eq!(
                lex_str_to_vec("+127_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(127))]
            );

            assert_eq!(
                lex_str_to_vec("-128_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-128_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("128_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-129_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // short
        {
            assert_eq!(
                lex_str_to_vec("+32767_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(32767))]
            );

            assert_eq!(
                lex_str_to_vec("-32768_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-32768_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("32768_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-32769_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // int
        {
            assert_eq!(
                lex_str_to_vec("+2_147_483_647_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(2_147_483_647i32 as u32))]
            );

            assert_eq!(
                lex_str_to_vec("-2_147_483_648_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-2_147_483_648i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("2_147_483_648_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-2_147_483_649_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // long
        {
            assert_eq!(
                lex_str_to_vec("+9_223_372_036_854_775_807_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    9_223_372_036_854_775_807i64 as u64
                )),]
            );

            assert_eq!(
                lex_str_to_vec("-9_223_372_036_854_775_808_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -9_223_372_036_854_775_808i64 as u64
                )),]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("9_223_372_036_854_775_808_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-9_223_372_036_854_775_809_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // location

        {
            // todo::
        }
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_number() {
        // general
        assert_eq!(
            lex_str_to_vec("+3.402_823_5e+38").unwrap(),
            vec![Token::Number(NumberToken::F64(3.402_823_5e38f64))]
        );

        assert_eq!(
            lex_str_to_vec("-3.402_823_5e+38").unwrap(),
            vec![Token::Number(NumberToken::F64(-3.402_823_5e38f64))]
        );

        // 0.0, +0.0, -0.0
        {
            assert_eq!(
                lex_str_to_vec("0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_str_to_vec("+0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec("-0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );
        }

        // NaN
        {
            let t = lex_str_to_vec("NaN").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));
        }

        // Inf
        {
            assert_eq!(
                lex_str_to_vec("Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("+Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("-Inf").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::NEG_INFINITY))]
            );
        }

        // err: +NaN
        assert!(matches!(
            lex_str_to_vec("+NaN"),
            Err(Error::MessageWithRange(..)) // todo:: location
        ));

        // err: -NaN
        assert!(matches!(
            lex_str_to_vec("-NaN"),
            Err(Error::MessageWithRange(..)) // todo:: location
        ));

        // location
        {
            // todo:
        }
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_number_with_explicit_type() {
        // single precision, f32
        {
            assert_eq!(
                lex_str_to_vec("+1.602_176_6e-19_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(1.602_176_6e-19f32))]
            );

            assert_eq!(
                lex_str_to_vec("-1.602_176_6e-19_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(-1.602_176_6e-19f32))]
            );

            assert_eq!(
                lex_str_to_vec("0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            assert_eq!(
                lex_str_to_vec("+0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec("-0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            let t = lex_str_to_vec("NaN_f32").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F32(v)) if v.is_nan()));

            assert_eq!(
                lex_str_to_vec("Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("+Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("-Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::NEG_INFINITY))]
            );

            // err: +NaN
            assert!(matches!(
                lex_str_to_vec("+NaN_f32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: -NaN
            assert!(matches!(
                lex_str_to_vec("-NaN_f32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // double precision, f64
        {
            assert_eq!(
                lex_str_to_vec("1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(
                    1.797_693_134_862_315_7e308_f64
                )),]
            );

            assert_eq!(
                lex_str_to_vec("-1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(
                    -1.797_693_134_862_315_7e308_f64
                )),]
            );

            assert_eq!(
                lex_str_to_vec("0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_str_to_vec("+0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec("-0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            let t = lex_str_to_vec("NaN_f64").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));

            assert_eq!(
                lex_str_to_vec("Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("+Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec("-Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::NEG_INFINITY))]
            );

            // err: +NaN
            assert!(matches!(
                lex_str_to_vec("+NaN_f64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: -NaN
            assert!(matches!(
                lex_str_to_vec("-NaN_f64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // location
        {
            // todo::
        }
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_hex_number() {
        // implicit type, default int
        {
            assert_eq!(
                lex_str_to_vec("+0x11").unwrap(),
                vec![Token::Number(NumberToken::I32(0x11))]
            );

            assert_eq!(
                lex_str_to_vec("-0x13").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x13_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0x8000_0000"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0001"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // byte
        {
            assert_eq!(
                lex_str_to_vec("+0x7f_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                lex_str_to_vec("-0x80_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0x80_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x81_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // short
        {
            assert_eq!(
                lex_str_to_vec("+0x7fff_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                lex_str_to_vec("-0x8000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0x8000_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8001_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // int
        {
            assert_eq!(
                lex_str_to_vec("+0x7fff_ffff_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                lex_str_to_vec("-0x8000_0000_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0x8000_0000_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0001_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // long
        {
            assert_eq!(
                lex_str_to_vec("+0x7fff_ffff_ffff_ffff_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    0x7fff_ffff_ffff_ffff_i64 as u64
                ))]
            );

            assert_eq!(
                lex_str_to_vec("-0x8000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -0x8000_0000_0000_0000_i64 as u64
                ))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0x8000_0000_0000_0000_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0000_0000_0001_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // location

        {
            // todo:: location
        }
    }

    #[test]
    fn test_normalize_plus_and_minus_hex_floating_point_number() {
        // 3.1415927f32
        assert_eq!(
            lex_str_to_vec("+0x1.921fb6p1f32").unwrap(),
            vec![Token::Number(NumberToken::F32(std::f32::consts::PI))]
        );

        // -2.718281828459045f64
        assert_eq!(
            lex_str_to_vec("-0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![Token::Number(NumberToken::F64(-std::f64::consts::E))]
        );

        // location

        {
            // todo:: location
        }
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_binary_number() {
        // implicit type, default int
        {
            assert_eq!(
                lex_str_to_vec("+0b101").unwrap(),
                vec![Token::Number(NumberToken::I32(0b101_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0001_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // byte
        {
            assert_eq!(
                lex_str_to_vec("0b0111_1111_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                lex_str_to_vec("-0b1000_0000_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0b1000_0000_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0001_i8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u8"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // short
        {
            assert_eq!(
                lex_str_to_vec("+0b0111_1111_1111_1111_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                lex_str_to_vec("-0b1000_0000_0000_0000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0b1000_0000_0000_0000_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0001_i16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u16"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // int
        {
            assert_eq!(
                lex_str_to_vec("+0b0111_1111_1111_1111__1111_1111_1111_1111_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0000_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0001_i32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u32"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));
        }

        // long
        {
            assert_eq!(
                lex_str_to_vec("0b0111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(0x7fff_ffff_ffff_ffff_i64 as u64))]
            );

            assert_eq!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(-0x8000_0000_0000_0000_i64 as u64))]
            );

            // err: signed overflow
            assert!(matches!(
                lex_str_to_vec("0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0001_i64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u64"),
                Err(Error::MessageWithRange(..)) // todo:: location
            ));

            // location

            {
                // todo:: location
            }
        }
    }
}
