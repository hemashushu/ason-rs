// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::ops::Neg;

use crate::{
    error::Error,
    lexer::{NumberToken, Token, TokenWithRange},
    location::{Position, Range},
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
                        ..
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
    match iter.upstream.next() {
        Some(result) => match &result {
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

                            Some(Ok(TokenWithRange::new(
                                Token::Comma,
                                Range::from_range_pair(&start_range.unwrap(), &end_range.unwrap()),
                            )))
                        } else {
                            Some(Ok(TokenWithRange::new(
                                Token::NewLine,
                                Range::from_range_pair(&start_range.unwrap(), &end_range.unwrap()),
                            )))
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

                        Some(Ok(TokenWithRange::new(
                            Token::Comma,
                            Range::from_range_pair(&start_range.unwrap(), &end_range.unwrap()),
                        )))
                    }
                    Token::Plus => {
                        match iter.upstream.peek(0) {
                            Some(Ok(TokenWithRange {
                                token: token @ Token::Number(num),
                                range: current_range,
                            })) => {
                                match num {
                                    NumberToken::F32(f) if f.is_nan() => {
                                        // combines two token ranges.
                                        Some(Err(Error::MessageWithRange(
                                            "The plus sign cannot be applied to NaN.".to_owned(),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    NumberToken::F64(f) if f.is_nan() => {
                                        // combines two token ranges.
                                        Some(Err(Error::MessageWithRange(
                                            "The plus sign cannot be applied to NaN.".to_owned(),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    NumberToken::I8(v) if *v > i8::MAX as u8 => {
                                        // check signed number overflow
                                        Some(Err(Error::MessageWithRange(
                                            format!("The byte integer number {} is overflowed.", v),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    NumberToken::I16(v) if *v > i16::MAX as u16 => {
                                        // check signed number overflow
                                        Some(Err(Error::MessageWithRange(
                                            format!(
                                                "The short integer number {} is overflowed.",
                                                v
                                            ),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    NumberToken::I32(v) if *v > i32::MAX as u32 => {
                                        // check signed number overflow
                                        Some(Err(Error::MessageWithRange(
                                            format!("The integer number {} is overflowed.", v),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    NumberToken::I64(v) if *v > i64::MAX as u64 => {
                                        // check signed number overflow
                                        Some(Err(Error::MessageWithRange(
                                            format!("The long integer number {} is overflowed.", v),
                                            Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        )))
                                    }
                                    _ => {
                                        // combines two token ranges and constructs new number token.
                                        let ret_val = Some(Ok(TokenWithRange {
                                            token: token.clone(),
                                            range: Range::from_range_pair(
                                                &start_range.unwrap(),
                                                current_range,
                                            ),
                                        }));

                                        // consumes the the plus sign (it's already done) and the
                                        // number token.
                                        iter.upstream.next();

                                        ret_val
                                    }
                                }
                            }
                            Some(Ok(TokenWithRange {
                                token: _,
                                range: current_range,
                            })) => {
                                // combines two token ranges.
                                Some(Err(Error::MessageWithRange(
                                    "The plus sign cannot be applied to data other than numbers."
                                        .to_owned(),
                                    Range::from_range_pair(&start_range.unwrap(), current_range),
                                )))
                            }
                            Some(Err(e)) => Some(Err(e.clone())),
                            None => {
                                // "...+EOF"
                                Some(Err(Error::MessageWithPosition(
                                    "Unexpected end of document.".to_owned(),
                                    {
                                        let range = start_range.unwrap();
                                        Position {
                                            unit: range.unit,
                                            index: range.index + 1,
                                            line: range.line,
                                            column: range.column + 1,
                                        }
                                    },
                                )))
                            }
                        }
                    }
                    Token::Minus => {
                        match iter.upstream.peek(0) {
                            Some(Ok(TokenWithRange {
                                token: Token::Number(num),
                                range: current_range,
                            })) => {
                                match num {
                                    NumberToken::F32(v) => {
                                        if v.is_nan() {
                                            // combines two token ranges.
                                            Some(Err(Error::MessageWithRange(
                                                "The minus sign cannot be applied to NaN."
                                                    .to_owned(),
                                                Range::from_range_pair(
                                                    &start_range.unwrap(),
                                                    current_range,
                                                ),
                                            )))
                                        } else {
                                            // combines two token ranges and constructs new number token.
                                            let ret_val = Some(Ok(TokenWithRange {
                                                token: Token::Number(NumberToken::F32(v.neg())),
                                                range: Range::from_range_pair(
                                                    &start_range.unwrap(),
                                                    current_range,
                                                ),
                                            }));

                                            // consume the minus sign (it's already done) and the
                                            // number token
                                            iter.upstream.next();

                                            ret_val
                                        }
                                    }
                                    NumberToken::F64(v) => {
                                        if v.is_nan() {
                                            // combines two token ranges.
                                            Some(Err(Error::MessageWithRange(
                                                "The minus sign cannot be applied to NaN."
                                                    .to_owned(),
                                                Range::from_range_pair(
                                                    &start_range.unwrap(),
                                                    current_range,
                                                ),
                                            )))
                                        } else {
                                            // combines two token ranges and constructs new number token.
                                            let ret_val = Some(Ok(TokenWithRange {
                                                token: Token::Number(NumberToken::F64(v.neg())),
                                                range: Range::from_range_pair(
                                                    &start_range.unwrap(),
                                                    current_range,
                                                ),
                                            }));

                                            // consume the minus sign (it's already done) and the
                                            // number token
                                            iter.upstream.next();

                                            ret_val
                                        }
                                    }
                                    NumberToken::I8(v) => {
                                        let combined_range = Range::from_range_pair(
                                            &start_range.unwrap(),
                                            current_range,
                                        );

                                        let parse_result =
                                            format!("-{}", v).parse::<i8>().map_err(|e| {
                                                Error::MessageWithRange(
                                                    format!(
                                                "Can not convert \"{}\" to negative i8, reason: {}",
                                                v, e
                                            ),
                                                    combined_range,
                                                )
                                            });

                                        match parse_result {
                                            Ok(v) => {
                                                let ret_val = Some(Ok(TokenWithRange::new(
                                                    Token::Number(NumberToken::I8(v as u8)),
                                                    combined_range,
                                                )));

                                                // consume the minus sign (already done) and the number literal token
                                                iter.next();

                                                ret_val
                                            }
                                            Err(e) => Some(Err(e)),
                                        }
                                    }
                                    NumberToken::I16(v) => {
                                        let combined_range = Range::from_range_pair(
                                            &start_range.unwrap(),
                                            current_range,
                                        );

                                        let parse_result = format!("-{}", v).parse::<i16>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative i16, reason: {}",
                                                        v, e
                                                    ),
                                                    combined_range
                                                )
                                                });

                                        match parse_result {
                                            Ok(v) => {
                                                let ret_val = Some(Ok(TokenWithRange::new(
                                                    Token::Number(NumberToken::I16(v as u16)),
                                                    combined_range,
                                                )));

                                                // consume the minus sign (already done) and the number literal token
                                                iter.next();

                                                ret_val
                                            }
                                            Err(e) => Some(Err(e)),
                                        }
                                    }
                                    NumberToken::I32(v) => {
                                        let combined_range = Range::from_range_pair(
                                            &start_range.unwrap(),
                                            current_range,
                                        );

                                        let parse_result = format!("-{}", v).parse::<i32>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative, reason: {}",
                                                        v, e
                                                    ),
                                                    combined_range
                                                )
                                                });

                                        match parse_result {
                                            Ok(v) => {
                                                let ret_val = Some(Ok(TokenWithRange::new(
                                                    Token::Number(NumberToken::I32(v as u32)),
                                                    combined_range,
                                                )));

                                                // consume the minus sign (already done) and the number literal token
                                                iter.next();

                                                ret_val
                                            }
                                            Err(e) => Some(Err(e)),
                                        }
                                    }
                                    NumberToken::I64(v) => {
                                        let combined_range = Range::from_range_pair(
                                            &start_range.unwrap(),
                                            current_range,
                                        );

                                        let parse_result = format!("-{}", v).parse::<i64>().map_err(|e| {
                                                    Error::MessageWithRange(format!(
                                                        "Can not convert \"{}\" to negative, reason: {}",
                                                        v, e
                                                    ),
                                                    combined_range
                                                )
                                                });

                                        match parse_result {
                                            Ok(v) => {
                                                let ret_val = Some(Ok(TokenWithRange::new(
                                                    Token::Number(NumberToken::I64(v as u64)),
                                                    combined_range,
                                                )));

                                                // consume the minus sign (already done) and the number literal token
                                                iter.next();

                                                ret_val
                                            }
                                            Err(e) => Some(Err(e)),
                                        }
                                    }
                                    NumberToken::U8(_)
                                    | NumberToken::U16(_)
                                    | NumberToken::U32(_)
                                    | NumberToken::U64(_) => Some(Err(Error::MessageWithRange(
                                        "The minus sign cannot be applied to unsigned numbers."
                                            .to_owned(),
                                        Range::from_range_pair(
                                            &start_range.unwrap(),
                                            current_range,
                                        ),
                                    ))),
                                }
                            }
                            Some(Ok(TokenWithRange {
                                token: _,
                                range: current_range,
                            })) => {
                                // combines two token ranges.
                                Some(Err(Error::MessageWithRange(
                                    "The minus sign cannot be applied to data other than numbers."
                                        .to_owned(),
                                    Range::from_range_pair(&start_range.unwrap(), current_range),
                                )))
                            }
                            Some(Err(e)) => Some(Err(e.clone())),
                            None => {
                                // "...-EOF"
                                Some(Err(Error::MessageWithPosition(
                                    "Unexpected end of document.".to_owned(),
                                    {
                                        let range = start_range.unwrap();
                                        Position {
                                            unit: range.unit,
                                            index: range.index + 1,
                                            line: range.line,
                                            column: range.column + 1,
                                        }
                                    },
                                )))
                            }
                        }
                    }
                    Token::Number(NumberToken::I8(v)) if *v > i8::MAX as u8 => {
                        // check signed number overflow
                        Some(Err(Error::MessageWithRange(
                            format!("The byte integer number {} is overflowed.", v),
                            start_range.unwrap(),
                        )))
                    }
                    Token::Number(NumberToken::I16(v)) if *v > i16::MAX as u16 => {
                        // check signed number overflow
                        Some(Err(Error::MessageWithRange(
                            format!("The short integer number {} is overflowed.", v),
                            start_range.unwrap(),
                        )))
                    }
                    Token::Number(NumberToken::I32(v)) if *v > i32::MAX as u32 => {
                        // check signed number overflow
                        Some(Err(Error::MessageWithRange(
                            format!("The integer number {} is overflowed.", v),
                            start_range.unwrap(),
                        )))
                    }
                    Token::Number(NumberToken::I64(v)) if *v > i64::MAX as u64 => {
                        // check signed number overflow
                        Some(Err(Error::MessageWithRange(
                            format!("The long integer number {} is overflowed.", v),
                            start_range.unwrap(),
                        )))
                    }
                    _ => Some(result),
                }
            }
            Err(_) => Some(result),
        },
        None => None,
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
        chariter::CharIterFromOrdinary,
        charposition::CharsWithPositionIter,
        error::Error,
        lexer::{NumberToken, Token, TokenIter, TokenWithRange},
        location::{Position, Range},
        peekableiter::PeekableIter,
    };

    use super::{ClearTokenIter, NormalizedTokenIter, TrimmedTokenIter};

    fn lex_str_to_vec_with_range(s: &str) -> Result<Vec<TokenWithRange>, Error> {
        let mut chars = s.chars();
        let mut char_iter = CharIterFromOrdinary::new(&mut chars);
        let mut position_iter = CharsWithPositionIter::new(0, &mut char_iter);
        let mut peekable_position_iter = PeekableIter::new(&mut position_iter, 3);
        let mut token_iter = TokenIter::new(&mut peekable_position_iter);
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
        let tokens = lex_str_to_vec_with_range(s)?
            .iter()
            .map(|e| e.token.to_owned())
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    #[test]
    fn test_clear_comments() {
        assert_eq!(
            lex_str_to_vec(
                r#"11 // line comment 1
                // line comment 2
                13 /* block comment 1 */
                /*
                block comment 2
                */
                17
                "#
            )
            .unwrap(),
            vec![
                Token::Number(NumberToken::I32(11)),
                Token::NewLine,
                Token::Number(NumberToken::I32(13)),
                Token::NewLine,
                Token::Number(NumberToken::I32(17)),
            ]
        );
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
            lex_str_to_vec_with_range("11\n \n  \n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::NewLine,
                    &Position::new(0, 2, 0, 2),
                    6
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Position::new(0, 8, 3, 0),
                    2
                ),
            ]
        );

        // comma + blanks -> comma
        assert_eq!(
            lex_str_to_vec_with_range(",\n\n\n11").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 0, 0, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 4, 3, 0),
                    2
                ),
            ]
        );

        // blanks + comma -> comma
        assert_eq!(
            lex_str_to_vec_with_range("11\n\n\n,").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 5, 3, 0),
                    1
                ),
            ]
        );

        // blanks + comma + blanks -> comma
        assert_eq!(
            lex_str_to_vec_with_range("11\n\n,\n\n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 4, 2, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Position::new(0, 7, 4, 0),
                    2
                ),
            ]
        );

        // comma + comment + comma -> comma + comma
        assert_eq!(
            lex_str_to_vec_with_range(",//abc\n,").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 0, 0, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 7, 1, 0),
                    1
                ),
            ]
        );

        // blanks + comment + blanks -> blank
        assert_eq!(
            lex_str_to_vec_with_range("11\n\n//abc\n\n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::NewLine,
                    &Position::new(0, 2, 0, 2),
                    9
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Position::new(0, 11, 4, 0),
                    2
                ),
            ]
        );
    }

    #[test]
    fn test_trim_blanks() {
        assert_eq!(
            lex_str_to_vec(
                r#"

                11

                13

                "#
            )
            .unwrap(),
            vec![
                Token::Number(NumberToken::I32(11)),
                Token::NewLine,
                Token::Number(NumberToken::I32(13)),
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_decimal_numbers() {
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+2_147_483_648"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 14
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-2_147_483_649"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 14
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+128_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 7
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-129_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 7
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 5
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+32768_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 10
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-32769_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 10
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 6
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+2_147_483_648_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 18
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-2_147_483_649_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 18
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 6
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+9_223_372_036_854_775_808_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 30
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-9_223_372_036_854_775_809_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 30
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-1_u64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 6
                    }
                ))
            ));
        }

        // location

        {
            assert_eq!(
                lex_str_to_vec_with_range("+11").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Position::new(0, 0, 0, 0),
                    3
                ),]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-13").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(-13_i32 as u32)),
                    &Position::new(0, 0, 0, 0),
                    3
                ),]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+11,-13").unwrap(),
                vec![
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(11)),
                        &Position::new(0, 0, 0, 0),
                        3
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Comma,
                        &Position::new(0, 3, 0, 3),
                        1
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-13_i32 as u32)),
                        &Position::new(0, 4, 0, 4),
                        3
                    ),
                ]
            );
        }

        // +EOF
        assert!(matches!(
            lex_str_to_vec("abc,+"),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 5,
                    line: 0,
                    column: 5,
                }
            ))
        ));

        // -EOF
        assert!(matches!(
            lex_str_to_vec("xyz,-"),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 5,
                    line: 0,
                    column: 5,
                }
            ))
        ));

        // err: plus sign is added to non-numbers
        assert!(matches!(
            lex_str_to_vec("+true"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 5
                }
            ))
        ));

        // err: minus sign is added to non-numbers
        assert!(matches!(
            lex_str_to_vec("-true"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 5
                }
            ))
        ));
    }

    #[test]
    fn test_normalize_signed_integer_overflow_decimal() {
        assert!(matches!(
            lex_str_to_vec("2_147_483_648"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 13
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("128_i8"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 6
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("32768_i16"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 9
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("2_147_483_648_i32"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 17
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("9_223_372_036_854_775_808_i64"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 29
                }
            ))
        ));
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_numbers() {
        // general
        assert_eq!(
            lex_str_to_vec_with_range("+3.402_823_5e+38").unwrap(),
            vec![TokenWithRange::from_position_and_length(
                Token::Number(NumberToken::F64(3.402_823_5e38f64)),
                &Position::new(0, 0, 0, 0),
                16
            )]
        );

        assert_eq!(
            lex_str_to_vec_with_range("-3.402_823_5e+38").unwrap(),
            vec![TokenWithRange::from_position_and_length(
                Token::Number(NumberToken::F64(-3.402_823_5e38f64)),
                &Position::new(0, 0, 0, 0),
                16
            )]
        );

        // 0.0, +0.0, -0.0
        {
            assert_eq!(
                lex_str_to_vec("0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+0.0").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Position::new(0, 0, 0, 0),
                    4
                )]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec_with_range("-0.0").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Position::new(0, 0, 0, 0),
                    4
                )]
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
                lex_str_to_vec_with_range("+Inf").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(f64::INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    4
                )]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-Inf").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(f64::NEG_INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    4
                )]
            );
        }

        // err: +NaN
        assert!(matches!(
            lex_str_to_vec("+NaN"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 4
                }
            ))
        ));

        // err: -NaN
        assert!(matches!(
            lex_str_to_vec("-NaN"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 4
                }
            ))
        ));
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_numbers_with_explicit_type() {
        // single precision, f32
        {
            assert_eq!(
                lex_str_to_vec_with_range("+1.602_176_6e-19_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(1.602_176_6e-19f32)),
                    &Position::new(0, 0, 0, 0),
                    20
                )]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-1.602_176_6e-19_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(-1.602_176_6e-19f32)),
                    &Position::new(0, 0, 0, 0),
                    20
                )]
            );

            assert_eq!(
                lex_str_to_vec("0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+0_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(0f32)),
                    &Position::new(0, 0, 0, 0),
                    6
                )]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec_with_range("-0_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(0f32)),
                    &Position::new(0, 0, 0, 0),
                    6
                )]
            );

            let t = lex_str_to_vec("NaN_f32").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F32(v)) if v.is_nan()));

            assert_eq!(
                lex_str_to_vec("Inf_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+Inf_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(f32::INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    8
                )]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-Inf_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(f32::NEG_INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    8
                )]
            );

            // err: +NaN
            assert!(matches!(
                lex_str_to_vec("+NaN_f32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // err: -NaN
            assert!(matches!(
                lex_str_to_vec("-NaN_f32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));
        }

        // double precision, f64
        {
            assert_eq!(
                lex_str_to_vec_with_range("+1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(1.797_693_134_862_315_7e308_f64)),
                    &Position::new(0, 0, 0, 0),
                    33
                )]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(-1.797_693_134_862_315_7e308_f64)),
                    &Position::new(0, 0, 0, 0),
                    33
                )]
            );

            assert_eq!(
                lex_str_to_vec("0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+0_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Position::new(0, 0, 0, 0),
                    6
                )]
            );

            // +0 == -0
            assert_eq!(
                lex_str_to_vec_with_range("-0_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Position::new(0, 0, 0, 0),
                    6
                )]
            );

            let t = lex_str_to_vec("NaN_f64").unwrap();
            assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));

            assert_eq!(
                lex_str_to_vec("Inf_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+Inf_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(f64::INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    8
                )]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-Inf_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(f64::NEG_INFINITY)),
                    &Position::new(0, 0, 0, 0),
                    8
                )]
            );

            // err: +NaN
            assert!(matches!(
                lex_str_to_vec("+NaN_f64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // err: -NaN
            assert!(matches!(
                lex_str_to_vec("-NaN_f64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));
        }
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_hex_numbers() {
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0x8000_0000"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 12
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0001"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 12
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0x80_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x81_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 7
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0x8000_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 11
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8001_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 11
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0x8000_0000_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 16
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0001_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 16
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0x8000_0000_0000_0000_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0x8000_0000_0000_0001_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0x1_u64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));
        }

        // location

        {
            assert_eq!(
                lex_str_to_vec_with_range("+0x11").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(0x11)),
                    &Position::new(0, 0, 0, 0),
                    5
                ),]
            );

            assert_eq!(
                lex_str_to_vec_with_range("-0x13").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(-0x13_i32 as u32)),
                    &Position::new(0, 0, 0, 0),
                    5
                ),]
            );

            assert_eq!(
                lex_str_to_vec_with_range("+0x11,-0x13").unwrap(),
                vec![
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(0x11)),
                        &Position::new(0, 0, 0, 0),
                        5
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Comma,
                        &Position::new(0, 5, 0, 5),
                        1
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-0x13_i32 as u32)),
                        &Position::new(0, 6, 0, 6),
                        5
                    ),
                ]
            );
        }
    }

    #[test]
    fn test_normalize_signed_integer_overflow_hex() {
        assert!(matches!(
            lex_str_to_vec("0x8000_0000"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 11
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0x80_i8"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 7
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0x8000_i16"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 10
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0x8000_0000_i32"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 15
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0x8000_0000_0000_0000_i64"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 25
                }
            ))
        ));
    }

    #[test]
    fn test_normalize_plus_and_minus_hex_floating_point_numbers() {
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

        assert_eq!(
            lex_str_to_vec_with_range("+0x1.921fb6p1f32,-0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(std::f32::consts::PI)),
                    &Position::new(0, 0, 0, 0),
                    16
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Position::new(0, 16, 0, 16),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(-std::f64::consts::E)),
                    &Position::new(0, 17, 0, 17),
                    25
                ),
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_binary_numbers() {
        // implicit type, default int
        {
            assert_eq!(
                lex_str_to_vec("+0b101").unwrap(),
                vec![Token::Number(NumberToken::I32(0b101_i32 as u32))]
            );

            assert_eq!(
                lex_str_to_vec("-0b010").unwrap(),
                vec![Token::Number(NumberToken::I32(-0b010_i32 as u32))]
            );

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0b1000_0000_0000_0000__0000_0000_0000_0000"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 43
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0001"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 43
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0b1000_0000_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 15
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0001_i8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 15
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u8"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 7
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec("+0b1000_0000_0000_0000_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0001_i16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u16"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec_with_range("+0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 47
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0001_i32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 47
                    }
                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u32"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
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

            // err: positive overflow
            assert!(matches!(
                lex_str_to_vec_with_range("+0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 89
                    }
                ))
            ));

            // err: negative overflow
            assert!(matches!(
                lex_str_to_vec("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0001_i64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 89
                    }

                ))
            ));

            // err: unsigned number with minus sign
            assert!(matches!(
                lex_str_to_vec("-0b1_u64"),
                Err(Error::MessageWithRange(
                    _,
                    Range {
                        unit: 0,
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // location

            {
                assert_eq!(
                    lex_str_to_vec_with_range("+0b101").unwrap(),
                    vec![TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(0b101_i32 as u32)),
                        &Position::new(0, 0, 0, 0),
                        6
                    )]
                );

                assert_eq!(
                    lex_str_to_vec_with_range("-0b010").unwrap(),
                    vec![TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-0b010_i32 as u32)),
                        &Position::new(0, 0, 0, 0),
                        6
                    )]
                );

                assert_eq!(
                    lex_str_to_vec_with_range("+0b101,-0b010").unwrap(),
                    vec![
                        TokenWithRange::from_position_and_length(
                            Token::Number(NumberToken::I32(0b101_i32 as u32)),
                            &Position::new(0, 0, 0, 0),
                            6
                        ),
                        TokenWithRange::from_position_and_length(
                            Token::Comma,
                            &Position::new(0, 6, 0, 6),
                            1
                        ),
                        TokenWithRange::from_position_and_length(
                            Token::Number(NumberToken::I32(-0b010_i32 as u32)),
                            &Position::new(0, 7, 0, 7),
                            6
                        )
                    ]
                );
            }
        }
    }

    #[test]
    fn test_normalize_signed_integer_overflow_binary() {
        assert!(matches!(
            lex_str_to_vec("0b1000_0000_0000_0000__0000_0000_0000_0000"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 42
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0b1000_0000_i8"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 14
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec("0b1000_0000_0000_0000_i16"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 25
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec_with_range("0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 46
                }
            ))
        ));

        assert!(matches!(
            lex_str_to_vec_with_range("0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64"),
            Err(Error::MessageWithRange(
                _,
                Range {
                    unit: 0,
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 88
                }
            ))
        ));
    }
}
