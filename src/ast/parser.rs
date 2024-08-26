// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{
    error::Error,
    lexer::{CharsWithPositionIter, NumberToken, Token, TokenIter, TokenWithRange},
    location::{Position, Range},
    normalizer::{ClearTokenIter, NormalizedTokenIter, TrimmedTokenIter},
    peekableiter::PeekableIter,
};

use super::{AsonNode, KeyValuePair, Number, Variant};

pub fn parse_from(s: &str) -> Result<AsonNode, Error> {
    let mut chars = s.chars();
    let mut char_position_iter = CharsWithPositionIter::new(0, &mut chars);
    let mut peekable_char_position_iter = PeekableIter::new(&mut char_position_iter, 3);
    let mut token_iter = TokenIter::new(&mut peekable_char_position_iter);
    let mut clear_iter = ClearTokenIter::new(&mut token_iter);
    let mut peekable_clear_iter = PeekableIter::new(&mut clear_iter, 1);
    let mut normalized_iter = NormalizedTokenIter::new(&mut peekable_clear_iter);
    let mut peekable_normalized_iter = PeekableIter::new(&mut normalized_iter, 1);
    let mut trimmed_iter = TrimmedTokenIter::new(&mut peekable_normalized_iter);
    let mut peekable_trimmed_iter = PeekableIter::new(&mut trimmed_iter, 2);

    let root = parse_node(&mut peekable_trimmed_iter)?;

    match peekable_trimmed_iter.peek(0) {
        Some(Ok(TokenWithRange { range, .. })) => Err(Error::MessageWithPosition(
            "The ASON document does not end properly.".to_owned(),
            Position::from_range_start(range),
        )),
        Some(Err(e)) => Err(e.clone()),
        None => {
            // expected
            Ok(root)
        }
    }
}

fn parse_node(iter: &mut PeekableIter<Result<TokenWithRange, Error>>) -> Result<AsonNode, Error> {
    loop {
        match iter.peek(0) {
            Some(Ok(TokenWithRange {
                token: current_token,
                range: current_range,
            })) => {
                let node = match current_token {
                    Token::NewLine => {
                        // it is possible to exist a newline token after the ']', '}' and ')' punctuations.
                        iter.next();
                        continue;
                    }
                    Token::Comma
                        if !matches!(
                            iter.peek(1),
                            Some(Ok(TokenWithRange {
                                token: Token::Comma,
                                ..
                            }))
                        ) =>
                    {
                        // it is possible to exist a comma token after the ']', '}' and ')' punctuations.
                        iter.next();
                        continue;
                    }
                    Token::Number(n) => {
                        let v = convert_number_token(*n);
                        iter.next();
                        v
                    }
                    Token::Boolean(b) => {
                        let v = AsonNode::Boolean(*b);
                        iter.next();
                        v
                    }
                    Token::Char(c) => {
                        let v = AsonNode::Char(*c);
                        iter.next();
                        v
                    }
                    Token::String_(s) => {
                        let v = AsonNode::String_(s.to_owned());
                        iter.next();
                        v
                    }
                    Token::Date(d) => {
                        let v = AsonNode::DateTime(*d);
                        iter.next();
                        v
                    }
                    Token::Variant(type_name, member_name) => {
                        match iter.peek(1) {
                            Some(Ok(TokenWithRange {
                                token: Token::LeftParen,
                                ..
                            })) => {
                                // tuple variant or the new type variant (i.e. single value variant)
                                parse_tuple_variant(iter)?
                            }
                            Some(Ok(TokenWithRange {
                                token: Token::LeftBrace,
                                ..
                            })) => {
                                // struct variant
                                parse_struct_variant(iter)?
                            }
                            // note: let the next loop to handle the error
                            _ => {
                                // unit variant
                                let v = AsonNode::Variant(Variant::new(type_name, member_name));
                                iter.next();
                                v
                            }
                        }
                    }
                    Token::ByteData(b) => {
                        let v = AsonNode::ByteData(b.to_owned());
                        iter.next();
                        v
                    }
                    Token::LeftBrace => {
                        // object: {...}
                        parse_object(iter)?
                    }
                    Token::LeftBracket => {
                        // array: [...]
                        parse_array(iter)?
                    }
                    Token::LeftParen => {
                        // tuple: (...)
                        parse_tuple(iter)?
                    }
                    _ => {
                        return Err(Error::MessageWithPosition(
                            "Syntax error, unexpected token.".to_owned(),
                            Position::from_range_start(current_range),
                        ))
                    }
                };

                return Ok(node);
            }
            Some(Err(e)) => {
                return Err(e.clone());
            }
            None => {
                return Err(Error::Message(
                    "Incomplete document, unexpected to reach the end of document.".to_owned(),
                ));
            }
        }
    }
}

fn convert_number_token(token: NumberToken) -> AsonNode {
    let number = match token {
        NumberToken::I8(v) => Number::I8(v as i8),
        NumberToken::U8(v) => Number::U8(v),
        NumberToken::I16(v) => Number::I16(v as i16),
        NumberToken::U16(v) => Number::U16(v),
        NumberToken::I32(v) => Number::I32(v as i32),
        NumberToken::U32(v) => Number::U32(v),
        NumberToken::I64(v) => Number::I64(v as i64),
        NumberToken::U64(v) => Number::U64(v),
        NumberToken::F32(v) => Number::F32(v),
        NumberToken::F64(v) => Number::F64(v),
    };

    AsonNode::Number(number)
}

fn parse_tuple_variant(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
) -> Result<AsonNode, Error> {
    // type::member(...)?  //
    // ^           ^    ^__// to here
    // |           |-------// left paren, validated
    // |-------------------// current token, validated

    // consume variant token
    let (type_name, member_name, next_position) = if let Some(Ok(TokenWithRange {
        token: Token::Variant(type_name, member_name),
        range,
    })) = iter.next()
    {
        (type_name, member_name, Position::from_range_end(&range))
    } else {
        unreachable!()
    };

    consume_left_paren(iter, &next_position)?;

    // consume_new_line_or_comma_if_exist_ignore_position(iter);

    let mut items = vec![];

    // let value = parse_node(iter)?;
    // values.push(value);

    loop {
        consume_new_line_or_comma_if_exist_ignore_position(iter);

        // match iter.peek(0) {
        //     Some(Ok(TokenWithRange {
        //         token: Token::RightParen,
        //         ..
        //     })) => {
        //         break;
        //     }
        //     _ => {
        //         let value = parse_node(iter)?;
        //         values.push(value);
        //         consume_new_line_or_comma_if_exist_ignore_position(iter);
        //     }
        // }

        if matches!(
            iter.peek(0),
            Some(Ok(TokenWithRange {
                token: Token::RightParen,
                ..
            }))
        ) {
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    // consume ')'
    let next_position = if let Some(Ok(TokenWithRange { range, .. })) = iter.next() {
        Position::from_range_start(&range)
    } else {
        unreachable!()
    };

    let variant_item = match items.len() {
        0 => {
            return Err(Error::MessageWithPosition(
                "The values of tuple variant can not be empty.".to_owned(),
                next_position,
            ));
        }
        1 => Variant::with_value(&type_name, &member_name, items.remove(0)),
        _ => Variant::with_values(&type_name, &member_name, items),
    };

    // iter.next(); // consume ')'

    Ok(AsonNode::Variant(variant_item))
}

fn parse_struct_variant(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
) -> Result<AsonNode, Error> {
    // type::member{...}?  //
    // ^           ^    ^__// to here
    // |           |_______// left brace, validated
    // |-------------------// current token, validated

    let (type_name, member_name) = if let Some(Ok(TokenWithRange {
        token: Token::Variant(type_name, member_name),
        ..
    })) = iter.next()
    {
        (type_name, member_name)
    } else {
        unreachable!()
    };

    let kvps = parse_key_value_pairs(iter)?;

    Ok(AsonNode::Variant(Variant::with_object(
        &type_name,
        &member_name,
        kvps,
    )))
}

fn parse_key_value_pairs(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
) -> Result<Vec<KeyValuePair>, Error> {
    // {...}?  //
    // ^    ^__// to here
    // |-------// current token, validated

    // consume '{'
    iter.next();

    let mut kvps: Vec<KeyValuePair> = vec![];

    loop {
        consume_new_line_or_comma_if_exist_ignore_position(iter);

        if matches!(
            iter.peek(0),
            Some(Ok(TokenWithRange {
                token: Token::RightBrace,
                ..
            }))
        ) {
            break;
        }

        let (name, mut next_position) =
            if let Some(Ok(TokenWithRange { token, range })) = iter.peek(0) {
                if let Token::Identifier(n) = token {
                    let name = n.to_owned();
                    let next_position = Position::from_range_end(range);
                    iter.next();
                    (name, next_position)
                } else {
                    return Err(Error::MessageWithPosition(
                        "Expect a key name for the object.".to_owned(),
                        Position::from_range_start(range),
                    ));
                }
            } else {
                return Err(Error::Message(
                    "Incomplete object, unexpected to reach the end of document.".to_owned(),
                ));
            };

        next_position = consume_new_line_or_comma_if_exist(iter, &next_position);
        consume_colon(iter, &next_position)?;
        consume_new_line_or_comma_if_exist_ignore_position(iter);

        let value = parse_node(iter)?;
        let name_value_pair = KeyValuePair {
            key: name,
            value: Box::new(value),
        };
        kvps.push(name_value_pair);
    }

    iter.next(); // consume '}'

    Ok(kvps)
}

fn parse_object(iter: &mut PeekableIter<Result<TokenWithRange, Error>>) -> Result<AsonNode, Error> {
    let kvps = parse_key_value_pairs(iter)?;
    Ok(AsonNode::Object(kvps))
}

fn parse_array(iter: &mut PeekableIter<Result<TokenWithRange, Error>>) -> Result<AsonNode, Error> {
    // [...]?  //
    // ^    ^__// to here
    // |-------// current token, validated

    iter.next(); // consume '['

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_or_comma_if_exist_ignore_position(iter);

        if matches!(
            iter.peek(0),
            Some(Ok(TokenWithRange {
                token: Token::RightBracket,
                ..
            }))
        ) {
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    iter.next(); // consume ']'

    Ok(AsonNode::List(items))
}

fn parse_tuple(iter: &mut PeekableIter<Result<TokenWithRange, Error>>) -> Result<AsonNode, Error> {
    // (...)?  //
    // ^    ^__// to here
    // |-------// current token, validated

    iter.next(); // consume '('

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_or_comma_if_exist_ignore_position(iter);

        if matches!(
            iter.peek(0),
            Some(Ok(TokenWithRange {
                token: Token::RightParen,
                ..
            }))
        ) {
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    // consume ')'
    let next_position = if let Some(Ok(TokenWithRange { range, .. })) = iter.next() {
        Position::from_range_start(&range)
    } else {
        unreachable!()
    };

    if items.is_empty() {
        Err(Error::MessageWithPosition(
            "Tuple can not be empty.".to_owned(),
            next_position,
        ))
    } else {
        Ok(AsonNode::Tuple(items))
    }
}

fn consume_token(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
    expect_token: &Token,
    expect_position: &Position,
) -> Result<Position, Error> {
    match iter.next() {
        Some(Ok(TokenWithRange { token, range })) => {
            if &token == expect_token {
                Ok(Position::from_range_end(&range))
            } else {
                Err(Error::MessageWithPosition(
                    format!(
                        "Expect token: {:?}, actual token: {:?}",
                        expect_token, token
                    ),
                    *expect_position,
                ))
            }
        }
        Some(Err(e)) => Err(e),
        None => Err(Error::MessageWithPosition(
            format!("Missing token: {:?}", expect_token),
            *expect_position,
        )),
    }
}

// consume ':'
fn consume_colon(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
    expect_position: &Position,
) -> Result<Position, Error> {
    consume_token(iter, &Token::Colon, expect_position)
}

fn consume_left_paren(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
    expect_position: &Position,
) -> Result<Position, Error> {
    consume_token(iter, &Token::LeftParen, expect_position)
}

fn consume_right_paren(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
    expect_position: &Position,
) -> Result<Position, Error> {
    consume_token(iter, &Token::RightParen, expect_position)
}

// consume '\n' or ',' if they exist.
fn consume_new_line_or_comma_if_exist(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
    expect_position: &Position,
) -> Position {
    match iter.peek(0) {
        Some(Ok(TokenWithRange {
            token: Token::NewLine | Token::Comma,
            range,
        })) => {
            let position = Position::from_range_end(range);
            iter.next();
            position
        }
        _ => *expect_position,
    }
}

fn consume_new_line_or_comma_if_exist_ignore_position(
    iter: &mut PeekableIter<Result<TokenWithRange, Error>>,
) {
    if matches!(
        iter.peek(0),
        Some(Ok(TokenWithRange {
            token: Token::NewLine | Token::Comma,
            ..
        }))
    ) {
        iter.next();
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{parser::parse_from, KeyValuePair, Number, Variant},
        error::Error,
        location::Position,
    };

    use super::AsonNode;

    #[test]
    fn test_parse_simple_value() {
        assert_eq!(
            parse_from(
                r#"
            123
            "#
            )
            .unwrap(),
            AsonNode::Number(Number::I32(123))
        );

        assert_eq!(
            parse_from(
                r#"
            true
            "#
            )
            .unwrap(),
            AsonNode::Boolean(true)
        );

        assert_eq!(
            parse_from(
                r#"
            '🍒'
            "#
            )
            .unwrap(),
            AsonNode::Char('🍒')
        );

        assert_eq!(
            parse_from(
                r#"
            "hello"
            "#
            )
            .unwrap(),
            AsonNode::String_("hello".to_owned())
        );

        assert_eq!(
            parse_from(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            )
            .unwrap(),
            AsonNode::DateTime(DateTime::parse_from_rfc3339("2024-03-17 10:01:11+08:00").unwrap())
        );
    }

    #[test]
    fn test_parse_byte_data() {
        assert_eq!(
            parse_from(
                r#"
            h"11 13 17 19"
            "#
            )
            .unwrap(),
            AsonNode::ByteData(vec![0x11u8, 0x13, 0x17, 0x19])
        );
    }

    #[test]
    fn test_parse_variant() {
        // empty value
        assert_eq!(
            parse_from(
                r#"
            Option::None
            "#
            )
            .unwrap(),
            AsonNode::Variant(Variant::new("Option", "None"))
        );

        // single value
        assert_eq!(
            parse_from(
                r#"
            Option::Some(123)
            "#
            )
            .unwrap(),
            AsonNode::Variant(Variant::with_value(
                "Option",
                "Some",
                AsonNode::Number(Number::I32(123))
            ))
        );

        // multiple values
        assert_eq!(
            parse_from(
                r#"
            Color::RGB(100,75,0)
            "#
            )
            .unwrap(),
            AsonNode::Variant(Variant::with_values(
                "Color",
                "RGB",
                vec![
                    AsonNode::Number(Number::I32(100)),
                    AsonNode::Number(Number::I32(75)),
                    AsonNode::Number(Number::I32(0)),
                ]
            ))
        );

        // object value
        assert_eq!(
            parse_from(
                r#"
            Shape::Rect{width:123, height:456}
            "#
            )
            .unwrap(),
            AsonNode::Variant(Variant::with_object(
                "Shape",
                "Rect",
                vec![
                    KeyValuePair::new("width", AsonNode::Number(Number::I32(123))),
                    KeyValuePair::new("height", AsonNode::Number(Number::I32(456))),
                ]
            ))
        );

        // err: missing value(s)
        assert!(matches!(
            parse_from(r#"Option::Some()"#),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 13,
                    line: 0,
                    column: 13
                }
            ))
        ));

        // err: missing ')'
        assert!(matches!(
            parse_from(r#"Color::RGB(11,13"#),
            Err(Error::Message(_))
        ));

        // err: missing '}'
        assert!(matches!(
            parse_from(r#"Color::Rect{width:11"#),
            Err(Error::Message(_))
        ));
    }

    #[test]
    fn test_parse_object() {
        let expect_object1 = AsonNode::Object(vec![
            KeyValuePair {
                key: "id".to_owned(),
                value: Box::new(AsonNode::Number(Number::I32(123))),
            },
            KeyValuePair {
                key: "name".to_owned(),
                value: Box::new(AsonNode::String_("foo".to_owned())),
            },
        ]);

        assert_eq!(
            parse_from(
                r#"
            {id:123,name:"foo"}
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse_from(
                r#"
            {
                id:123
                name:"foo"
            }
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse_from(
                r#"
            {
                id:123,
                name:"foo"
            }
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse_from(
                r#"
            {
                id: 123,
                name: "foo",
            }
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse_from(
                r#"
            {
                id: 123
                addr: Option::Some({
                    city: "ShenZhen"
                    street: Option::None
                })
            }
            "#
            )
            .unwrap(),
            AsonNode::Object(vec![
                KeyValuePair {
                    key: "id".to_owned(),
                    value: Box::new(AsonNode::Number(Number::I32(123))),
                },
                KeyValuePair {
                    key: "addr".to_owned(),
                    value: Box::new(AsonNode::Variant(Variant::with_value(
                        "Option",
                        "Some",
                        AsonNode::Object(vec![
                            KeyValuePair {
                                key: "city".to_owned(),
                                value: Box::new(AsonNode::String_("ShenZhen".to_owned())),
                            },
                            KeyValuePair {
                                key: "street".to_owned(),
                                value: Box::new(AsonNode::Variant(Variant::new("Option", "None"))),
                            },
                        ])
                    ))),
                },
            ])
        );

        // err: incorrect key name (should be without quote)
        assert!(matches!(
            parse_from(
                r#"{
    "id": 123,
    "name": "foo",
}"#
            ),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 6,
                    line: 1,
                    column: 4
                }
            ))
        ));

        // err: missing key name
        assert!(matches!(
            parse_from(r#"{123}"#),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 1,
                    line: 0,
                    column: 1
                }
            ))
        ));

        // err: missing '}'
        assert!(matches!(parse_from(r#"{id:123"#), Err(Error::Message(_))));
    }

    #[test]
    fn test_parse_list() {
        let expect_list1 = AsonNode::List(vec![
            AsonNode::Number(Number::I32(123)),
            AsonNode::Number(Number::I32(456)),
            AsonNode::Number(Number::I32(789)),
        ]);

        assert_eq!(
            parse_from(
                r#"
            [123,456,789]
            "#
            )
            .unwrap(),
            expect_list1
        );

        assert_eq!(
            parse_from(
                r#"
            [
                123
                456
                789
            ]
            "#
            )
            .unwrap(),
            expect_list1
        );

        assert_eq!(
            parse_from(
                r#"
            [
                123,
                456,
                789
            ]
            "#
            )
            .unwrap(),
            expect_list1
        );

        assert_eq!(
            parse_from(
                r#"
            [
                123,
                456,
                789,
            ]
            "#
            )
            .unwrap(),
            expect_list1
        );

        // err: missing ']'
        assert!(matches!(parse_from(r#"[123,"#), Err(Error::Message(_))));
    }

    #[test]
    fn test_parse_tuple() {
        let expect_tuple1 = AsonNode::Tuple(vec![
            AsonNode::Number(Number::I32(123)),
            AsonNode::String_("foo".to_owned()),
            AsonNode::Boolean(true),
        ]);

        assert_eq!(
            parse_from(
                r#"
            (123,"foo",true)
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            parse_from(
                r#"
            (
                123
                "foo"
                true
            )
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            parse_from(
                r#"
            (
                123,
                "foo",
                true
            )
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            parse_from(
                r#"
            (
                123,
                "foo",
                true,
            )
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        // err: empty tuple
        assert!(matches!(
            parse_from(r#"()"#),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 1,
                    line: 0,
                    column: 1
                }
            ))
        ));

        // err: missing ')'
        assert!(matches!(parse_from(r#"(123,"#), Err(Error::Message(_))));
    }

    #[test]
    fn test_parse_complex() {
        assert_eq!(
            parse_from(
                r#"
            {
                id:123
                name:"hello"
                orders: [
                    (1, "foo", true)
                    (2, "bar", false)
                ]
                group: {
                    active: true
                    permissions:[
                        {number:11, title: "read"}
                        {number:13, title: "write"}
                    ]
                }
            }
            "#
            )
            .unwrap(),
            AsonNode::Object(vec![
                KeyValuePair {
                    key: "id".to_owned(),
                    value: Box::new(AsonNode::Number(Number::I32(123))),
                },
                KeyValuePair {
                    key: "name".to_owned(),
                    value: Box::new(AsonNode::String_("hello".to_owned())),
                },
                KeyValuePair {
                    key: "orders".to_owned(),
                    value: Box::new(AsonNode::List(vec![
                        AsonNode::Tuple(vec![
                            AsonNode::Number(Number::I32(1)),
                            AsonNode::String_("foo".to_owned()),
                            AsonNode::Boolean(true),
                        ]),
                        AsonNode::Tuple(vec![
                            AsonNode::Number(Number::I32(2)),
                            AsonNode::String_("bar".to_owned()),
                            AsonNode::Boolean(false),
                        ]),
                    ])),
                },
                KeyValuePair {
                    key: "group".to_owned(),
                    value: Box::new(AsonNode::Object(vec![
                        KeyValuePair {
                            key: "active".to_owned(),
                            value: Box::new(AsonNode::Boolean(true)),
                        },
                        KeyValuePair {
                            key: "permissions".to_owned(),
                            value: Box::new(AsonNode::List(vec![
                                AsonNode::Object(vec![
                                    KeyValuePair {
                                        key: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(Number::I32(11))),
                                    },
                                    KeyValuePair {
                                        key: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("read".to_owned())),
                                    },
                                ]),
                                AsonNode::Object(vec![
                                    KeyValuePair {
                                        key: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(Number::I32(13))),
                                    },
                                    KeyValuePair {
                                        key: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("write".to_owned())),
                                    },
                                ]),
                            ])),
                        },
                    ])),
                },
            ])
        );

        // err: document does not end properly
        assert!(matches!(
            parse_from(r#"true false"#),
            Err(Error::MessageWithPosition(
                _,
                Position {
                    unit: 0,
                    index: 5,
                    line: 0,
                    column: 5
                }
            ))
        ));
    }
}
