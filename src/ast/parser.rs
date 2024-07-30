// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{
    error::Error,
    lexer::{lex, normalize, NumberToken, Token},
    forwarditer::ForwardIter,
};

use super::{AsonNode, KeyValuePair, Number, Variant};

pub fn parse_from(s: &str) -> Result<AsonNode, Error> {
    let mut chars = s.chars();
    let mut char_iter = ForwardIter::new(&mut chars, 3);
    let tokens = lex(&mut char_iter)?;
    let normalized_tokens = normalize(tokens)?;
    let mut token_iter = normalized_tokens.into_iter();
    let mut lookahead_iter = ForwardIter::new(&mut token_iter, 2);

    let root = parse_node(&mut lookahead_iter)?;

    if lookahead_iter.peek(0).is_some() {
        return Err(Error::Message(
            "The ASON document does not end properly.".to_owned(),
        ));
    }

    Ok(root)
}

fn parse_node(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    while let Some(current_token) = iter.peek(0) {
        let node = match current_token {
            Token::NewLine => {
                // it is possible to exist a newline token after the ']', '}', ')' punctuations.
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
                if iter.equals(1, &Token::LeftParen) {
                    // tuple variant (includes the new type variant)
                    parse_tuple_variant(iter)?
                } else if iter.equals(1, &Token::LeftBrace) {
                    // struct variant
                    parse_struct_variant(iter)?
                } else {
                    // unit variant
                    let v = AsonNode::Variant(Variant::new(type_name, member_name));
                    iter.next();
                    v
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
            _ => return Err(Error::Message("ASON document syntax error.".to_owned())),
        };

        return Ok(node);
    }

    Err(Error::Message("Incomplete ASON document.".to_owned()))
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

fn parse_tuple_variant(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    // type::member(...)?  //
    // ^                ^__// to here
    // |-------------------// current token

    let (type_name, member_name) =
        if let Some(Token::Variant(type_name, member_name)) = iter.peek(0) {
            let v = (type_name.to_owned(), member_name.to_owned());
            iter.next();
            v
        } else {
            unreachable!()
        };

    consume_left_paren(iter)?;
    consume_new_line_when_exist(iter);

    let mut values = vec![];

    let value = parse_node(iter)?;
    values.push(value);

    consume_new_line_when_exist(iter);

    while !iter.equals(0, &Token::RightParen) {
        let value = parse_node(iter)?;
        values.push(value);
        consume_new_line_when_exist(iter);
    }

    let variant_item = if values.len() == 1 {
        Variant::with_value(&type_name, &member_name, values.remove(0))
    } else {
        Variant::with_values(&type_name, &member_name, values)
    };

    consume_right_paren(iter)?;

    Ok(AsonNode::Variant(variant_item))
}

fn parse_struct_variant(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    // type::member{...}?  //
    // ^                ^__// to here
    // |-------------------// current token

    let (type_name, member_name) =
        if let Some(Token::Variant(type_name, member_name)) = iter.peek(0) {
            let v = (type_name.to_owned(), member_name.to_owned());
            iter.next();
            v
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

fn parse_key_value_pairs(iter: &mut ForwardIter<Token>) -> Result<Vec<KeyValuePair>, Error> {
    // {...}?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '{'

    let mut kvps: Vec<KeyValuePair> = vec![];

    loop {
        consume_new_line_when_exist(iter);

        if iter.equals(0, &Token::RightBrace) {
            break;
        }

        let name = if let Some(Token::Identifier(n)) = iter.peek(0) {
            let v = n.to_owned();
            iter.next();
            v
        } else {
            return Err(Error::Message(
                "Expect a key name for the object.".to_owned(),
            ));
        };

        consume_new_line_when_exist(iter);
        consume_colon(iter)?;
        consume_new_line_when_exist(iter);

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

fn parse_object(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    let kvps = parse_key_value_pairs(iter)?;
    Ok(AsonNode::Object(kvps))
}

fn parse_array(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    // [...]?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '['

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_when_exist(iter);

        if iter.equals(0, &Token::RightBracket) {
            iter.next(); // consume ']'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    Ok(AsonNode::List(items))
}

fn parse_tuple(iter: &mut ForwardIter<Token>) -> Result<AsonNode, Error> {
    // (...)?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '('

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_when_exist(iter);

        if iter.equals(0, &Token::RightParen) {
            iter.next(); // consume ')'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    Ok(AsonNode::Tuple(items))
}

fn consume_token(iter: &mut ForwardIter<Token>, expect_token: Token) -> Result<(), Error> {
    let opt_token = iter.next();
    if let Some(token) = opt_token {
        if token == expect_token {
            Ok(())
        } else {
            Err(Error::Message(format!(
                "Expect token: {:?}, actual token: {:?}",
                expect_token, token
            )))
        }
    } else {
        Err(Error::Message(format!("Missing token: {:?}", expect_token)))
    }
}

// consume ':'
fn consume_colon(iter: &mut ForwardIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::Colon)
}

fn consume_left_paren(iter: &mut ForwardIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::LeftParen)
}

fn consume_right_paren(iter: &mut ForwardIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::RightParen)
}

// consume '\n'
fn consume_new_line_when_exist(iter: &mut ForwardIter<Token>) {
    if let Some(Token::NewLine) = iter.peek(0) {
        iter.next();
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{KeyValuePair, Number, Variant},
        error::Error,
        parse_from,
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

        // err: key name with quote
        assert!(matches!(
            parse_from(
                r#"
            {
                "id": 123,
                "name": "foo",
            }
            "#
            ),
            Err(Error::Message(_))
        ));
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
    }

    #[test]
    fn test_parse_compound() {
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
            parse_from(
                r#"
                {id: 123, name: "foo"} true
                "#
            ),
            Err(Error::Message(_))
        ));
    }
}
