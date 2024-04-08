// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{lexer::Token, peekable_iterator::PeekableIterator, AsonNode, NameValuePair, ParseError, VariantItem};

pub fn parse(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    let root = parse_node(iter)?;

    if iter.peek(0).is_some() {
        return Err(ParseError::new("The ASON document does not end properly."));
    }

    Ok(root)
}

fn parse_node(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    while let Some(current_token) = iter.peek(0) {
        let node = match current_token {
            Token::NewLine => {
                // it is possible to have a newline token after the ']', '}', ')' symbols.
                iter.next();
                continue;
            }
            Token::Number(n) => {
                let v = AsonNode::Number(*n);
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
                let v = AsonNode::Date(*d);
                iter.next();
                v
            }
            Token::VariantName(v) => {
                if iter.look_ahead_equals(1, &Token::LeftParen) {
                    parse_variant_item(iter)?
                } else {
                    let v = AsonNode::Variant(VariantItem {
                        name: v.to_owned(),
                        value: None,
                    });
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
            _ => return Err(ParseError::new("ASON document syntax error.")),
        };

        return Ok(node);
    }

    Err(ParseError::new("Incomplete ASON document."))
}

fn parse_variant_item(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    // name(...)?  //
    // ^        ^__// to here
    // |-----------// current token

    let name = if let Some(Token::VariantName(n)) = iter.peek(0) {
        let v = n.to_owned();
        iter.next();
        v
    } else {
        unreachable!()
    };

    consume_left_paren(iter)?;
    consume_new_line_if_exist(iter);

    let value = parse_node(iter)?;
    let variant_item = VariantItem {
        name,
        value: Some(Box::new(value)),
    };

    consume_new_line_if_exist(iter);
    consume_right_paren(iter)?;

    Ok(AsonNode::Variant(variant_item))
}

fn parse_object(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    // {...}?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '{'

    let mut entries: Vec<NameValuePair> = vec![];

    loop {
        consume_new_line_if_exist(iter);

        if iter.look_ahead_equals(0, &Token::RightBrace) {
            iter.next(); // consume '}'
            break;
        }

        let name = if let Some(Token::KeyName(n)) = iter.peek(0) {
            let v = n.to_owned();
            iter.next();
            v
        } else {
            return Err(ParseError::new("Expect a key name for the object."));
        };

        consume_new_line_if_exist(iter);
        consume_colon(iter)?;
        consume_new_line_if_exist(iter);

        let value = parse_node(iter)?;
        let name_value_pair = NameValuePair {
            name,
            value: Box::new(value),
        };
        entries.push(name_value_pair);
    }

    Ok(AsonNode::Object(entries))
}

fn parse_array(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    // [...]?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '['

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_if_exist(iter);

        if iter.look_ahead_equals(0, &Token::RightBracket) {
            iter.next(); // consume ']'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    Ok(AsonNode::Array(items))
}

fn parse_tuple(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    // (...)?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '('

    let mut items: Vec<AsonNode> = vec![];

    loop {
        consume_new_line_if_exist(iter);

        if iter.look_ahead_equals(0, &Token::RightParen) {
            iter.next(); // consume ')'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);
    }

    Ok(AsonNode::Tuple(items))
}

fn consume_token(
    iter: &mut PeekableIterator<Token>,
    expect_token: Token,
) -> Result<(), ParseError> {
    let opt_token = iter.next();
    if let Some(token) = opt_token {
        if token == expect_token {
            Ok(())
        } else {
            Err(ParseError::new(&format!(
                "Expect token: {:?}, actual token: {:?}",
                expect_token, token
            )))
        }
    } else {
        Err(ParseError::new(&format!(
            "Missing token: {:?}",
            expect_token
        )))
    }
}

// consume ':'
fn consume_colon(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::Colon)
}

fn consume_left_paren(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::LeftParen)
}

fn consume_right_paren(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::RightParen)
}

fn consume_new_line_if_exist(iter: &mut PeekableIterator<Token>) {
    if let Some(Token::NewLine) = iter.peek(0) {
        iter.next(); // consume '\n'
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;

    use crate::{parse, parser::NameValuePair, NumberLiteral, ParseError, VariantItem};

    use super::AsonNode;

    #[test]
    fn test_parse_simple_value() {
        assert_eq!(
            parse(
                r#"
            123
            "#
            )
                .unwrap(),
            AsonNode::Number(NumberLiteral::Int(123))
        );

        assert_eq!(
            parse(
                r#"
            true
            "#
            )
                .unwrap(),
            AsonNode::Boolean(true)
        );

        assert_eq!(
            parse(
                r#"
            '🍒'
            "#
            )
                .unwrap(),
            AsonNode::Char('🍒')
        );

        assert_eq!(
            parse(
                r#"
            "hello"
            "#
            )
                .unwrap(),
            AsonNode::String_("hello".to_string())
        );

        assert_eq!(
            parse(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            )
                .unwrap(),
            AsonNode::Date(DateTime::parse_from_rfc3339("2024-03-17 10:01:11+08:00").unwrap())
        );

        // variant
        assert_eq!(
            parse(
                r#"
            Option::None
            "#
            )
                .unwrap(),
            AsonNode::Variant(VariantItem { name: "Option::None".to_string(), value: None })
        );

        assert_eq!(
            parse(
                r#"
            Option::Some(123)
            "#
            )
                .unwrap(),
            AsonNode::Variant(VariantItem {
                name: "Option::Some".to_string(),
                value: Some(Box::new(
                    AsonNode::Number(NumberLiteral::Int(123))
                )),
            })
        );

        assert_eq!(
            parse(
                r#"
            h"11:13:17:19"
            "#
            )
                .unwrap(),
            AsonNode::ByteData(vec![0x11u8, 0x13, 0x17, 0x19])
        );
    }

    #[test]
    fn test_parse_object() {
        let expect_object1 = AsonNode::Object(vec![
            NameValuePair {
                name: "id".to_string(),
                value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
            },
            NameValuePair {
                name: "name".to_string(),
                value: Box::new(AsonNode::String_("foo".to_string())),
            },
        ]);

        assert_eq!(
            parse(
                r#"
            {id:123,name:"foo"}
            "#
            )
                .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse(
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
            parse(
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
            parse(
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
            parse(
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
                NameValuePair {
                    name: "id".to_string(),
                    value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
                },
                NameValuePair {
                    name: "addr".to_string(),
                    value: Box::new(AsonNode::Variant(VariantItem {
                        name: "Option::Some".to_string(),
                        value: Some(Box::new(AsonNode::Object(vec![
                            NameValuePair {
                                name: "city".to_string(),
                                value: Box::new(AsonNode::String_("ShenZhen".to_string())),
                            },
                            NameValuePair {
                                name: "street".to_string(),
                                value: Box::new(AsonNode::Variant(VariantItem {
                                    name: "Option::None".to_string(),
                                    value: None,
                                })),
                            },
                        ]))),
                    })),
                },
            ])
        );

        // err: key name with quote
        assert!(matches!(
            parse(
                r#"
            {
                "id": 123,
                "name": "foo",
            }
            "#
            ),
            Err(ParseError { message: _ })
        ));
    }

    #[test]
    fn test_parse_array() {
        let expect_array1 = AsonNode::Array(vec![
            AsonNode::Number(NumberLiteral::Int(123)),
            AsonNode::Number(NumberLiteral::Int(456)),
            AsonNode::Number(NumberLiteral::Int(789)),
        ]);

        assert_eq!(
            parse(
                r#"
            [123,456,789]
            "#
            )
                .unwrap(),
            expect_array1
        );

        assert_eq!(
            parse(
                r#"
            [
                123
                456
                789
            ]
            "#
            )
                .unwrap(),
            expect_array1
        );

        assert_eq!(
            parse(
                r#"
            [
                123,
                456,
                789
            ]
            "#
            )
                .unwrap(),
            expect_array1
        );

        assert_eq!(
            parse(
                r#"
            [
                123,
                456,
                789,
            ]
            "#
            )
                .unwrap(),
            expect_array1
        );
    }

    #[test]
    fn test_parse_tuple() {
        let expect_tuple1 = AsonNode::Tuple(vec![
            AsonNode::Number(NumberLiteral::Int(123)),
            AsonNode::String_("foo".to_string()),
            AsonNode::Boolean(true),
        ]);

        assert_eq!(
            parse(
                r#"
            (123,"foo",true)
            "#
            )
                .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            parse(
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
            parse(
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
            parse(
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
    fn test_parse_complex() {
        assert_eq!(
            parse(
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
                NameValuePair {
                    name: "id".to_string(),
                    value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
                },
                NameValuePair {
                    name: "name".to_string(),
                    value: Box::new(AsonNode::String_("hello".to_string())),
                },
                NameValuePair {
                    name: "orders".to_string(),
                    value: Box::new(AsonNode::Array(vec![
                        AsonNode::Tuple(vec![
                            AsonNode::Number(NumberLiteral::Int(1)),
                            AsonNode::String_("foo".to_string()),
                            AsonNode::Boolean(true),
                        ]),
                        AsonNode::Tuple(vec![
                            AsonNode::Number(NumberLiteral::Int(2)),
                            AsonNode::String_("bar".to_string()),
                            AsonNode::Boolean(false),
                        ]),
                    ])),
                },
                NameValuePair {
                    name: "group".to_string(),
                    value: Box::new(AsonNode::Object(vec![
                        NameValuePair {
                            name: "active".to_string(),
                            value: Box::new(AsonNode::Boolean(true)),
                        },
                        NameValuePair {
                            name: "permissions".to_string(),
                            value: Box::new(AsonNode::Array(vec![
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_string(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(11))),
                                    },
                                    NameValuePair {
                                        name: "title".to_string(),
                                        value: Box::new(AsonNode::String_("read".to_string())),
                                    },
                                ]),
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_string(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(13))),
                                    },
                                    NameValuePair {
                                        name: "title".to_string(),
                                        value: Box::new(AsonNode::String_("write".to_string())),
                                    },
                                ]),
                            ])),
                        },
                    ])),
                },
            ])
        );

        // err: does not end properly
        assert!(matches!(
            parse(
                r#"
                {id: 123, name: "foo"} true
                "#
            ),
            Err(ParseError { message: _ })
        ));
    }
}
