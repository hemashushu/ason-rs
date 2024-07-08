// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::error::Error;

use super::{
    lexer::{sanitize, lex, Token},
    lookaheaditer::LookaheadIter,
    AsonNode, NameValuePair, VariantItem,
};

pub fn from_str(s: &str) -> Result<AsonNode, Error> {
    let mut chars = s.chars();
    let mut char_iter = LookaheadIter::new(&mut chars, 3);
    let tokens = lex(&mut char_iter)?;
    let effective_tokens = sanitize(tokens)?;
    let mut token_iter = effective_tokens.into_iter();
    let mut lookahead_iter = LookaheadIter::new(&mut token_iter, 2);
    parse(&mut lookahead_iter)
}

pub fn parse(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
    let root = parse_node(iter)?;

    if iter.peek(0).is_some() {
        return Err(Error::Message(
            "The ASON document does not end properly.".to_owned(),
        ));
    }

    Ok(root)
}

fn parse_node(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
    while let Some(current_token) = iter.peek(0) {
        let node = match current_token {
            Token::NewLine => {
                // it is possible to exist a newline token after the ']', '}', ')' punctuations.
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
            Token::Variant(v) => {
                if iter.equals(1, &Token::LeftParen) {
                    parse_variant_item(iter)?
                } else {
                    let v = AsonNode::Variant(VariantItem {
                        fullname: v.to_owned(),
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
            _ => return Err(Error::Message("ASON document syntax error.".to_owned())),
        };

        return Ok(node);
    }

    Err(Error::Message("Incomplete ASON document.".to_owned()))
}

fn parse_variant_item(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
    // name(...)?  //
    // ^        ^__// to here
    // |-----------// current token

    let name = if let Some(Token::Variant(n)) = iter.peek(0) {
        let v = n.to_owned();
        iter.next();
        v
    } else {
        unreachable!()
    };

    consume_left_paren(iter)?;
    consume_new_line_when_exist(iter);

    let value = parse_node(iter)?;
    let variant_item = VariantItem {
        fullname: name,
        value: Some(Box::new(value)),
    };

    consume_new_line_when_exist(iter);
    consume_right_paren(iter)?;

    Ok(AsonNode::Variant(variant_item))
}

fn parse_object(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
    // {...}?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '{'

    let mut entries: Vec<NameValuePair> = vec![];

    loop {
        consume_new_line_when_exist(iter);

        if iter.equals(0, &Token::RightBrace) {
            iter.next(); // consume '}'
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
        let name_value_pair = NameValuePair {
            name,
            value: Box::new(value),
        };
        entries.push(name_value_pair);
    }

    Ok(AsonNode::Object(entries))
}

fn parse_array(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
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

    Ok(AsonNode::Array(items))
}

fn parse_tuple(iter: &mut LookaheadIter<Token>) -> Result<AsonNode, Error> {
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

fn consume_token(iter: &mut LookaheadIter<Token>, expect_token: Token) -> Result<(), Error> {
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
fn consume_colon(iter: &mut LookaheadIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::Colon)
}

fn consume_left_paren(iter: &mut LookaheadIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::LeftParen)
}

fn consume_right_paren(iter: &mut LookaheadIter<Token>) -> Result<(), Error> {
    consume_token(iter, Token::RightParen)
}

// consume '\n'
fn consume_new_line_when_exist(iter: &mut LookaheadIter<Token>) {
    if let Some(Token::NewLine) = iter.peek(0) {
        iter.next();
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use pretty_assertions::assert_eq;

    use crate::{
        error::Error,
        process::{parser::from_str, NameValuePair, NumberLiteral, VariantItem},
    };

    use super::AsonNode;

    #[test]
    fn test_parse_simple_value() {
        assert_eq!(
            from_str(
                r#"
            123
            "#
            )
            .unwrap(),
            AsonNode::Number(NumberLiteral::Int(123))
        );

        assert_eq!(
            from_str(
                r#"
            true
            "#
            )
            .unwrap(),
            AsonNode::Boolean(true)
        );

        assert_eq!(
            from_str(
                r#"
            '🍒'
            "#
            )
            .unwrap(),
            AsonNode::Char('🍒')
        );

        assert_eq!(
            from_str(
                r#"
            "hello"
            "#
            )
            .unwrap(),
            AsonNode::String_("hello".to_owned())
        );

        assert_eq!(
            from_str(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            )
            .unwrap(),
            AsonNode::Date(DateTime::parse_from_rfc3339("2024-03-17 10:01:11+08:00").unwrap())
        );

        // variant
        assert_eq!(
            from_str(
                r#"
            Option::None
            "#
            )
            .unwrap(),
            AsonNode::Variant(VariantItem {
                fullname: "Option::None".to_owned(),
                value: None
            })
        );

        assert_eq!(
            from_str(
                r#"
            Option::Some(123)
            "#
            )
            .unwrap(),
            AsonNode::Variant(VariantItem {
                fullname: "Option::Some".to_owned(),
                value: Some(Box::new(AsonNode::Number(NumberLiteral::Int(123)))),
            })
        );

        assert_eq!(
            from_str(
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
                name: "id".to_owned(),
                value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
            },
            NameValuePair {
                name: "name".to_owned(),
                value: Box::new(AsonNode::String_("foo".to_owned())),
            },
        ]);

        assert_eq!(
            from_str(
                r#"
            {id:123,name:"foo"}
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            from_str(
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
            from_str(
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
            from_str(
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
            from_str(
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
                    name: "id".to_owned(),
                    value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
                },
                NameValuePair {
                    name: "addr".to_owned(),
                    value: Box::new(AsonNode::Variant(VariantItem {
                        fullname: "Option::Some".to_owned(),
                        value: Some(Box::new(AsonNode::Object(vec![
                            NameValuePair {
                                name: "city".to_owned(),
                                value: Box::new(AsonNode::String_("ShenZhen".to_owned())),
                            },
                            NameValuePair {
                                name: "street".to_owned(),
                                value: Box::new(AsonNode::Variant(VariantItem {
                                    fullname: "Option::None".to_owned(),
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
            from_str(
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
    fn test_parse_array() {
        let expect_array1 = AsonNode::Array(vec![
            AsonNode::Number(NumberLiteral::Int(123)),
            AsonNode::Number(NumberLiteral::Int(456)),
            AsonNode::Number(NumberLiteral::Int(789)),
        ]);

        assert_eq!(
            from_str(
                r#"
            [123,456,789]
            "#
            )
            .unwrap(),
            expect_array1
        );

        assert_eq!(
            from_str(
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
            from_str(
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
            from_str(
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
            AsonNode::String_("foo".to_owned()),
            AsonNode::Boolean(true),
        ]);

        assert_eq!(
            from_str(
                r#"
            (123,"foo",true)
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            from_str(
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
            from_str(
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
            from_str(
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
            from_str(
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
                    name: "id".to_owned(),
                    value: Box::new(AsonNode::Number(NumberLiteral::Int(123))),
                },
                NameValuePair {
                    name: "name".to_owned(),
                    value: Box::new(AsonNode::String_("hello".to_owned())),
                },
                NameValuePair {
                    name: "orders".to_owned(),
                    value: Box::new(AsonNode::Array(vec![
                        AsonNode::Tuple(vec![
                            AsonNode::Number(NumberLiteral::Int(1)),
                            AsonNode::String_("foo".to_owned()),
                            AsonNode::Boolean(true),
                        ]),
                        AsonNode::Tuple(vec![
                            AsonNode::Number(NumberLiteral::Int(2)),
                            AsonNode::String_("bar".to_owned()),
                            AsonNode::Boolean(false),
                        ]),
                    ])),
                },
                NameValuePair {
                    name: "group".to_owned(),
                    value: Box::new(AsonNode::Object(vec![
                        NameValuePair {
                            name: "active".to_owned(),
                            value: Box::new(AsonNode::Boolean(true)),
                        },
                        NameValuePair {
                            name: "permissions".to_owned(),
                            value: Box::new(AsonNode::Array(vec![
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(11))),
                                    },
                                    NameValuePair {
                                        name: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("read".to_owned())),
                                    },
                                ]),
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(13))),
                                    },
                                    NameValuePair {
                                        name: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("write".to_owned())),
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
            from_str(
                r#"
                {id: 123, name: "foo"} true
                "#
            ),
            Err(Error::Message(_))
        ));
    }
}
