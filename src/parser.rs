// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{
    lexer::Token, peekable_iterator::PeekableIterator, AsonNode, NameValuePair, ParseError,
};

pub fn parse(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    let root = parse_node(iter)?;
    while let Some(current_token) = iter.peek(0) {
        match current_token {
            Token::Comment(_) | Token::NewLine => {
                iter.next();
            }
            _ => return Err(ParseError::new("The ASON document does not end properly.")),
        }
    }
    Ok(root)
}

fn parse_node(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    while let Some(current_token) = iter.peek(0) {
        let node = match current_token {
            Token::Comment(_) => {
                iter.next();
                continue;
            }
            Token::NewLine => {
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

fn parse_object(iter: &mut PeekableIterator<Token>) -> Result<AsonNode, ParseError> {
    // {...}?  //
    // ^    ^__// to here
    // |-------// current char

    iter.next(); // consume '{'

    let mut entries: Vec<NameValuePair> = vec![];

    loop {
        skip_new_lines(iter);

        if iter.look_ahead_equals(0, &Token::RightBrace) {
            iter.next(); // consume '}'
            break;
        }

        let name = match iter.peek(0) {
            Some(Token::Name(n)) => {
                let v = n.to_owned();
                iter.next();
                v
            }
            // Some(Token::String_(s)) => {
            //     let v = s.to_owned();
            //     iter.next();
            //     v
            // }
            _ => return Err(ParseError::new("Expect a key name for the object.")),
        };

        skip_new_lines(iter);
        consume_colon(iter)?;
        let value = parse_node(iter)?;
        let name_value = NameValuePair {
            name,
            value: Box::new(value),
        };
        entries.push(name_value);

        let exist_splitter = consume_optional_splitter(iter);

        if !exist_splitter {
            skip_new_lines(iter);
            consume_right_brace(iter)?;
            break;
        }
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
        skip_new_lines(iter);

        if iter.look_ahead_equals(0, &Token::RightBracket) {
            iter.next(); // consume ']'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);

        let exist_splitter = consume_optional_splitter(iter);

        if !exist_splitter {
            skip_new_lines(iter);
            consume_right_bracket(iter)?;
            break;
        }
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
        skip_new_lines(iter);

        if iter.look_ahead_equals(0, &Token::RightParen) {
            iter.next(); // consume ')'
            break;
        }

        let value = parse_node(iter)?;
        items.push(value);

        let exist_splitter = consume_optional_splitter(iter);

        if !exist_splitter {
            skip_new_lines(iter);
            consume_right_paren(iter)?;
            break;
        }
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

// consume ')'
fn consume_right_paren(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::RightParen)
}

// consume ']'
fn consume_right_bracket(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::RightBracket)
}

// consume '}'
fn consume_right_brace(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::RightBrace)
}

// consume ':'
fn consume_colon(iter: &mut PeekableIterator<Token>) -> Result<(), ParseError> {
    consume_token(iter, Token::Colon)
}

fn consume_optional_splitter(iter: &mut PeekableIterator<Token>) -> bool {
    while let Some(t) = iter.peek(0) {
        match t {
            Token::Comment(_) => {
                iter.next();
            }
            Token::Comma | Token::NewLine => {
                iter.next();
                return true;
            }
            _ => {
                return false;
            }
        }
    }

    false
}

fn skip_new_lines(iter: &mut PeekableIterator<Token>) {
    while let Some(t) = iter.peek(0) {
        match t {
            Token::Comment(_) => {
                iter.next();
            }
            Token::NewLine => {
                iter.next();
            }
            _ => {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;

    use crate::{
        lexer::lex, parser::NameValuePair, peekable_iterator::PeekableIterator, NumberLiteral,
        ParseError,
    };

    use super::{parse, AsonNode};

    fn parse_from_str(s: &str) -> Result<AsonNode, ParseError> {
        let mut chars = s.chars();
        let mut char_iter = PeekableIterator::new(&mut chars, 3);
        let tokens = lex(&mut char_iter)?;
        let mut token_iter = tokens.into_iter();
        let mut peekable_token_iter = PeekableIterator::new(&mut token_iter, 2);
        parse(&mut peekable_token_iter)
    }

    #[test]
    fn test_parse_single_value() {
        assert_eq!(
            parse_from_str(
                r#"
            123
            "#
            )
            .unwrap(),
            AsonNode::Number(NumberLiteral::Int(123))
        );

        assert_eq!(
            parse_from_str(
                r#"
            true
            "#
            )
            .unwrap(),
            AsonNode::Boolean(true)
        );

        assert_eq!(
            parse_from_str(
                r#"
            '🍒'
            "#
            )
            .unwrap(),
            AsonNode::Char('🍒')
        );

        assert_eq!(
            parse_from_str(
                r#"
            "hello"
            "#
            )
            .unwrap(),
            AsonNode::String_("hello".to_owned())
        );

        assert_eq!(
            parse_from_str(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            )
            .unwrap(),
            AsonNode::Date(DateTime::parse_from_rfc3339("2024-03-17 10:01:11+08:00").unwrap())
        );

        assert_eq!(
            parse_from_str(
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
            parse_from_str(
                r#"
            {id:123,name:"foo"}
            "#
            )
            .unwrap(),
            expect_object1
        );

        assert_eq!(
            parse_from_str(
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
            parse_from_str(
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
            parse_from_str(
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

        // err: key name with quote
        assert!(matches!(
            parse_from_str(
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
            parse_from_str(
                r#"
            [123,456,789]
            "#
            )
            .unwrap(),
            expect_array1
        );

        assert_eq!(
            parse_from_str(
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
            parse_from_str(
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
            parse_from_str(
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
            parse_from_str(
                r#"
            (123,"foo",true)
            "#
            )
            .unwrap(),
            expect_tuple1
        );

        assert_eq!(
            parse_from_str(
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
            parse_from_str(
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
            parse_from_str(
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
            parse_from_str(
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
                            AsonNode::Boolean(true)
                        ]),
                        AsonNode::Tuple(vec![
                            AsonNode::Number(NumberLiteral::Int(2)),
                            AsonNode::String_("bar".to_owned()),
                            AsonNode::Boolean(false)
                        ]),
                    ]))
                },
                NameValuePair {
                    name: "group".to_owned(),
                    value: Box::new(AsonNode::Object(vec![
                        NameValuePair {
                            name: "active".to_owned(),
                            value: Box::new(AsonNode::Boolean(true))
                        },
                        NameValuePair {
                            name: "permissions".to_owned(),
                            value: Box::new(AsonNode::Array(vec![
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(11)))
                                    },
                                    NameValuePair {
                                        name: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("read".to_owned()))
                                    }
                                ]),
                                AsonNode::Object(vec![
                                    NameValuePair {
                                        name: "number".to_owned(),
                                        value: Box::new(AsonNode::Number(NumberLiteral::Int(13)))
                                    },
                                    NameValuePair {
                                        name: "title".to_owned(),
                                        value: Box::new(AsonNode::String_("write".to_owned()))
                                    }
                                ]),
                            ]))
                        }
                    ])),
                },
            ])
        );

        // err: does not end properly
        assert!(matches!(
            parse_from_str(
                r#"
                {id: 123, name: "foo"} true
                "#
            ),
            Err(ParseError { message: _ })
        ));
    }
}
