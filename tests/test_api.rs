// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use ason::{parse, write, AsonNode, NameValuePair, NumberLiteral, VariantItem};
use pretty_assertions::assert_eq;

#[test]
fn test_parse() {
    let text = r#"{
        id: 123
        name: "foo"
        orders: [11, 13]
    }"#;

    let node = parse(text).unwrap();

    if let AsonNode::Object(obj) = node {
        assert_eq!(
            &obj[0],
            &NameValuePair {
                name: "id".to_string(),
                value: Box::new(AsonNode::Number(NumberLiteral::Int(123)))
            }
        );

        assert_eq!(
            &obj[1],
            &NameValuePair {
                name: "name".to_string(),
                value: Box::new(AsonNode::String_("foo".to_string()))
            }
        );

        assert_eq!(
            &obj[2],
            &NameValuePair {
                name: "orders".to_string(),
                value: Box::new(AsonNode::Array(vec![
                    AsonNode::Number(NumberLiteral::Int(11)),
                    AsonNode::Number(NumberLiteral::Int(13))
                ]))
            }
        );
    }
}

#[test]
fn test_write() {
    let node = AsonNode::Object(vec![
        NameValuePair {
            name: "name".to_string(),
            value: Box::new(AsonNode::String_("foo".to_string())),
        },
        NameValuePair {
            name: "version".to_string(),
            value: Box::new(AsonNode::String_("0.1.0".to_string())),
        },
        NameValuePair {
            name: "dependencies".to_string(),
            value: Box::new(AsonNode::Array(vec![
                AsonNode::Object(vec![
                    NameValuePair {
                        name: "name".to_string(),
                        value: Box::new(AsonNode::String_("random".to_string())),
                    },
                    NameValuePair {
                        name: "version".to_string(),
                        value: Box::new(AsonNode::Variant(VariantItem {
                            name: "Option::None".to_string(),
                            value: None,
                        })),
                    },
                ]),
                AsonNode::Object(vec![
                    NameValuePair {
                        name: "name".to_string(),
                        value: Box::new(AsonNode::String_("regex".to_string())),
                    },
                    NameValuePair {
                        name: "version".to_string(),
                        value: Box::new(AsonNode::Variant(VariantItem {
                            name: "Option::Some".to_string(),
                            value: Some(Box::new(AsonNode::String_("1.0.1".to_string()))),
                        })),
                    },
                ]),
            ])),
        },
    ]);

    let text = write(&node);

    assert_eq!(
        text,
        r#"{
    name: "foo"
    version: "0.1.0"
    dependencies: [
        {
            name: "random"
            version: Option::None
        }
        {
            name: "regex"
            version: Option::Some("1.0.1")
        }
    ]
}"#
    );
}
