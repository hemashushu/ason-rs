// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub mod lexer;
pub mod lookaheaditer;
// pub mod lookaheadvec;
pub mod parser;
pub mod writer;

use chrono::{DateTime, FixedOffset};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberLiteral {
    // it is possible for literal to overflow for signed numbers,
    // such as `-128`, which consists of a negative/minus sign
    // and the number `128`, which is out of range for `i8`, so
    // define the literal using `u8`.
    Byte(u8),
    UByte(u8),
    Short(u16),
    UShort(u16),
    Int(u32),
    UInt(u32),
    Long(u64),
    ULong(u64),
    Float(f32),
    Double(f64),
}

#[derive(Debug, PartialEq)]
pub struct NameValuePair {
    pub name: String,
    pub value: Box<AsonNode>,
}

#[derive(Debug, PartialEq)]
pub struct VariantItem {
    // the variant item full name, which includes the variant name and the member name, e.g.
    // - "Option::None"
    // - "Option::Some"
    pub fullname: String,

    // set to `None` when the variant item has no value, e.g. Option::None
    pub value: Option<Box<AsonNode>>,
}

#[derive(Debug, PartialEq)]
pub enum AsonNode {
    Number(NumberLiteral),
    Boolean(bool),
    Char(char),
    String_(String),
    Date(DateTime<FixedOffset>),
    Variant(VariantItem),
    ByteData(Vec<u8>),
    Array(Vec<AsonNode>),
    Tuple(Vec<AsonNode>),
    Object(Vec<NameValuePair>),
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::process::{
        parser::from_str, writer::to_string, AsonNode, NameValuePair, NumberLiteral, VariantItem,
    };

    #[test]
    fn test_parse() {
        let text = r#"{
            id: 123
            name: "foo"
            orders: [11, 13]
        }"#;

        let node = from_str(text).unwrap();

        assert_eq!(
            node,
            AsonNode::Object(vec![
                NameValuePair {
                    name: "id".to_owned(),
                    value: Box::new(AsonNode::Number(NumberLiteral::Int(123)))
                },
                NameValuePair {
                    name: "name".to_owned(),
                    value: Box::new(AsonNode::String_("foo".to_owned()))
                },
                NameValuePair {
                    name: "orders".to_owned(),
                    value: Box::new(AsonNode::Array(vec![
                        AsonNode::Number(NumberLiteral::Int(11)),
                        AsonNode::Number(NumberLiteral::Int(13))
                    ]))
                }
            ])
        );
    }

    #[test]
    fn test_write() {
        let node = AsonNode::Object(vec![
            NameValuePair {
                name: "name".to_owned(),
                value: Box::new(AsonNode::String_("foo".to_owned())),
            },
            NameValuePair {
                name: "type".to_owned(),
                value: Box::new(AsonNode::Variant(VariantItem {
                    fullname: "Type::Application".to_owned(),
                    value: None,
                })),
            },
            NameValuePair {
                name: "version".to_owned(),
                value: Box::new(AsonNode::String_("0.1.0".to_owned())),
            },
            NameValuePair {
                name: "dependencies".to_owned(),
                value: Box::new(AsonNode::Array(vec![
                    AsonNode::Object(vec![
                        NameValuePair {
                            name: "name".to_owned(),
                            value: Box::new(AsonNode::String_("random".to_owned())),
                        },
                        NameValuePair {
                            name: "version".to_owned(),
                            value: Box::new(AsonNode::Variant(VariantItem {
                                fullname: "Option::None".to_owned(),
                                value: None,
                            })),
                        },
                    ]),
                    AsonNode::Object(vec![
                        NameValuePair {
                            name: "name".to_owned(),
                            value: Box::new(AsonNode::String_("regex".to_owned())),
                        },
                        NameValuePair {
                            name: "version".to_owned(),
                            value: Box::new(AsonNode::Variant(VariantItem {
                                fullname: "Option::Some".to_owned(),
                                value: Some(Box::new(AsonNode::String_("1.0.1".to_owned()))),
                            })),
                        },
                    ]),
                ])),
            },
        ]);

        let text = to_string(&node);

        assert_eq!(
            text,
            r#"{
    name: "foo"
    type: Type::Application
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

    fn read_example_file_to_string(filename: &str) -> String {
        // note:
        //
        // in the VSCode editor `Debug` environment, the `current_dir()` returns
        // the project's root folder.
        // while in the `$ cargo test` (and VSCode editor `Run Test`) environment,
        // the `current_dir()` returns the current crate path if there are
        // multiple crates in the current workspace.

        let mut pwd = std::env::current_dir().unwrap();
        pwd.push("examples");
        pwd.push(filename);

        let file_path = pwd.to_str().unwrap();
        std::fs::read_to_string(file_path).unwrap()
    }

    #[test]
    fn test_example_file_01() {
        let s = read_example_file_to_string("01-object.ason");
        let n = from_str(&s).unwrap();
        let t = to_string(&n);

        // note that the suffix 'a' should be '0.000000000000000001', but
        // in the debug mode, it may be '0.0000000000000000009999999' and
        // this unit test will be failed if it is running under the 'release' profile.

        assert_eq!(
            &t,
            r#"{
    integer: 123
    integer_negative: -123
    byte: 11@byte
    ubyte: 13@ubyte
    short: 17@short
    ushort: 19@ushort
    int: 23
    uint: 29@uint
    long: 31@long
    ulong: 37@ulong
    floating_point: 3.14
    float: 3.14
    double: 6.626@double
    float_with_exp: 602200000000000000000000.0
    float_with_exp_negative: 0.000000000066738
    suffix_K: 1000
    suffix_M: 1000000
    suffix_G: 1000000000
    suffix_T: 1000000000000@long
    suffix_P: 1000000000000000@long
    suffix_E: 1000000000000000000@long
    suffix_m: 0.001
    suffix_u: 0.000001
    suffix_n: 0.000000001
    suffix_p: 0.000000000001
    suffix_f: 0.000000000000001
    suffix_a: 0.0000000000000000009999999
    suffix_Ki: 1024
    suffix_Mi: 1048576
    suffix_Gi: 1073741824
    suffix_Ti: 1099511627776@long
    suffix_Pi: 1125899906842624@long
    suffix_Ei: 1152921504606846976@long
    both_metric_suffix_and_type_long: 1000000000@long
    both_metric_suffix_and_type_double: 0.000001@double
    hex_integer: 48879
    hex_integer_negative: -57005
    hex_floating_point: 3.1415927
    hex_byte: 127@byte
    hex_ubyte: 255@ubyte
    hex_short: 32767@short
    hex_ushort: 65535@ushort
    hex_int: 2147483647
    hex_uint: 4294967295@uint
    hex_long: 9223372036854775807@long
    hex_ulong: 18446744073709551615@ulong
    hex_float: 3.1415927
    hex_double: 2.718281828459045@double
    bin_integer: 9
    bin_integer_negative: 9
    bin_byte: 127@byte
    bin_ubyte: 255@ubyte
    bin_short: 32767@short
    bin_ushort: 65535@ushort
    bin_int: 2147483647
    bin_uint: 4294967295@uint
    bin_long: 140737488355327@long
    bin_ulong: 281474976710655@ulong
    bool_true: true
    bool_false: false
    data_time: d"2023-02-23T10:23:45+00:00"
    data_time_tz: d"2023-02-23T10:23:45+08:00"
    char: 'c'
    char_unicode: '文'
    char_escape: '\n'
    char_escape_zero: '\0'
    char_escape_unicode: '河'
    string: "hello world"
    string_unicode: "中文🍀emoji👋🏻"
    multiline_string: "one
        two
        three"
    multiline_string_with_new_line_escape: "onetwothree"
    string_escape_chars: "double quote:\"
        single quote:'
        slash:\\
        tab:\t
        line feed:
"
    string_escape_unicode: "河马"
    raw_string: "hello"
    raw_string_variant: "hello \"programming\" world"
    auto_trimmed_string: "heading 1
  heading 2
    heading 3"
    variant_none: Option::None
    variant_some: Option::Some(123)
    variant_object: Option::Some({
        id: 123
    })
    new_line: "value1"
    new_line_variant: "value2"
    space: "value3"
    line_comment: 101
    line_comment_in_tail: 103
    block_comment: 107
    multiline_block_comment: 109
    document_comment: 113
    inline_comma_1: 211
    inline_comma_2: 223
    inline_comma_3: 227
    tail_comma_1: 229
    tail_comma_2: 233
    tail_comma_3: 239
}"#
        )
    }

    #[test]
    fn test_example_file_02() {
        let s = read_example_file_to_string("02-array.ason");
        let n = from_str(&s).unwrap();
        let t = to_string(&n);

        assert_eq!(
            t,
            r#"{
    number_array: [
        1
        2
        3
    ]
    string_array: [
        "one"
        "two"
        "three"
    ]
    array_with_trailing_comma: [
        1
        2
        3
        4
    ]
    mulitline_array: [
        1
        2
        3
    ]
    mulitline_array_with_commas: [
        1
        2
        3
    ]
    mulitline_array_with_trailing_comma: [
        1
        2
        3
    ]
}"#
        );
    }

    #[test]
    fn test_example_file_03() {
        let s = read_example_file_to_string("03-tuple.ason");
        let n = from_str(&s).unwrap();
        let t = to_string(&n);

        assert_eq!(
            t,
            r#"{
    tuple: (1, "foo", true)
    tuple_with_trailing_comma: (1, "foo", true)
    mulitline_tuple: (1, "foo", true)
    mulitline_tuple_with_commas: (1, "foo", true)
    mulitline_tuple_with_trailing_comma: (1, "foo", true)
}"#
        );
    }

    #[test]
    fn test_example_file_04() {
        let s = read_example_file_to_string("04-nested.ason");
        let n = from_str(&s).unwrap();
        let t = to_string(&n);

        assert_eq!(
            t,
            r#"{
    id: 123
    name: "hello"
    orders: [
        (1, "foo", true)
        (2, "bar", false)
    ]
    group: {
        active: true
        permissions: [
            {
                number: 11
                title: "read"
            }
            {
                number: 13
                title: "write"
            }
        ]
    }
}"#
        );
    }
}
