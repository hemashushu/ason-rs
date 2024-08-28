// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub mod parser;
pub mod printer;

use chrono::{DateTime, FixedOffset};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Number {
    // it is possible for literal to overflow for signed numbers,
    // such as `-128`, which consists of a negative/minus sign
    // and the number `128`, the number 128 is out of range for `i8`, so
    // define the `i8` literal using `u8`.
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, PartialEq)]
pub struct KeyValuePair {
    pub key: String,
    pub value: Box<AsonNode>,
}

#[derive(Debug, PartialEq)]
pub struct Variant {
    // variant type name, e.g. the "Option" of "Option::None"
    pub type_name: String,

    // variant member name, e.g. the "None" of "Option::None"
    pub member_name: String,

    pub value: VariantValue,
}

#[derive(Debug, PartialEq)]
pub enum VariantValue {
    Empty,                     // unit variant
    Value(Box<AsonNode>),      // new type variant
    Tuple(Vec<AsonNode>),      // tuple variant
    Object(Vec<KeyValuePair>), // struct variant
}

#[derive(Debug, PartialEq)]
pub enum AsonNode {
    Number(Number),
    Boolean(bool),
    Char(char),
    String_(String),
    DateTime(DateTime<FixedOffset>),
    Variant(Variant),
    ByteData(Vec<u8>),
    List(Vec<AsonNode>),
    Tuple(Vec<AsonNode>),
    Object(Vec<KeyValuePair>),
}

impl KeyValuePair {
    pub fn new(key: &str, value: AsonNode) -> Self {
        Self {
            key: key.to_owned(),
            value: Box::new(value),
        }
    }
}

impl Variant {
    pub fn new(type_name: &str, member_name: &str) -> Self {
        Self {
            type_name: type_name.to_owned(),
            member_name: member_name.to_owned(),
            value: VariantValue::Empty,
        }
    }

    pub fn with_value(type_name: &str, member_name: &str, value: AsonNode) -> Self {
        Self {
            type_name: type_name.to_owned(),
            member_name: member_name.to_owned(),
            value: VariantValue::Value(Box::new(value)),
        }
    }

    pub fn with_tuple(type_name: &str, member_name: &str, values: Vec<AsonNode>) -> Self {
        Self {
            type_name: type_name.to_owned(),
            member_name: member_name.to_owned(),
            value: VariantValue::Tuple(values),
        }
    }

    pub fn with_object(
        type_name: &str,
        member_name: &str,
        key_value_pairs: Vec<KeyValuePair>,
    ) -> Self {
        Self {
            type_name: type_name.to_owned(),
            member_name: member_name.to_owned(),
            value: VariantValue::Object(key_value_pairs),
        }
    }
}

impl AsonNode {
    pub fn new_string(s: &str) -> Self {
        Self::String_(s.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::ast::{
        parser::parse_from, printer::print_to, AsonNode, KeyValuePair, Number, Variant,
    };

    #[test]
    fn test_parse() {
        let text = r#"{
            id: 123
            name: "foo"
            orders: [11, 13]
        }"#;

        let node = parse_from(text).unwrap();

        assert_eq!(
            node,
            AsonNode::Object(vec![
                KeyValuePair::new("id", AsonNode::Number(Number::I32(123))),
                KeyValuePair::new("name", AsonNode::new_string("foo")),
                KeyValuePair::new(
                    "orders",
                    AsonNode::List(vec![
                        AsonNode::Number(Number::I32(11)),
                        AsonNode::Number(Number::I32(13))
                    ])
                )
            ])
        );
    }

    #[test]
    fn test_write() {
        let node = AsonNode::Object(vec![
            KeyValuePair::new("name", AsonNode::new_string("foo")),
            KeyValuePair::new(
                "type",
                AsonNode::Variant(Variant::new("Type", "Application")),
            ),
            KeyValuePair::new("version", AsonNode::new_string("0.1.0")),
            KeyValuePair::new(
                "dependencies",
                AsonNode::List(vec![
                    AsonNode::Object(vec![
                        KeyValuePair::new("name", AsonNode::new_string("random")),
                        KeyValuePair::new(
                            "version",
                            AsonNode::Variant(Variant::new("Option", "None")),
                        ),
                    ]),
                    AsonNode::Object(vec![
                        KeyValuePair::new("name", AsonNode::new_string("regex")),
                        KeyValuePair::new(
                            "version",
                            AsonNode::Variant(Variant::with_value(
                                "Option",
                                "Some",
                                AsonNode::new_string("1.0.1"),
                            )),
                        ),
                    ]),
                ]),
            ),
        ]);

        let text = print_to(&node);

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
        let s = read_example_file_to_string("01-primitive.ason");
        let n = parse_from(&s).unwrap();
        let t = print_to(&n);

        // note that the suffix 'a' should be '0.000000000000000001', but
        // in the debug mode, it may be '0.0000000000000000009999999' and
        // this unit test will be failed if it is running under the 'release' profile.

        assert_eq!(
            &t,
            r#"{
    integer: 123
    integer_negative: -123
    byte: 11_i8
    short: 17_i16
    int: 23
    long: 31_i64
    unsigned_byte: 13_u8
    unsigned_short: 19_u16
    unsigned_int: 29_u32
    unsigned_long: 37_u64
    floating_point: 3.14
    floating_point_with_exponent: 602200000000000000000000.0
    floating_point_with_negative_exponent: 0.000000000066738
    single_precision: 3.14_f32
    double_precision: 6.626
    hexadecimal_integer: 48879
    hexadecimal_integer_negative: -48879
    hexadecimal_byte: 127_i8
    hexadecimal_short: 32767_i16
    hexadecimal_int: 2147483647
    hexadecimal_long: 9223372036854775807_i64
    hexadecimal_unsigned_byte: 255_u8
    hexadecimal_unsigned_short: 65535_u16
    hexadecimal_unsigned_int: 4294967295_u32
    hexadecimal_unsigned_long: 18446744073709551615_u64
    hexadecimal_floating_point: 10.0
    hexadecimal_single_precison: 3.1415927_f32
    hexadecimal_double_precison: 2.718281828459045
    binary_integer: 9
    binary_integer_negative: -9
    binary_byte: 127_i8
    binary_short: 32767_i16
    binary_int: 2147483647
    binary_long: 140737488355327_i64
    binary_unsigned_byte: 255_u8
    binary_unsigned_short: 65535_u16
    binary_unsigned_int: 4294967295_u32
    binary_unsigned_long: 281474976710655_u64
    boolean_true: true
    boolean_false: false
    datatime: d"2023-02-23T10:23:45+00:00"
    datatime_with_timezone: d"2023-02-23T10:23:45+08:00"
    datatime_rfc3339: d"2023-02-23T10:23:45+08:00"
    datatime_rfc3339_zero_timezone: d"2023-02-23T10:23:45+00:00"
    char: 'c'
    char_unicode: '文'
    char_emoji: '🍋'
    char_escaped: '\n'
    char_escaped_zero: '\0'
    char_escaped_unicode: '河'
    string: "hello world"
    string_unicode: "中文🍀emoji👋🏻"
    multiline_string: "one
        two
        three"
    multiline_string_with_new_line_escaped: "onetwothree"
    string_with_escaped_chars: "double quote:\"
        single quote:'
        slash:\\
        tab:\t
        line feed:
"
    string_with_escaped_unicode: "河马"
    raw_string: "hello"
    raw_string_with_hash: "hello \"programming\" world"
    auto_trimmed_string: "heading 1
  heading 2
    heading 3"
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
        let s = read_example_file_to_string("02-list.ason");
        let n = parse_from(&s).unwrap();
        let t = print_to(&n);

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
        let n = parse_from(&s).unwrap();
        let t = print_to(&n);

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
        let s = read_example_file_to_string("04-object.ason");
        let n = parse_from(&s).unwrap();
        let t = print_to(&n);

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

    #[test]
    fn test_example_file_05() {
        let s = read_example_file_to_string("05-variant.ason");
        let n = parse_from(&s).unwrap();
        let t = print_to(&n);

        assert_eq!(
            t,
            r#"{
    variant_without_value: Option::None
    variant_single_value: Option::Some(123)
    variant_multiple_values: Color::RGB(255, 127, 63)
    variant_object: Shape::Rect{
        width: 200
        height: 100
    }
    variant_single_value: Option::Some((11, 13))
    variant_single_value: Option::Some([
        17
        19
        23
        29
    ])
    variant_single_value: Option::Some({
        id: 123
        name: "foo"
    })
}"#
        );
    }
}
