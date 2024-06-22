// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{AsonNode, NameValuePair, NumberLiteral};

pub const INDENT_SPACES: &str = "    ";

fn write_number_literal(number_literal: &NumberLiteral) -> String {
    match number_literal {
        NumberLiteral::Byte(v) => {
            format!("{}@byte", v)
        }
        NumberLiteral::UByte(v) => {
            format!("{}@ubyte", v)
        }
        NumberLiteral::Short(v) => {
            format!("{}@short", v)
        }
        NumberLiteral::UShort(v) => {
            format!("{}@ushort", v)
        }
        NumberLiteral::Int(v) => {
            // default integer number type
            format!("{}", v)
        }
        NumberLiteral::UInt(v) => {
            format!("{}@uint", v)
        }
        NumberLiteral::Long(v) => {
            format!("{}@long", v)
        }
        NumberLiteral::ULong(v) => {
            format!("{}@ulong", v)
        }
        NumberLiteral::Float(v) => {
            // default floating-point number type
            let mut s = v.to_string();
            if !s.contains('.') {
                s.push_str(".0");
            }
            s
        }
        NumberLiteral::Double(v) => {
            format!("{}@double", v)
        }
    }
}

fn write_name_value_pair(name_value_pair: &NameValuePair, level: usize) -> String {
    // let sub_level = level + 1;
    let leading_space = INDENT_SPACES.repeat(level);
    format!(
        "{}{}: {}",
        leading_space,
        name_value_pair.name,
        write_ason_node(&name_value_pair.value, level)
    )
}

fn write_ason_node(node: &AsonNode, level: usize) -> String {
    match node {
        AsonNode::Number(v) => write_number_literal(v),
        AsonNode::Boolean(v) => match v {
            true => "true".to_string(),
            false => "false".to_string(),
        },
        AsonNode::Char(v) => format!("'{}'", escape(*v, true)),
        AsonNode::String_(v) => format!(
            "\"{}\"",
            v.chars()
                .map(|c| escape(c, false))
                .collect::<Vec<String>>()
                .join("")
        ),
        AsonNode::Date(v) => format!("d\"{}\"", v.to_rfc3339()),
        AsonNode::Variant(i) => {
            if let Some(v) = &i.value {
                format!("{}({})", i.name, write_ason_node(v, level))
            } else {
                i.name.to_owned()
            }
        }
        AsonNode::ByteData(v) => format!(
            "h\"{}\"",
            v.iter()
                .map(|item| format!("{:02x}", item))
                .collect::<Vec<String>>()
                .join(":")
        ),
        AsonNode::Array(v) => {
            let leading_space = INDENT_SPACES.repeat(level);
            let sub_level = level + 1;
            let element_leading_space = INDENT_SPACES.repeat(sub_level);
            format!(
                "[\n{}\n{}]",
                v.iter()
                    .map(|item| format!(
                        "{}{}",
                        element_leading_space,
                        write_ason_node(item, sub_level)
                    ))
                    .collect::<Vec<String>>()
                    .join("\n"),
                leading_space
            )
        }
        AsonNode::Tuple(v) => format!(
            "({})",
            v.iter()
                .map(|item| write_ason_node(item, level))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        AsonNode::Object(v) => {
            let sub_level = level + 1;
            format!(
                "{{\n{}\n{}}}",
                v.iter()
                    .map(|item| write_name_value_pair(item, sub_level))
                    .collect::<Vec<String>>()
                    .join("\n"),
                INDENT_SPACES.repeat(level),
            )
        }
    }
}

fn escape(c: char, escape_single_char: bool) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '\'' if escape_single_char => "\\'".to_string(),
        '"' => "\\\"".to_string(),
        '\t' => {
            // horizontal tabulation
            "\\t".to_string()
        }
        '\r' if escape_single_char => {
            // carriage return, jump to the beginning of the line (CR)
            "\\r".to_string()
        }
        '\n' if escape_single_char => {
            // new line/line feed (LF)
            "\\n".to_string()
        }
        '\0' => {
            // null char
            "\\0".to_string()
        }
        _ => c.to_string(),
    }
}

pub fn write(node: &AsonNode) -> String {
    write_ason_node(node, 0)
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use pretty_assertions::assert_eq;

    use super::write;

    fn format_ason_document(s: &str) -> String {
        let node = parse(s).unwrap();
        write(&node)
    }

    #[test]
    fn test_format_simple_value() {
        assert_eq!(
            format_ason_document(
                r#"
            123
            "#
            ),
            "123"
        );

        assert_eq!(
            format_ason_document(
                r#"
            1.23
            "#
            ),
            "1.23"
        );

        assert_eq!(
            format_ason_document(
                r#"
            123@float
            "#
            ),
            "123.0"
        );

        assert_eq!(
            format_ason_document(
                r#"
            true
            "#
            ),
            "true"
        );

        assert_eq!(
            format_ason_document(
                r#"
            '🍒'
            "#
            ),
            "'🍒'"
        );

        assert_eq!(
            format_ason_document(
                r#"
            '\n'
            "#
            ),
            "'\\n'"
        );

        assert_eq!(
            format_ason_document(
                r#"
            "hello\"world"
            "#
            ),
            "\"hello\\\"world\""
        );

        assert_eq!(
            format_ason_document(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            ),
            "d\"2024-03-17T10:01:11+08:00\""
        );

        assert_eq!(
            format_ason_document(
                r#"
                    Option::None
                    "#
            ),
            "Option::None"
        );

        assert_eq!(
            format_ason_document(
                r#"
                    Option::Some(123)
                    "#
            ),
            "Option::Some(123)"
        );

        assert_eq!(
            format_ason_document(
                r#"
            h"11:13:17:19"
            "#
            ),
            "h\"11:13:17:19\""
        );
    }

    #[test]
    fn test_format_object() {
        assert_eq!(
            format_ason_document(
                r#"
            {id:123,name:"foo"}
            "#
            ),
            r#"{
    id: 123
    name: "foo"
}"#
        );

        assert_eq!(
            format_ason_document(
                r#"
            {id:123,name:{first:"foo", last:"bar"}}
            "#
            ),
            r#"{
    id: 123
    name: {
        first: "foo"
        last: "bar"
    }
}"#
        );

        assert_eq!(
            format_ason_document(
                r#"
                    {id:123,name:Option::Some({first:"foo", last:"bar"}),result:Result::Ok(456)}
                    "#
            ),
            r#"{
    id: 123
    name: Option::Some({
        first: "foo"
        last: "bar"
    })
    result: Result::Ok(456)
}"#
        );
    }

    #[test]
    fn test_format_array() {
        assert_eq!(
            format_ason_document(
                r#"
            [123,456,789]
            "#
            ),
            r#"[
    123
    456
    789
]"#
        );

        assert_eq!(
            format_ason_document(
                r#"
            [{id:123, name:"abc"},{id:456, name:"def"},{id:789,name:"xyz"}]
            "#
            ),
            r#"[
    {
        id: 123
        name: "abc"
    }
    {
        id: 456
        name: "def"
    }
    {
        id: 789
        name: "xyz"
    }
]"#
        );
    }

    #[test]
    fn test_format_tuple() {
        assert_eq!(
            format_ason_document(
                r#"
            (123,"foo",true)
            "#
            ),
            "(123, \"foo\", true)"
        );
    }
}
