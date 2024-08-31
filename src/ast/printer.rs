// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use chrono::{DateTime, FixedOffset};

use super::{AsonNode, KeyValuePair, Number, Variant};

pub const INDENT_SPACES: &str = "    ";

fn print_number(v: &Number) -> String {
    match v {
        Number::I8(v) => {
            format!("{}_i8", v)
        }
        Number::U8(v) => {
            format!("{}_u8", v)
        }
        Number::I16(v) => {
            format!("{}_i16", v)
        }
        Number::U16(v) => {
            format!("{}_u16", v)
        }
        Number::I32(v) => {
            // default integer number type
            format!("{}", v)
        }
        Number::U32(v) => {
            format!("{}_u32", v)
        }
        Number::I64(v) => {
            format!("{}_i64", v)
        }
        Number::U64(v) => {
            format!("{}_u64", v)
        }
        Number::F32(v) => {
            format!("{}_f32", v)
        }
        Number::F64(v) => {
            // default floating-point number type
            let mut s = v.to_string();
            if !s.contains('.') {
                s.push_str(".0");
            }
            s
        }
    }
}

fn print_boolean(v: &bool) -> String {
    match v {
        true => "true".to_owned(),
        false => "false".to_owned(),
    }
}

fn print_char(v: &char) -> String {
    // escape single char
    let s = match v {
        '\\' => "\\\\".to_owned(),
        '\'' => "\\'".to_owned(),
        '\t' => {
            // horizontal tabulation
            "\\t".to_owned()
        }
        '\r' => {
            // carriage return, jump to the beginning of the line (CR)
            "\\r".to_owned()
        }
        '\n' => {
            // new line/line feed (LF)
            "\\n".to_owned()
        }
        '\0' => {
            // null char
            "\\0".to_owned()
        }
        _ => v.to_string(),
    };

    format!("'{}'", s)
}

fn print_string(v: &str) -> String {
    format!(
        "\"{}\"",
        v.chars()
            .map(|c| match c {
                '\\' => "\\\\".to_owned(),
                '"' => "\\\"".to_owned(),
                // null char is allowed in the ASON string, it is used for represent the string resource.
                '\0' => "\\0".to_owned(),
                // some text editors automatically remove the tab when
                // it is at the end of a line, so it is best to escape the tab char.
                // therefor it should be escaped
                '\t' => "\\t".to_owned(),
                _ => c.to_string(),
            })
            .collect::<Vec<String>>()
            .join("")
    )
}

fn print_date(v: &DateTime<FixedOffset>) -> String {
    format!("d\"{}\"", v.to_rfc3339())
}

fn print_variant(v: &Variant, level: usize) -> String {
    let (type_name, member_name, value) = (&v.type_name, &v.member_name, &v.value);

    match value {
        super::VariantValue::Empty => format!("{}::{}", type_name, member_name),
        super::VariantValue::Value(v) => {
            format!("{}::{}({})", type_name, member_name, print_node(v, level))
        }
        super::VariantValue::Tuple(v) => {
            let s = v
                .iter()
                .map(|node| print_node(node, level))
                .collect::<Vec<String>>()
                .join(", ");
            format!("{}::{}({})", type_name, member_name, s)
        }
        super::VariantValue::Object(kvps) => {
            format!(
                "{}::{}{}",
                type_name,
                member_name,
                print_object(kvps, level)
            )
        }
    }
}

fn print_byte_data(v: &[u8]) -> String {
    format!(
        "h\"{}\"",
        v.iter()
            .map(|item| format!("{:02x}", item))
            .collect::<Vec<String>>()
            .join(" ")
    )
}

fn print_list(v: &[AsonNode], level: usize) -> String {
    let leading_space = INDENT_SPACES.repeat(level);
    let sub_level = level + 1;
    let element_leading_space = INDENT_SPACES.repeat(sub_level);
    format!(
        "[\n{}\n{}]",
        v.iter()
            .map(|item| format!("{}{}", element_leading_space, print_node(item, sub_level)))
            .collect::<Vec<String>>()
            .join("\n"),
        leading_space
    )
}

fn print_tuple(v: &[AsonNode], level: usize) -> String {
    format!(
        "({})",
        v.iter()
            .map(|item| print_node(item, level))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn print_object(v: &[KeyValuePair], level: usize) -> String {
    let write_name_value_pair = |name_value_pair: &KeyValuePair, current_level: usize| -> String {
        let leading_space = INDENT_SPACES.repeat(current_level);
        format!(
            "{}{}: {}",
            leading_space,
            name_value_pair.key,
            print_node(&name_value_pair.value, current_level)
        )
    };

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

fn print_node(node: &AsonNode, level: usize) -> String {
    match node {
        AsonNode::Number(v) => print_number(v),
        AsonNode::Boolean(v) => print_boolean(v),
        AsonNode::Char(v) => print_char(v),
        AsonNode::String_(v) => print_string(v),
        AsonNode::DateTime(v) => print_date(v),
        AsonNode::Variant(v) => print_variant(v, level),
        AsonNode::ByteData(v) => print_byte_data(v),
        AsonNode::List(v) => print_list(v, level),
        AsonNode::Tuple(v) => print_tuple(v, level),
        AsonNode::Object(v) => print_object(v, level),
    }
}

pub fn print_to_string(node: &AsonNode) -> String {
    print_node(node, 0)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::ast::parser::parse_from_str;

    use super::print_to_string;

    fn format(s: &str) -> String {
        let node = parse_from_str(s).unwrap();
        print_to_string(&node)
    }

    #[test]
    fn test_format_simple_value() {
        assert_eq!(
            format(
                r#"
            123
            "#
            ),
            "123"
        );

        assert_eq!(
            format(
                r#"
            1.23
            "#
            ),
            "1.23"
        );

        assert_eq!(
            format(
                r#"
            123f64
            "#
            ),
            "123.0"
        );

        assert_eq!(
            format(
                r#"
            123f32
            "#
            ),
            "123_f32"
        );

        assert_eq!(
            format(
                r#"
            true
            "#
            ),
            "true"
        );

        assert_eq!(
            format(
                r#"
            '🍒'
            "#
            ),
            "'🍒'"
        );

        assert_eq!(
            format(
                r#"
            '\n'
            "#
            ),
            "'\\n'"
        );

        assert_eq!(
            format(
                r#"
            "hello\"world"
            "#
            ),
            "\"hello\\\"world\""
        );

        assert_eq!(
            format(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            ),
            "d\"2024-03-17T10:01:11+08:00\""
        );
    }

    #[test]
    fn test_format_byte_data() {
        assert_eq!(
            format(
                r#"
            h"11 13 17 19"
            "#
            ),
            "h\"11 13 17 19\""
        );
    }

    #[test]
    fn test_format_object() {
        assert_eq!(
            format(
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
            format(
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
            format(
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
    fn test_format_list() {
        assert_eq!(
            format(
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
            format(
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
            format(
                r#"
            (123,"foo",true)
            "#
            ),
            "(123, \"foo\", true)"
        );
    }

    #[test]
    fn test_format_variant() {
        assert_eq!(format(r#"Option::None"#), "Option::None");
        assert_eq!(format(r#"Option::Some(123)"#), "Option::Some(123)");
        assert_eq!(
            format(r#"Color::RGB(255,200,100)"#),
            "Color::RGB(255, 200, 100)"
        );
        assert_eq!(
            format(r#"Shape::Rect{width:11, height:13}"#),
            r#"Shape::Rect{
    width: 11
    height: 13
}"#
        );
    }
}
