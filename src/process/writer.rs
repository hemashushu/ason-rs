// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use chrono::{DateTime, FixedOffset};

use super::{AsonNode, KeyValuePair, Number, Variant};

pub const INDENT_SPACES: &str = "    ";

fn write_number(v: &Number) -> String {
    match v {
        Number::I8(v) => {
            format!("{}@byte", *v as i8)
        }
        Number::U8(v) => {
            format!("{}@ubyte", v)
        }
        Number::I16(v) => {
            format!("{}@short", *v as i16)
        }
        Number::U16(v) => {
            format!("{}@ushort", v)
        }
        Number::I32(v) => {
            // default integer number type
            format!("{}", *v as i32)
        }
        Number::U32(v) => {
            format!("{}@uint", v)
        }
        Number::I64(v) => {
            format!("{}@long", *v as i64)
        }
        Number::U64(v) => {
            format!("{}@ulong", v)
        }
        Number::F32(v) => {
            // default floating-point number type
            let mut s = v.to_string();
            if !s.contains('.') {
                s.push_str(".0");
            }
            s
        }
        Number::F64(v) => {
            format!("{}@double", v)
        }
    }
}

fn write_boolean(v: &bool) -> String {
    match v {
        true => "true".to_owned(),
        false => "false".to_owned(),
    }
}

fn write_char(v: &char) -> String {
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

fn write_string(v: &str) -> String {
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

fn write_date(v: &DateTime<FixedOffset>) -> String {
    format!("d\"{}\"", v.to_rfc3339())
}

fn write_variant(v: &Variant, level: usize) -> String {
    if let Some(val) = &v.value {
        format!("{}({})", v.fullname, write_ason_node(val, level))
    } else {
        v.fullname.to_owned()
    }
}

fn write_byte_data(v: &[u8]) -> String {
    format!(
        "h\"{}\"",
        v.iter()
            .map(|item| format!("{:02x}", item))
            .collect::<Vec<String>>()
            .join(":")
    )
}

fn write_array(v: &[AsonNode], level: usize) -> String {
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

fn write_tuple(v: &[AsonNode], level: usize) -> String {
    format!(
        "({})",
        v.iter()
            .map(|item| write_ason_node(item, level))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn write_object(v: &[KeyValuePair], level: usize) -> String {
    let write_name_value_pair = |name_value_pair: &KeyValuePair, current_level: usize| -> String {
        let leading_space = INDENT_SPACES.repeat(current_level);
        format!(
            "{}{}: {}",
            leading_space,
            name_value_pair.key,
            write_ason_node(&name_value_pair.value, current_level)
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

fn write_ason_node(node: &AsonNode, level: usize) -> String {
    match node {
        AsonNode::Number(v) => write_number(v),
        AsonNode::Bool(v) => write_boolean(v),
        AsonNode::Char(v) => write_char(v),
        AsonNode::String_(v) => write_string(v),
        AsonNode::Date(v) => write_date(v),
        AsonNode::Variant(v) => write_variant(v, level),
        AsonNode::ByteData(v) => write_byte_data(v),
        AsonNode::List(v) => write_array(v, level),
        AsonNode::Tuple(v) => write_tuple(v, level),
        AsonNode::Object(v) => write_object(v, level),
    }
}

// pub fn write(node: &AsonNode) -> String {
//     write_ason_node(node, 0)
// }

pub fn to_string(node: &AsonNode) -> String {
    write_ason_node(node, 0)
}

#[cfg(test)]
mod tests {

    use pretty_assertions::assert_eq;

    use crate::process::parser::from_str;

    use super::to_string;

    fn format_ason_document(s: &str) -> String {
        let node = from_str(s).unwrap();
        to_string(&node)
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
