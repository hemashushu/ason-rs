// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::io::Write;

use chrono::{DateTime, FixedOffset};

use crate::error::Error;

use super::{AsonNode, KeyValuePair, Number, Variant};

pub const INDENT_SPACES: &str = "    ";

fn print_number(writer: &mut dyn Write, v: &Number) -> Result<(), std::io::Error> {
    match v {
        Number::I8(v) => {
            write!(writer, "{}_i8", v)
        }
        Number::U8(v) => {
            write!(writer, "{}_u8", v)
        }
        Number::I16(v) => {
            write!(writer, "{}_i16", v)
        }
        Number::U16(v) => {
            write!(writer, "{}_u16", v)
        }
        Number::I32(v) => {
            // default integer number type
            write!(writer, "{}", v)
        }
        Number::U32(v) => {
            write!(writer, "{}_u32", v)
        }
        Number::I64(v) => {
            write!(writer, "{}_i64", v)
        }
        Number::U64(v) => {
            write!(writer, "{}_u64", v)
        }
        Number::F32(v) => {
            if v.is_nan() {
                write!(writer, "NaN_f32")
            } else if v == &f32::INFINITY {
                write!(writer, "Inf_f32")
            } else if v == &f32::NEG_INFINITY {
                write!(writer, "-Inf_f32")
            } else {
                write!(writer, "{}_f32", v)
            }
        }
        Number::F64(v) => {
            // default floating-point number type
            if v.is_nan() {
                write!(writer, "NaN")
            } else if v == &f64::INFINITY {
                write!(writer, "Inf")
            } else if v == &f64::NEG_INFINITY {
                write!(writer, "-Inf")
            } else {
                // a decimal point needs to be appended if there is no decimal point
                // in the literal.
                let mut s = v.to_string();
                if !s.contains('.') {
                    s.push_str(".0");
                }
                write!(writer, "{}", s)
            }
        }
    }
}

fn print_boolean(writer: &mut dyn Write, v: &bool) -> Result<(), std::io::Error> {
    match v {
        true => write!(writer, "true"),
        false => write!(writer, "false"),
    }
}

fn print_char(writer: &mut dyn Write, v: &char) -> Result<(), std::io::Error> {
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

    write!(writer, "'{}'", s)
}

fn print_string(writer: &mut dyn Write, v: &str) -> Result<(), std::io::Error> {
    write!(
        writer,
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

fn print_date(writer: &mut dyn Write, v: &DateTime<FixedOffset>) -> Result<(), std::io::Error> {
    write!(writer, "d\"{}\"", v.to_rfc3339())
}

fn print_variant(writer: &mut dyn Write, v: &Variant, level: usize) -> Result<(), std::io::Error> {
    let (type_name, member_name, value) = (&v.type_name, &v.member_name, &v.value);

    match value {
        super::VariantValue::Empty => write!(writer, "{}::{}", type_name, member_name),
        super::VariantValue::Value(v) => {
            write!(writer, "{}::{}(", type_name, member_name)?;
            print_node(writer, v, level)?;
            write!(writer, ")")
        }
        super::VariantValue::Tuple(v) => {
            write!(writer, "{}::{}", type_name, member_name)?;
            print_tuple(writer, v, level)
        }
        super::VariantValue::Object(kvps) => {
            write!(writer, "{}::{}", type_name, member_name)?;
            print_object(writer, kvps, level)
        }
    }
}

fn print_byte_data(writer: &mut dyn Write, v: &[u8]) -> Result<(), std::io::Error> {
    write!(
        writer,
        "h\"{}\"",
        v.iter()
            .map(|item| format!("{:02x}", item))
            .collect::<Vec<String>>()
            .join(" ")
    )
}

fn print_list(writer: &mut dyn Write, v: &[AsonNode], level: usize) -> Result<(), std::io::Error> {
    let leading_space = INDENT_SPACES.repeat(level);
    let sub_level = level + 1;
    let element_leading_space = INDENT_SPACES.repeat(sub_level);

    writeln!(writer, "[")?;
    for e in v {
        write!(writer, "{}", element_leading_space)?;
        print_node(writer, e, sub_level)?;
        writeln!(writer)?;
    }
    write!(writer, "{}]", leading_space)
}

fn print_tuple(writer: &mut dyn Write, v: &[AsonNode], level: usize) -> Result<(), std::io::Error> {
    write!(writer, "(")?;
    let mut is_first_element = true;

    for e in v {
        if is_first_element {
            is_first_element = false;
        } else {
            write!(writer, ", ")?;
        }
        print_node(writer, e, level)?;
    }
    write!(writer, ")")
}

fn print_object(
    writer: &mut dyn Write,
    v: &[KeyValuePair],
    level: usize,
) -> Result<(), std::io::Error> {
    let leading_space = INDENT_SPACES.repeat(level);
    let sub_level = level + 1;
    let element_leading_space = INDENT_SPACES.repeat(sub_level);

    writeln!(writer, "{{")?;
    for e in v {
        write!(writer, "{}{}: ", element_leading_space, e.key)?;
        print_node(writer, &e.value, sub_level)?;
        writeln!(writer)?;
    }
    write!(writer, "{}}}", leading_space)
}

fn print_node(writer: &mut dyn Write, node: &AsonNode, level: usize) -> Result<(), std::io::Error> {
    match node {
        AsonNode::Number(v) => print_number(writer, v),
        AsonNode::Boolean(v) => print_boolean(writer, v),
        AsonNode::Char(v) => print_char(writer, v),
        AsonNode::String_(v) => print_string(writer, v),
        AsonNode::DateTime(v) => print_date(writer, v),
        AsonNode::Variant(v) => print_variant(writer, v, level),
        AsonNode::ByteData(v) => print_byte_data(writer, v),
        AsonNode::List(v) => print_list(writer, v, level),
        AsonNode::Tuple(v) => print_tuple(writer, v, level),
        AsonNode::Object(v) => print_object(writer, v, level),
    }
}

pub fn print_to_writer(writer: &mut dyn Write, node: &AsonNode) -> Result<(), Error> {
    match print_node(writer, node, 0) {
        Ok(_) => Ok(()),
        Err(e) => Err(Error::Message(e.to_string())),
    }
}

pub fn print_to_string(node: &AsonNode) -> String {
    let mut buf: Vec<u8> = vec![];
    print_to_writer(&mut buf, node).unwrap();
    String::from_utf8(buf).unwrap()
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
