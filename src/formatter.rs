// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use crate::{AsonNode, NameValuePair, NumberLiteral};

pub const INDENT_SPACES: &'static str = "    ";

fn format_number_literal(number_literal: &NumberLiteral) -> String {
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
            format!("{}", v)
        }
        NumberLiteral::Double(v) => {
            format!("{}@double", v)
        }
    }
}

fn format_name_value_pair(name_value_pair: &NameValuePair, level: usize) -> String {
    let sub_level = level + 1;
    let leading_space = INDENT_SPACES.repeat(sub_level);
    format!(
        "{}{}: {}",
        leading_space,
        name_value_pair.name,
        format_ason_node(&name_value_pair.value, sub_level)
    )
}

fn format_ason_node(node: &AsonNode, level: usize) -> String {
    match node {
        AsonNode::Number(v) => format_number_literal(v),
        AsonNode::Boolean(v) => match v {
            true => "true".to_owned(),
            false => "false".to_owned(),
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
        AsonNode::ByteData(v) => format!(
            "h\"{}\"",
            v.iter()
                .map(|item| format!("{:02x}", item))
                .collect::<Vec<String>>()
                .join(":")
        ),
        AsonNode::Array(v) => format!(
            "[{}]",
            v.iter()
                .map(|item| format_ason_node(item, level))
                .collect::<Vec<String>>()
                .join(",")
        ),
        AsonNode::Tuple(v) => format!(
            "({})",
            v.iter()
                .map(|item| format_ason_node(item, level))
                .collect::<Vec<String>>()
                .join(",")
        ),
        AsonNode::Object(v) => format!(
            "{{\n{}\n}}",
            v.iter()
                .map(|item| format_name_value_pair(item, level))
                .collect::<Vec<String>>()
                .join("\n")
        ),
    }
}

fn escape(c: char, escape_single_char: bool) -> String {
    match c {
        '\\' => "\\\\".to_owned(),
        '\'' if escape_single_char => "\\'".to_owned(),
        '"' => "\\\"".to_owned(),
        '\t' => {
            // horizontal tabulation
            "\\t".to_owned()
        }
        '\r' if escape_single_char => {
            // carriage return, jump to the beginning of the line (CR)
            "\\r".to_owned()
        }
        '\n' if escape_single_char => {
            // new line/line feed (LF)
            "\\n".to_owned()
        }
        '\0' => {
            // null char
            "\\0".to_owned()
        }
        _ => c.to_string(),
    }
}

pub fn format(node: &AsonNode) -> String {
    format_ason_node(node, 0)
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::lex, parser::parse, peekable_iterator::PeekableIterator, AsonNode, ParseError,
    };

    use super::format;

    fn parse_from_str(s: &str) -> Result<AsonNode, ParseError> {
        let mut chars = s.chars();
        let mut char_iter = PeekableIterator::new(&mut chars, 3);
        let tokens = lex(&mut char_iter)?;
        let mut token_iter = tokens.into_iter();
        let mut peekable_token_iter = PeekableIterator::new(&mut token_iter, 2);
        parse(&mut peekable_token_iter)
    }

    fn format_to_str_from_str(s: &str) -> String {
        let node = parse_from_str(s).unwrap();
        format(&node)
    }

    #[test]
    fn test_format_single_value() {
        assert_eq!(
            format_to_str_from_str(
                r#"
            123
            "#
            ),
            "123"
        );

        assert_eq!(
            format_to_str_from_str(
                r#"
            true
            "#
            ),
            "true"
        );

        assert_eq!(
            format_to_str_from_str(
                r#"
            '🍒'
            "#
            ),
            "'🍒'"
        );

        assert_eq!(
            format_to_str_from_str(
                r#"
            '\n'
            "#
            ),
            "'\\n'"
        );

        assert_eq!(
            format_to_str_from_str(
                r#"
            "hello\"world"
            "#
            ),
            "\"hello\\\"world\""
        );

        assert_eq!(
            format_to_str_from_str(
                r#"
            d"2024-03-17 10:01:11+08:00"
            "#
            ),
            "d\"2024-03-17T10:01:11+08:00\""
        );

        assert_eq!(
            format_to_str_from_str(
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
            format_to_str_from_str(
                r#"
            {id:123,name:"foo"}
            "#
            ),
            "{\n    id: 123\n    name: \"foo\"\n}"
        );
    }

    #[test]
    fn test_format_array() {
        assert_eq!(
            format_to_str_from_str(
                r#"
            [123,456,789]
            "#
            ),
            "[123,456,789]"
        );
    }

    #[test]
    fn test_format_tuple() {
        assert_eq!(
            format_to_str_from_str(
                r#"
            (123,"foo",true)
            "#
            ),
            "(123,\"foo\",true)"
        );
    }
}
