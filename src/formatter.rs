// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::fmt::Display;

use crate::{AsonNode, NameValuePair, NumberLiteral};

impl Display for NumberLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberLiteral::Byte(v) => {
                write!(f, "{}@byte", v)
            }
            NumberLiteral::UByte(v) => {
                write!(f, "{}@ubyte", v)
            }
            NumberLiteral::Short(v) => {
                write!(f, "{}@short", v)
            }
            NumberLiteral::UShort(v) => {
                write!(f, "{}@ushort", v)
            }
            NumberLiteral::Int(v) => {
                write!(f, "{}", v)
            }
            NumberLiteral::UInt(v) => {
                write!(f, "{}@uint", v)
            }
            NumberLiteral::Long(v) => {
                write!(f, "{}@long", v)
            }
            NumberLiteral::ULong(v) => {
                write!(f, "{}@ulong", v)
            }
            NumberLiteral::Float(v) => {
                write!(f, "{}", v)
            }
            NumberLiteral::Double(v) => {
                write!(f, "{}@double", v)
            }
        }
    }
}

impl Display for NameValuePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.contains(' ') {
            write!(f, "\"{}\": {}", self.name, self.value)
        } else {
            write!(f, "{}: {}", self.name, self.value)
        }
    }
}

impl Display for AsonNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsonNode::Number(v) => {
                write!(f, "{}", v)
            }
            AsonNode::Boolean(v) => match v {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            AsonNode::Char(v) => write!(f, "{}", v),
            AsonNode::String_(v) => write!(f, "{}", v),
            AsonNode::Date(v) => write!(f, "{}", v.to_rfc3339()),
            AsonNode::ByteData(v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|item| format!("{:02x}", item))
                    .collect::<Vec<String>>()
                    .join(":")
            ),
            AsonNode::Array(v) => write!(
                f,
                "[{}]",
                v.iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            AsonNode::Tuple(v) => write!(
                f,
                "({})",
                v.iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            AsonNode::Object(v) => write!(
                f,
                "{{{}}}",
                v.iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        }
    }
}

pub fn format(node: &AsonNode) -> String {
    node.to_string()
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
        let s = format_to_str_from_str(
            r#"
        123
        "#,
        );

        println!("{}", s);
    }
}
