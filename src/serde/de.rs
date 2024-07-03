// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use serde::de::{self, EnumAccess, SeqAccess, VariantAccess};

use super::Result;
use crate::{
    error::Error,
    process::{
        lexer::{lex, sanitize, Token},
        lookaheaditer::LookaheadIter,
        lookaheadvec::LookaheadVec,
        NumberLiteral,
    },
};

pub fn from_str<T>(input: &str) -> Result<T>
where
    T: de::DeserializeOwned,
{
    // There are two main ways to write Deserialize trait bounds,
    // whether on an impl block or a function or anywhere else.
    // - <'de, T> where T: Deserialize<'de>
    // - <T> where T: DeserializeOwned
    // see:
    // https://serde.rs/lifetimes.html

    let mut chars = input.chars();
    let mut char_iter = LookaheadIter::new(&mut chars, 3);
    let tokens = lex(&mut char_iter)?;
    let effective_tokens = sanitize(tokens);
    let mut lookahead_tokens = LookaheadVec::new(effective_tokens);
    let mut deserializer = Deserializer::from_tokens(&mut lookahead_tokens);
    let t = T::deserialize(&mut deserializer)?;

    if let Some(_) = deserializer.vec.peek(0) {
        Err(Error::Message(
            "The ASON document ends incorrectly.".to_owned(),
        ))
    } else {
        Ok(t)
    }
}

pub struct Deserializer<'de> {
    vec: &'de mut LookaheadVec<Token>,
}

impl<'de> Deserializer<'de> {
    pub fn from_tokens(vec: &'de mut LookaheadVec<Token>) -> Self {
        Self { vec }
    }
}

impl<'de> Deserializer<'de> {
    fn consume_token(&mut self, expect_token: Token) -> Result<()> {
        let opt_token = self.vec.next();
        if let Some(token) = opt_token {
            if token == expect_token {
                Ok(())
            } else {
                Err(Error::Message(format!(
                    "Expect token: {:?}, actual token: {:?}",
                    expect_token, token
                )))
            }
        } else {
            Err(Error::Message(format!("Missing token: {:?}", expect_token)))
        }
    }

    // consume ':'
    fn consume_colon(&mut self) -> Result<()> {
        self.consume_token(Token::Colon)
    }

    // consume '('
    fn consume_left_paren(&mut self) -> Result<()> {
        self.consume_token(Token::LeftParen)
    }

    // consume ')'
    fn consume_right_paren(&mut self) -> Result<()> {
        self.consume_token(Token::RightParen)
    }

    // consume ']'
    fn consume_right_bracket(&mut self) -> Result<()> {
        self.consume_token(Token::RightBracket)
    }

    // consume '}'
    fn consume_right_brace(&mut self) -> Result<()> {
        self.consume_token(Token::RightBrace)
    }

    // consume '\n'
    fn consume_new_line_when_exist(&mut self) {
        if let Some(Token::NewLine) = self.vec.peek(0) {
            self.vec.next();
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(current_token) = self.vec.peek(0) {
            match current_token {
                Token::Boolean(_) => self.deserialize_bool(visitor),
                Token::Number(n) => match n {
                    NumberLiteral::Byte(_) => self.deserialize_i8(visitor),
                    NumberLiteral::UByte(_) => self.deserialize_u8(visitor),
                    NumberLiteral::Short(_) => self.deserialize_i16(visitor),
                    NumberLiteral::UShort(_) => self.deserialize_u16(visitor),
                    NumberLiteral::Int(_) => self.deserialize_i32(visitor),
                    NumberLiteral::UInt(_) => self.deserialize_u32(visitor),
                    NumberLiteral::Long(_) => self.deserialize_i64(visitor),
                    NumberLiteral::ULong(_) => self.deserialize_u64(visitor),
                    NumberLiteral::Float(_) => self.deserialize_f32(visitor),
                    NumberLiteral::Double(_) => self.deserialize_f64(visitor),
                },
                Token::Char(_) => self.deserialize_char(visitor),
                Token::String_(_) => self.deserialize_string(visitor),
                _ => unreachable!(),
            }
        } else {
            Err(Error::Message("Incomplete ASON document.".to_owned()))
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Boolean(v)) = self.vec.next() {
            visitor.visit_bool(v)
        } else {
            Err(Error::Message("Expect \"Boolean\".".to_owned()))
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Byte(v))) = self.vec.next() {
            visitor.visit_i8(v)
        } else {
            Err(Error::Message("Expect \"i8\".".to_owned()))
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Short(v))) = self.vec.next() {
            visitor.visit_i16(v)
        } else {
            Err(Error::Message("Expect \"i16\".".to_owned()))
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Int(v))) = self.vec.next() {
            visitor.visit_i32(v)
        } else {
            Err(Error::Message("Expect \"i32\".".to_owned()))
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Long(v))) = self.vec.next() {
            visitor.visit_i64(v)
        } else {
            Err(Error::Message("Expect \"i64\".".to_owned()))
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::UByte(v))) = self.vec.next() {
            visitor.visit_u8(v)
        } else {
            Err(Error::Message("Expect \"u8\".".to_owned()))
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::UShort(v))) = self.vec.next() {
            visitor.visit_u16(v)
        } else {
            Err(Error::Message("Expect \"u16\".".to_owned()))
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::UInt(v))) = self.vec.next() {
            visitor.visit_u32(v)
        } else {
            Err(Error::Message("Expect \"u32\".".to_owned()))
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::ULong(v))) = self.vec.next() {
            visitor.visit_u64(v)
        } else {
            Err(Error::Message("Expect \"u64\".".to_owned()))
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Float(v))) = self.vec.next() {
            visitor.visit_f32(v)
        } else {
            Err(Error::Message("Expect \"f32\".".to_owned()))
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberLiteral::Double(v))) = self.vec.next() {
            visitor.visit_f64(v)
        } else {
            Err(Error::Message("Expect \"f64\".".to_owned()))
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Char(c)) = self.vec.next() {
            visitor.visit_char(c)
        } else {
            Err(Error::Message("Expect \"Char\".".to_owned()))
        }
    }

    fn deserialize_str<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::Message(
            "Does not support \"String\" reference.".to_owned(),
        ))
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::String_(s)) = self.vec.next() {
            visitor.visit_string(s)
        } else {
            Err(Error::Message("Expect \"String\".".to_owned()))
        }
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::Message("Does not support \"&[u8]\".".to_owned()))
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::ByteData(v)) = self.vec.next() {
            visitor.visit_byte_buf(v)
        } else {
            Err(Error::Message("Expect \"Bytes\".".to_owned()))
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::VariantName(name)) = self.vec.next() {
            if &name == "Option::None" && !self.vec.equals(0, &Token::LeftParen) {
                visitor.visit_none()
            } else if &name == "Option::Some" && self.vec.equals(0, &Token::LeftParen) {
                self.consume_left_paren()?;
                let v = visitor.visit_some(&mut *self);
                self.consume_right_paren()?;
                v
            } else {
                Err(Error::Message("Incorrect variant \"Option\".".to_owned()))
            }
        } else {
            Err(Error::Message("Expect variant \"Option\".".to_owned()))
        }
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // The type of `()` in Rust.
        // It represents an anonymous value containing no data.
        Err(Error::Message("ASON does not support \"Unit\".".to_owned()))
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // For example `struct Unit` or `PhantomData<T>`.
        // It represents a named value containing no data.
        Err(Error::Message(
            "ASON does not support \"Unit Struct\".".to_owned(),
        ))
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // For example `struct Millimeters(u8)`.
        Err(Error::Message(
            "ASON does not support \"New Type Struct\".".to_owned(),
        ))
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::LeftBracket) = self.vec.next() {
            let value = visitor.visit_seq(ArrayAccessor::new(self))?;
            self.consume_right_bracket()?; // consume ']'
            self.consume_new_line_when_exist(); // consume trailing newlines
            Ok(value)
        } else {
            Err(Error::Message("Expect \"Array\".".to_owned()))
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::LeftParen) = self.vec.next() {
            let value = visitor.visit_seq(TupleAccessor::new(self))?;

            self.consume_new_line_when_exist(); // consume additional newlines
            self.consume_right_paren()?; // consume ')'
            self.consume_new_line_when_exist(); // consume trailing newlines

            Ok(value)
        } else {
            Err(Error::Message("Expect \"Tuple\".".to_owned()))
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // A named tuple, for example `struct Rgb(u8, u8, u8)`.
        Err(Error::Message(
            "ASON does not support \"Tuple Struct\".".to_owned(),
        ))
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // A variably sized heterogeneous key-value pairing,
        // for example `BTreeMap<K, V>`.
        // When serializing, the length may or may not be known before
        // iterating through all the entries. When deserializing,
        // the length is determined by looking at the serialized data.
        Err(Error::Message("ASON does not support \"map\".".to_owned()))
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        println!("NAME: {}", name);
        println!("VARIANTS: {:?}", variants);
        todo!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }
}

struct ArrayAccessor<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> ArrayAccessor<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}

impl<'de, 'a> SeqAccess<'de> for ArrayAccessor<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        self.de.consume_new_line_when_exist(); // includes the commas and newlines

        if self.de.vec.equals(0, &Token::RightBracket) {
            // exits the procedure when the end marker ']' is encountered.
            return Ok(None);
        }

        seed.deserialize(&mut *self.de).map(Some)
    }
}

struct TupleAccessor<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> TupleAccessor<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}

impl<'de, 'a> SeqAccess<'de> for TupleAccessor<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        self.de.consume_new_line_when_exist(); // includes the commas and newlines
        seed.deserialize(&mut *self.de).map(Some)

        // the deserializer knows the number of members of the
        // target tuple, so it doesn't need to check the
        // ending marker ')'.
    }
}

#[cfg(test)]
mod tests {
    use crate::serde::de::from_str;

    use pretty_assertions::assert_eq;
    use serde::Deserialize;
    use serde_bytes::ByteBuf;

    #[test]
    fn test_primitive_types() {
        // bool
        {
            assert_eq!(from_str::<bool>(r#"true"#).unwrap(), true);
            assert_eq!(from_str::<bool>(r#"false"#).unwrap(), false);
        }

        // signed integers
        {
            assert_eq!(from_str::<i8>(r#"11@i8"#).unwrap(), 11);
            assert_eq!(from_str::<i8>(r#"11@byte"#).unwrap(), 11);

            assert_eq!(from_str::<i16>(r#"13@i16"#).unwrap(), 13);
            assert_eq!(from_str::<i16>(r#"13@short"#).unwrap(), 13);

            assert_eq!(from_str::<i32>(r#"17"#).unwrap(), 17);
            assert_eq!(from_str::<i32>(r#"17@i32"#).unwrap(), 17);
            assert_eq!(from_str::<i32>(r#"17@int"#).unwrap(), 17);

            assert_eq!(from_str::<i64>(r#"19@i64"#).unwrap(), 19);
            assert_eq!(from_str::<i64>(r#"19@long"#).unwrap(), 19);
        }

        // unsigned integers
        {
            assert_eq!(from_str::<u8>(r#"11@u8"#).unwrap(), 11);
            assert_eq!(from_str::<u8>(r#"11@ubyte"#).unwrap(), 11);

            assert_eq!(from_str::<u16>(r#"13@u16"#).unwrap(), 13);
            assert_eq!(from_str::<u16>(r#"13@ushort"#).unwrap(), 13);

            assert_eq!(from_str::<u32>(r#"17@u32"#).unwrap(), 17);
            assert_eq!(from_str::<u32>(r#"17@uint"#).unwrap(), 17);

            assert_eq!(from_str::<u64>(r#"19@u64"#).unwrap(), 19);
            assert_eq!(from_str::<u64>(r#"19@ulong"#).unwrap(), 19);
        }

        // f32
        {
            assert_eq!(from_str::<f32>(r#"1.23"#).unwrap(), 1.23_f32);
            assert_eq!(from_str::<f32>(r#"1.23@f32"#).unwrap(), 1.23_f32);
            assert_eq!(from_str::<f32>(r#"1.23@float"#).unwrap(), 1.23_f32);
        }

        // f64
        {
            assert_eq!(from_str::<f64>(r#"4.56@f64"#).unwrap(), 4.56_f64);
            assert_eq!(from_str::<f64>(r#"4.56@double"#).unwrap(), 4.56_f64);
        }

        // char
        {
            assert_eq!(from_str::<char>(r#"'a'"#).unwrap(), 'a');
            assert_eq!(from_str::<char>(r#"'文'"#).unwrap(), '文');
            assert_eq!(from_str::<char>(r#"'🍒'"#).unwrap(), '🍒');
            assert_eq!(from_str::<char>(r#"'\n'"#).unwrap(), '\n');
            assert_eq!(from_str::<char>(r#"'\u{8431}'"#).unwrap(), '萱');
        }

        // string
        {
            assert_eq!(from_str::<String>(r#""abc""#).unwrap(), "abc".to_owned());
            assert_eq!(from_str::<String>(r#""文字""#).unwrap(), "文字".to_owned());
            assert_eq!(from_str::<String>(r#""🍒🍎""#).unwrap(), "🍒🍎".to_owned());
            assert_eq!(
                from_str::<String>(r#""hello\nworld""#).unwrap(),
                "hello\nworld".to_owned()
            );
            assert_eq!(
                from_str::<String>(r#""\u{5c0f}\u{8431}脚本""#).unwrap(),
                "小萱脚本".to_owned()
            );
        }
    }

    #[test]
    fn test_bytes() {
        assert_eq!(
            from_str::<ByteBuf>(r#"h"0b:0d:11:13""#).unwrap(),
            ByteBuf::from(vec![11u8, 13, 17, 19])
        );

        assert_eq!(
            from_str::<ByteBuf>(r#"h"61:62:63""#).unwrap(),
            ByteBuf::from(b"abc")
        );
    }

    #[test]
    fn test_option() {
        assert_eq!(from_str::<Option<i32>>(r#"Option::None"#).unwrap(), None);
        assert_eq!(
            from_str::<Option<i32>>(r#"Option::Some(123)"#).unwrap(),
            Some(123)
        );
    }

    #[test]
    fn test_variant() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum Color {
            Red,
            Green,
            Blue,
            Grey(u8),
        }

        assert_eq!(from_str::<Color>(r#"Color::Red"#).unwrap(), Color::Red);
        assert_eq!(from_str::<Color>(r#"Color::Green"#).unwrap(), Color::Green);
        assert_eq!(
            from_str::<Color>(r#"Color::Grey(11@ubyte)"#).unwrap(),
            Color::Grey(11)
        );

        // nested
        #[derive(Deserialize, Debug, PartialEq)]
        enum Apperance {
            Transparent,
            Color(Color),
        }
    }

    #[test]
    fn test_array() {
        assert_eq!(
            from_str::<Vec<i32>>(r#"[11,13,17,19]"#).unwrap(),
            vec![11, 13, 17, 19]
        );

        assert_eq!(
            from_str::<Vec<u8>>(
                r#"[
    97@ubyte
    98@ubyte
    99@ubyte
]"#
            )
            .unwrap(),
            b"abc"
        );

        assert_eq!(
            from_str::<Vec<String>>(
                r#"[
    "foo"
    "bar"
    "2024"
]"#
            )
            .unwrap(),
            vec!["foo", "bar", "2024"]
        );

        // variants
        assert_eq!(
            from_str::<Vec<Option<i32>>>(
                r#"[
            Option::Some(11)
            Option::None
            Option::Some(13)
        ]"#
            )
            .unwrap(),
            vec![Some(11), None, Some(13)]
        );

        // nested seq
        assert_eq!(
            from_str::<Vec<Vec<i32>>>(
                r#"[
    [11,13]
    [17,19]
    [23,29]
]"#
            )
            .unwrap(),
            vec![vec![11, 13], vec![17, 19], vec![23, 29]]
        );
    }

    #[test]
    fn test_tuple() {
        assert_eq!(
            from_str::<(i32, i32, i32, i32)>(r#"(11, 13, 17, 19)"#).unwrap(),
            (11, 13, 17, 19)
        );

        // a fixed-length array is treated as tuple
        assert_eq!(
            from_str::<[u8; 3]>(
                r#"(
97@ubyte
98@ubyte
99@ubyte
)"#
            )
            .unwrap(),
            b"abc".to_owned()
        );

        assert_eq!(
            from_str::<(String, String, String)>(
                r#"(
"foo", "bar", "2024", )"#
            )
            .unwrap(),
            ("foo".to_owned(), "bar".to_owned(), "2024".to_owned())
        );

        // variant
        assert_eq!(
            from_str::<(Option<i32>, Option<i32>, Option<i32>)>(
                r#"(
        Option::Some(11), Option::None, Option::Some(13))"#
            )
            .unwrap(),
            (Option::Some(11), Option::<i32>::None, Option::Some(13))
        );

        // nested tuple
        assert_eq!(
            from_str::<((i32, i32), (i32, i32), (i32, i32))>(r#"((11, 13), (17, 19), (23, 29))"#)
                .unwrap(),
            ((11, 13), (17, 19), (23, 29))
        );
    }
}
