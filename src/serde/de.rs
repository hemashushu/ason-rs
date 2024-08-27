// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use serde::de::{self, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess};

use crate::{
    error::Error,
    lexer::{CharsWithPositionIter, NumberToken, Token, TokenIter, TokenWithRange},
    location::Position,
    normalizer::{ClearTokenIter, NormalizedTokenIter, TrimmedTokenIter},
    peekableiter::PeekableIter,
};

use super::Result;

pub fn from_str<T>(s: &str) -> Result<T>
where
    T: de::DeserializeOwned,
{
    let mut chars = s.chars();
    from_char_iter(&mut chars)
}

pub fn from_char_iter<T>(chars: &mut dyn Iterator<Item = char>) -> Result<T>
where
    T: de::DeserializeOwned,
{
    // There are two main ways to write Deserialize trait bounds,
    // whether on an impl block or a function or anywhere else.
    // - <'de, T> where T: Deserialize<'de>
    // - <T> where T: DeserializeOwned
    // see:
    // https://serde.rs/lifetimes.html

    let mut char_position_iter = CharsWithPositionIter::new(0, chars);
    let mut peekable_char_position_iter = PeekableIter::new(&mut char_position_iter, 3);
    let mut token_iter = TokenIter::new(&mut peekable_char_position_iter);
    let mut clear_iter = ClearTokenIter::new(&mut token_iter);
    let mut peekable_clear_iter = PeekableIter::new(&mut clear_iter, 1);
    let mut normalized_iter = NormalizedTokenIter::new(&mut peekable_clear_iter);
    let mut peekable_normalized_iter = PeekableIter::new(&mut normalized_iter, 1);
    let mut trimmed_iter = TrimmedTokenIter::new(&mut peekable_normalized_iter);
    let mut peekable_trimmed_iter = PeekableIter::new(&mut trimmed_iter, 2);

    let mut deserializer = Deserializer::from_token_peekable_iter(&mut peekable_trimmed_iter);
    let t = T::deserialize(&mut deserializer)?;

    // if deserializer.vec.peek(0).is_some() {
    //     Err(Error::Message(
    //         "The ASON document ends incorrectly.".to_owned(),
    //     ))
    // } else {
    //     Ok(t)
    // }

    match deserializer.iter.peek(0) {
        Some(Ok(TokenWithRange { range, .. })) => Err(Error::MessageWithPosition(
            "The ASON document does not end properly.".to_owned(),
            Position::from_range_start(range),
        )),
        Some(Err(e)) => Err(e.clone()),
        None => {
            // expected
            Ok(t)
        }
    }
}

pub struct Deserializer<'de> {
    // vec: &'de mut ForwardIter<'de, Token>,
    iter: &'de mut PeekableIter<'de, Result<TokenWithRange>>,
}

impl<'de> Deserializer<'de> {
    // pub fn from_tokens(vec: &'de mut ForwardIter<'de, Token>) -> Self {
    pub fn from_token_peekable_iter(
        iter: &'de mut PeekableIter<'de, Result<TokenWithRange>>,
    ) -> Self {
        Self { iter }
    }
}

impl<'de> Deserializer<'de> {
    fn consume_token(
        &mut self,
        expect_token: &Token,
        expect_position: &Position,
    ) -> Result<Position> {
        // let opt_token = self.iter.next();
        // if let Some(token) = opt_token {
        //     if token == expect_token {
        //         Ok(())
        //     } else {
        //         Err(Error::Message(format!(
        //             "Expect token: {:?}, actual token: {:?}",
        //             expect_token, token
        //         )))
        //     }
        // } else {
        //     Err(Error::Message(format!("Missing token: {:?}", expect_token)))
        // }

        match self.iter.next() {
            Some(Ok(TokenWithRange { token, range })) => {
                if &token == expect_token {
                    Ok(Position::from_range_end(&range))
                } else {
                    Err(Error::MessageWithPosition(
                        format!(
                            "Expect token: {:?}, actual token: {:?}",
                            expect_token, token
                        ),
                        *expect_position,
                    ))
                }
            }
            Some(Err(e)) => Err(e),
            None => Err(Error::MessageWithPosition(
                format!("Missing token: {:?}", expect_token),
                *expect_position,
            )),
        }
    }

    // consume ':'
    fn consume_colon(&mut self, expect_position: &Position) -> Result<Position> {
        self.consume_token(&Token::Colon, expect_position)
    }

    // consume ')'
    fn consume_right_paren(&mut self, expect_position: &Position) -> Result<Position> {
        self.consume_token(&Token::RightParen, expect_position)
    }

    // consume ']'
    fn consume_right_bracket(&mut self, expect_position: &Position) -> Result<Position> {
        self.consume_token(&Token::RightBracket, expect_position)
    }

    // consume '}'
    fn consume_right_brace(&mut self, expect_position: &Position) -> Result<Position> {
        self.consume_token(&Token::RightBrace, expect_position)
    }

    // consume '\n' or ',' if they exist.
    fn consume_new_line_or_comma_if_exist(&mut self, expect_position: &Position) -> Position {
        match self.iter.peek(0) {
            Some(Ok(TokenWithRange {
                token: Token::NewLine | Token::Comma,
                range,
            })) => {
                let position = Position::from_range_end(range);
                self.iter.next();
                position
            }
            _ => *expect_position,
        }
    }

    // consume '\n' or ',' if they exist.
    fn consume_new_line_or_comma_if_exist_ignore_position(&mut self) {
        if matches!(
            self.iter.peek(0),
            Some(Ok(TokenWithRange {
                token: Token::NewLine | Token::Comma,
                ..
            }))
        ) {
            self.iter.next();
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.iter.next() {
            Some(Ok(TokenWithRange {
                token: Token::Boolean(v),
                range: _,
            })) => visitor.visit_bool(v),
            Some(Ok(TokenWithRange { token: _, range })) => Err(Error::MessageWithPosition(
                "Expect a \"Boolean\" value.".to_owned(),
                Position::from_range_start(&range),
            )),
            Some(Err(e)) => Err(e.clone()),
            None => Err(Error::Message(
                "Expect a \"Boolean\" value, unexpected to reach the end of document.".to_owned(),
            )),
        }

        // if let Some(Token::Boolean(v)) = self.iter.next() {
        //     visitor.visit_bool(v)
        // } else {
        //     Err(Error::Message("Expect \"Boolean\".".to_owned()))
        // }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.iter.next() {
            Some(Ok(TokenWithRange {
                token: Token::Number(NumberToken::I8(v)) ,
                range: _,
            })) => visitor.visit_i8(v as i8),
            Some(Ok(TokenWithRange { token: _, range })) => Err(Error::MessageWithPosition(
                "Expect an \"i8\" value.".to_owned(),
                Position::from_range_start(&range),
            )),
            Some(Err(e)) => Err(e.clone()),
            None => Err(Error::Message(
                "Expect an \"i8\" value, unexpected to reach the end of document.".to_owned(),
            )),
        }

        // if let Some(Token::Number(NumberToken::I8(v))) = self.iter.next() {
        //     visitor.visit_i8(v as i8)
        // } else {
        //     Err(Error::Message("Expect \"i8\".".to_owned()))
        // }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::I16(v))) = self.iter.next() {
            visitor.visit_i16(v as i16)
        } else {
            Err(Error::Message("Expect \"i16\".".to_owned()))
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::I32(v))) = self.iter.next() {
            visitor.visit_i32(v as i32)
        } else {
            Err(Error::Message("Expect \"i32\".".to_owned()))
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::I64(v))) = self.iter.next() {
            visitor.visit_i64(v as i64)
        } else {
            Err(Error::Message("Expect \"i64\".".to_owned()))
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::U8(v))) = self.iter.next() {
            visitor.visit_u8(v)
        } else {
            Err(Error::Message("Expect \"u8\".".to_owned()))
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::U16(v))) = self.iter.next() {
            visitor.visit_u16(v)
        } else {
            Err(Error::Message("Expect \"u16\".".to_owned()))
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::U32(v))) = self.iter.next() {
            visitor.visit_u32(v)
        } else {
            Err(Error::Message("Expect \"u32\".".to_owned()))
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::U64(v))) = self.iter.next() {
            visitor.visit_u64(v)
        } else {
            Err(Error::Message("Expect \"u64\".".to_owned()))
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::F32(v))) = self.iter.next() {
            visitor.visit_f32(v)
        } else {
            Err(Error::Message("Expect \"f32\".".to_owned()))
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Number(NumberToken::F64(v))) = self.iter.next() {
            visitor.visit_f64(v)
        } else {
            Err(Error::Message("Expect \"f64\".".to_owned()))
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Char(c)) = self.iter.next() {
            visitor.visit_char(c)
        } else {
            Err(Error::Message("Expect \"Char\".".to_owned()))
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::String_(s)) = self.iter.next() {
            visitor.visit_str(&s)
        } else {
            Err(Error::Message("Expect \"String\".".to_owned()))
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::String_(s)) = self.iter.next() {
            visitor.visit_string(s)
        } else {
            Err(Error::Message("Expect \"String\".".to_owned()))
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::ByteData(v)) = self.iter.next() {
            visitor.visit_bytes(&v)
        } else {
            Err(Error::Message("Expect \"Bytes\".".to_owned()))
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::ByteData(v)) = self.iter.next() {
            visitor.visit_byte_buf(v)
        } else {
            Err(Error::Message("Expect \"Bytes\".".to_owned()))
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Variant(type_name, member_name)) = self.iter.next() {
            if type_name == "Option" {
                if member_name == "None" && !self.iter.equals(0, &Token::LeftParen) {
                    visitor.visit_none()
                } else if member_name == "Some" && self.iter.equals(0, &Token::LeftParen) {
                    self.iter.next(); // consume '('
                    let v = visitor.visit_some(&mut *self);
                    self.consume_right_paren()?;
                    v
                } else {
                    Err(Error::Message("Incorrect variant \"Option\".".to_owned()))
                }
            } else {
                Err(Error::Message("Expect variant \"Option\".".to_owned()))
            }
        } else {
            Err(Error::Message("Expect variant.".to_owned()))
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
        if let Some(Token::LeftBracket) = self.iter.next() {
            let value = visitor.visit_seq(ArrayAccessor::new(self))?;

            self.consume_right_bracket()?; // consume ']'
            self.consume_new_line_or_comma_if_exist_ignore_position(); // consume trailing newlines

            Ok(value)
        } else {
            Err(Error::Message("Expect \"List\".".to_owned()))
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::LeftParen) = self.iter.next() {
            let value = visitor.visit_seq(TupleAccessor::new(self))?;

            self.consume_new_line_or_comma_if_exist_ignore_position(); // consume additional newlines
            self.consume_right_paren()?; // consume ')'

            self.consume_new_line_or_comma_if_exist_ignore_position(); // consume trailing newlines

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
        Err(Error::Message("ASON does not support Map.".to_owned()))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::LeftBrace) = self.iter.next() {
            let value = visitor.visit_map(ObjectAccessor::new(self))?;

            self.consume_new_line_or_comma_if_exist_ignore_position(); // consume additional newlines
            self.consume_right_brace()?; // consume '}'

            self.consume_new_line_or_comma_if_exist_ignore_position(); // consume trailing newlines

            Ok(value)
        } else {
            Err(Error::Message("Expect \"Object\".".to_owned()))
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Some(Token::Variant(type_name, member_name)) = self.iter.next() {
            if type_name == name {
                if self.iter.equals(0, &Token::LeftParen) {
                    // variant with single value or multiple values
                    let v = visitor.visit_enum(VariantAccessor::new(self, &member_name))?;
                    Ok(v)
                } else if self.iter.equals(0, &Token::LeftBrace) {
                    // variant with struct value
                    let v = visitor.visit_enum(VariantAccessor::new(self, &member_name))?;
                    Ok(v)
                } else {
                    // variant without value
                    visitor.visit_enum(member_name.into_deserializer())
                }
            } else {
                Err(Error::Message(format!(
                    "Variant type mismatch, expect: {}, actual: {}.",
                    name, type_name
                )))
            }
        } else {
            Err(Error::Message("Expect \"Variant\".".to_owned()))
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // An identifier in Serde is the type that identifies a field of a struct.
        // In ASON, struct fields  represented as strings.

        if let Some(Token::Identifier(s)) = self.iter.next() {
            visitor.visit_string(s)
        } else {
            Err(Error::Message("Expect \"Object Field Name\".".to_owned()))
        }
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
        // Err(Error::Message(
        //     "ASON does not support type-less data.".to_owned(),
        // ))
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
        self.de.consume_new_line_or_comma_if_exist_ignore_position(); // includes the commas and newlines

        if self.de.iter.equals(0, &Token::RightBracket) {
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
        self.de.consume_new_line_or_comma_if_exist_ignore_position(); // includes the commas and newlines
        seed.deserialize(&mut *self.de).map(Some)

        // the deserializer knows the number of members of the
        // target tuple, so it doesn't need to check the
        // ending marker ')'.
    }
}

struct VariantAccessor<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    variant_member_name: &'a str,
}

impl<'a, 'de> VariantAccessor<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>, variant_member_name: &'a str) -> Self {
        Self {
            de,
            variant_member_name,
        }
    }
}

// `EnumAccess` is provided to the `Visitor` to give it the ability to determine
// which variant of the enum is supposed to be deserialized.
//
// Note that all enum deserialization methods in Serde refer exclusively to the
// "externally tagged" enum representation.
impl<'de, 'a> EnumAccess<'de> for VariantAccessor<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = seed.deserialize(self.variant_member_name.into_deserializer())?;
        Ok((value, self))
    }
}

// `VariantAccess` is provided to the `Visitor` to give it the ability to see
// the content of the single variant that it decided to deserialize.
impl<'de, 'a> VariantAccess<'de> for VariantAccessor<'a, 'de> {
    type Error = Error;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<()> {
        unreachable!()
    }

    // Newtype variants are represented in ASON as `(value)` so
    // deserialize the value here.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        self.de.iter.next(); // consume '('
        self.de.consume_new_line_or_comma_if_exist_ignore_position();

        let v = seed.deserialize(&mut *self.de);
        self.de.consume_new_line_or_comma_if_exist_ignore_position();

        self.de.iter.next(); // consume ')'
        self.de.consume_new_line_or_comma_if_exist_ignore_position();
        v
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_tuple(self.de, len, visitor)
    }

    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.de, "", fields, visitor)
    }
}

struct ObjectAccessor<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> ObjectAccessor<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}

impl<'de, 'a> MapAccess<'de> for ObjectAccessor<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        self.de.consume_new_line_or_comma_if_exist_ignore_position();

        // it seems the struct/object accessor wouldn't stop automatically when
        // it encounters the last field.
        if self.de.iter.equals(0, &Token::RightBrace) {
            return Ok(None);
        }

        // Deserialize a field key.
        seed.deserialize(&mut *self.de).map(Some)

        // the function 'deserialize_identifier' is called here, and then
        // the key name will be obtained.
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        self.de.consume_new_line_or_comma_if_exist_ignore_position();
        self.de.consume_colon()?;
        self.de.consume_new_line_or_comma_if_exist_ignore_position();

        // Deserialize a field value.
        seed.deserialize(&mut *self.de)
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
            assert_eq!(from_str::<i8>(r#"11_i8"#).unwrap(), 11);
            assert_eq!(from_str::<i16>(r#"13_i16"#).unwrap(), 13);
            assert_eq!(from_str::<i32>(r#"17"#).unwrap(), 17);
            assert_eq!(from_str::<i32>(r#"17_i32"#).unwrap(), 17);
            assert_eq!(from_str::<i64>(r#"19_i64"#).unwrap(), 19);
        }

        // unsigned integers
        {
            assert_eq!(from_str::<u8>(r#"11_u8"#).unwrap(), 11);
            assert_eq!(from_str::<u16>(r#"13_u16"#).unwrap(), 13);
            assert_eq!(from_str::<u32>(r#"17_u32"#).unwrap(), 17);
            assert_eq!(from_str::<u64>(r#"19_u64"#).unwrap(), 19);
        }

        // f32
        {
            assert_eq!(from_str::<f32>(r#"123_f32"#).unwrap(), 123_f32);
            assert_eq!(from_str::<f32>(r#"-4.56_f32"#).unwrap(), -4.56_f32);
            assert_eq!(
                from_str::<f32>(r#"3.1415927_f32"#).unwrap(),
                std::f32::consts::PI
            );
            assert_eq!(from_str::<f32>(r#"0_f32"#).unwrap(), 0_f32);
            assert_eq!(from_str::<f32>(r#"-0_f32"#).unwrap(), 0_f32); // -0 == 0
            assert!(from_str::<f32>(r#"NaN_f32"#).unwrap().is_nan()); // NaN != NaN
            assert_eq!(from_str::<f32>(r#"Inf_f32"#).unwrap(), f32::INFINITY);
            assert_eq!(from_str::<f32>(r#"-Inf_f32"#).unwrap(), f32::NEG_INFINITY);
        }

        // f64
        {
            assert_eq!(from_str::<f64>(r#"123.0"#).unwrap(), 123_f64);
            assert_eq!(from_str::<f64>(r#"123_f64"#).unwrap(), 123_f64);
            assert_eq!(from_str::<f64>(r#"-4.56"#).unwrap(), -4.56_f64);
            assert_eq!(
                from_str::<f64>(r#"3.141592653589793"#).unwrap(),
                std::f64::consts::PI
            );
            assert_eq!(from_str::<f64>(r#"0_f64"#).unwrap(), 0_f64);
            assert_eq!(from_str::<f64>(r#"-0_f64"#).unwrap(), 0_f64); // -0 == 0
            assert!(from_str::<f64>(r#"NaN"#).unwrap().is_nan()); // NaN != NaN
            assert!(from_str::<f64>(r#"NaN_f64"#).unwrap().is_nan()); // NaN != NaN
            assert_eq!(from_str::<f64>(r#"Inf"#).unwrap(), f64::INFINITY);
            assert_eq!(from_str::<f64>(r#"-Inf"#).unwrap(), f64::NEG_INFINITY);
            assert_eq!(from_str::<f64>(r#"Inf_f64"#).unwrap(), f64::INFINITY);
            assert_eq!(from_str::<f64>(r#"-Inf_f64"#).unwrap(), f64::NEG_INFINITY);
        }

        // char
        {
            assert_eq!(from_str::<char>(r#"'a'"#).unwrap(), 'a');
            assert_eq!(from_str::<char>(r#"'文'"#).unwrap(), '文');
            assert_eq!(from_str::<char>(r#"'🍒'"#).unwrap(), '🍒');
            assert_eq!(from_str::<char>(r#"'\\'"#).unwrap(), '\\');
            assert_eq!(from_str::<char>(r#"'\''"#).unwrap(), '\'');
            assert_eq!(from_str::<char>(r#"'\"'"#).unwrap(), '"');
            assert_eq!(from_str::<char>(r#"'\t'"#).unwrap(), '\t');
            assert_eq!(from_str::<char>(r#"'\r'"#).unwrap(), '\r');
            assert_eq!(from_str::<char>(r#"'\n'"#).unwrap(), '\n');
            assert_eq!(from_str::<char>(r#"'\0'"#).unwrap(), '\0');
            assert_eq!(from_str::<char>(r#"'\u{8431}'"#).unwrap(), '萱');
        }

        // string
        {
            assert_eq!(
                from_str::<String>(r#""abc文字🍒""#).unwrap(),
                "abc文字🍒".to_owned()
            );
            assert_eq!(
                from_str::<String>(r#""abc\"\\\t\0xyz""#).unwrap(),
                "abc\"\\\t\0xyz".to_owned()
            );
            assert_eq!(
                from_str::<String>(r#""hello\nworld""#).unwrap(),
                "hello\nworld".to_owned()
            );
            assert_eq!(
                from_str::<String>(r#""\u{5c0f}\u{8431}脚本""#).unwrap(),
                "小萱脚本".to_owned()
            );

            assert_eq!(
                from_str::<String>(
                    r#"
            r"a\nb"
            "#
                )
                .unwrap(),
                "a\\nb".to_owned()
            );

            assert_eq!(
                from_str::<String>(
                    r#"
            """
            a
            \tb
                c
            """
            "#
                )
                .unwrap(),
                "a\n\\tb\n    c".to_owned()
            );
        }
    }

    #[test]
    fn test_byte_data() {
        assert_eq!(
            from_str::<ByteBuf>(r#"h"0b 0d 11 13""#).unwrap(),
            ByteBuf::from(vec![11u8, 13, 17, 19])
        );

        assert_eq!(
            from_str::<ByteBuf>(r#"h"61 62 63""#).unwrap(),
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
    fn test_list() {
        assert_eq!(
            from_str::<Vec<i32>>(r#"[11,13,17,19]"#).unwrap(),
            vec![11, 13, 17, 19]
        );

        assert_eq!(
            from_str::<Vec<u8>>(
                r#"[
    97_u8
    98_u8
    99_u8
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

        // // variants
        // assert_eq!(
        //     from_str::<Vec<Option<i32>>>(
        //         r#"[
        //     Option::Some(11)
        //     Option::None
        //     Option::Some(13)
        // ]"#
        //     )
        //     .unwrap(),
        //     vec![Some(11), None, Some(13)]
        // );

        // nested list
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
97_u8
98_u8
99_u8
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

        // // variant
        // assert_eq!(
        //     from_str::<(Option<i32>, Option<i32>, Option<i32>)>(
        //         r#"(
        // Option::Some(11), Option::None, Option::Some(13))"#
        //     )
        //     .unwrap(),
        //     (Option::Some(11), Option::<i32>::None, Option::Some(13))
        // );

        // nested tuple
        assert_eq!(
            from_str::<((i32, i32), (i32, i32), (i32, i32))>(r#"((11, 13), (17, 19), (23, 29))"#)
                .unwrap(),
            ((11, 13), (17, 19), (23, 29))
        );
    }

    #[test]
    fn test_object() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Object {
            id: i32,
            name: String,
            checked: bool,
        }

        assert_eq!(
            from_str::<Object>(r#"{id: 123, name: "foo", checked: true}"#).unwrap(),
            Object {
                id: 123,
                name: "foo".to_owned(),
                checked: true
            }
        );

        // nested object
        #[derive(Deserialize, Debug, PartialEq)]
        struct Address {
            code: i32,
            city: String,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct NestedObject {
            id: i32,
            name: String,
            address: Box<Address>,
        }

        assert_eq!(
            from_str::<NestedObject>(
                r#"{
    id: 456
    name: "bar"
    address: {
        code: 518000
        city: "sz"
    }
}"#
            )
            .unwrap(),
            NestedObject {
                id: 456,
                name: "bar".to_owned(),
                address: Box::new(Address {
                    code: 518000,
                    city: "sz".to_owned()
                })
            }
        )
    }

    #[test]
    fn test_variant_with_single_value() {
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
            from_str::<Color>(r#"Color::Grey(11_u8)"#).unwrap(),
            Color::Grey(11)
        );

        //         #[derive(Deserialize, Debug, PartialEq)]
        //         enum Member {
        //             Code(i32),
        //             Name(String),
        //         }
        //
        //         assert_eq!(
        //             from_str::<Member>(r#"Member::Code(11)"#).unwrap(),
        //             Member::Code(11)
        //         );
        //
        //         assert_eq!(
        //             from_str::<Member>(r#"Member::Name("foo")"#).unwrap(),
        //             Member::Name("foo".to_owned())
        //         );

        // nested
        #[derive(Deserialize, Debug, PartialEq)]
        enum Apperance {
            Transparent,
            Color(Color),
        }

        assert_eq!(
            from_str::<Apperance>(r#"Apperance::Transparent"#).unwrap(),
            Apperance::Transparent
        );

        assert_eq!(
            from_str::<Apperance>(r#"Apperance::Color(Color::Blue)"#).unwrap(),
            Apperance::Color(Color::Blue)
        );

        assert_eq!(
            from_str::<Apperance>(r#"Apperance::Color(Color::Grey(13_u8))"#).unwrap(),
            Apperance::Color(Color::Grey(13))
        );
    }

    #[test]
    fn test_variant_with_multiple_values() {
        #[allow(clippy::upper_case_acronyms)]
        #[derive(Deserialize, Debug, PartialEq)]
        enum Color {
            RGB(u8, u8, u8),
            Grey(u8),
        }

        assert_eq!(
            from_str::<Color>(r#"Color::RGB(255_u8,127_u8,63_u8)"#).unwrap(),
            Color::RGB(255, 127, 63)
        );
    }

    #[test]
    fn test_variant_with_object_value() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum Shape {
            Circle(i32),
            Rect { width: i32, height: i32 },
        }

        assert_eq!(
            from_str::<Shape>(
                r#"Shape::Rect{
    width: 200
    height: 100
}"#
            )
            .unwrap(),
            Shape::Rect {
                width: 200,
                height: 100
            }
        );

        assert_eq!(
            from_str::<Shape>(r#"Shape::Circle(127)"#).unwrap(),
            Shape::Circle(127)
        );
    }

    #[test]
    fn test_mix_list_and_tuple() {
        assert_eq!(
            from_str::<Vec<(i32, String)>>(
                r#"[
    (1, "foo")
    (2, "bar")
]"#
            )
            .unwrap(),
            vec![(1, "foo".to_owned()), (2, "bar".to_owned())]
        );

        assert_eq!(
            from_str::<(Vec<i32>, Vec<String>)>(
                r#"([
    11
    13
], [
    "foo"
    "bar"
])"#
            )
            .unwrap(),
            (vec![11, 13], vec!["foo".to_owned(), "bar".to_owned()])
        );
    }

    #[test]
    fn test_mix_list_and_object() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Object {
            id: i32,
            name: String,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct ObjectList {
            id: i32,
            items: Vec<i32>,
        }

        assert_eq!(
            from_str::<Vec<Object>>(
                r#"[
    {
        id: 11
        name: "foo"
    }
    {
        id: 13
        name: "bar"
    }
]"#
            )
            .unwrap(),
            vec![
                Object {
                    id: 11,
                    name: "foo".to_owned()
                },
                Object {
                    id: 13,
                    name: "bar".to_owned()
                }
            ]
        );

        assert_eq!(
            from_str::<ObjectList>(
                r#"{
    id: 456
    items: [
        11
        13
        17
        19
    ]
}"#
            )
            .unwrap(),
            ObjectList {
                id: 456,
                items: vec![11, 13, 17, 19]
            }
        );
    }

    #[test]
    fn test_mix_tuple_and_object() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Object {
            id: i32,
            name: String,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct ObjectDetail {
            id: i32,
            address: (i32, String),
        }

        assert_eq!(
            from_str::<(i32, Object)>(
                r#"(123, {
                id: 11
                name: "foo"
            })"#
            )
            .unwrap(),
            (
                123,
                Object {
                    id: 11,
                    name: "foo".to_owned()
                }
            )
        );

        assert_eq!(
            from_str::<ObjectDetail>(
                r#"{
    id: 456
    address: (11, "sz")
}"#
            )
            .unwrap(),
            ObjectDetail {
                id: 456,
                address: (11, "sz".to_owned())
            }
        );
    }

    #[test]
    fn test_mix_variant_object() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Simple {
            id: i32,
            name: String,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Complex {
            id: i32,
            checked: bool,
            fullname: String,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        enum Item {
            Empty,
            Minimal(i32),
            Simple(Simple),
            Complex(Complex),
        }

        assert_eq!(
            from_str::<Vec<Item>>(
                r#"[
    Item::Empty
    Item::Minimal(11)
    Item::Simple({
        id: 13
        name: "foo"
    })
    Item::Complex({
        id: 17
        checked: true
        fullname: "foobar"
    })
]"#
            )
            .unwrap(),
            vec![
                Item::Empty,
                Item::Minimal(11),
                Item::Simple(Simple {
                    id: 13,
                    name: "foo".to_owned()
                }),
                Item::Complex(Complex {
                    id: 17,
                    checked: true,
                    fullname: "foobar".to_owned()
                })
            ]
        );
    }
}
