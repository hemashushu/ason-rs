// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

// use serde::de;
//
// use super::Result;
// use crate::{
//     error::Error,
//     process::{
//         lexer::{filter, lex, Token},
//         lookaheaditer::LookaheadIter,
//     },
// };
//
// pub struct Deserializer<'a> {
//     iter: LookaheadIter<'a, Token>,
// }

// pub fn from_str<'a, T>(s: &'a str) -> Result<T>
// where
//     T: de::Deserialize<'a>,
// {
//     let mut chars = s.chars();
//     let mut char_iter = PeekableIterator::new(&mut chars, 3);
//     let tokens = lex(&mut char_iter)?;
//     let effective_tokens = filter(tokens);
//     let mut token_iter = effective_tokens.into_iter();
//     let peekable_token_iter = PeekableIterator::new(&mut token_iter, 2);
//     let mut deserializer = Deserializer::from_token_iter(peekable_token_iter)?;
//     T::deserialize(&mut deserializer)
// }
//
// impl<'a> Deserializer<'a> {
//     pub fn from_token_iter(iter: PeekableIterator<'a, Token>) -> Result<Self> {
//         Ok(Self { iter })
//     }
// }
//
// impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
//     type Error = Error;
//
//     fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_tuple_struct<V>(
//         self,
//         name: &'static str,
//         len: usize,
//         visitor: V,
//     ) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_struct<V>(
//         self,
//         name: &'static str,
//         fields: &'static [&'static str],
//         visitor: V,
//     ) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_enum<V>(
//         self,
//         name: &'static str,
//         variants: &'static [&'static str],
//         visitor: V,
//     ) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
//
//     fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
//     where
//         V: de::Visitor<'de>,
//     {
//         todo!()
//     }
// }

#[cfg(test)]
mod tests {
    //     use serde::Deserialize;
    //
    //     use crate::from_str;
    //
    //     #[test]
    //     fn test_struct() {
    //         #[derive(Deserialize, PartialEq, Debug)]
    //         struct Test {
    //             int: u32,
    //             seq: Vec<String>,
    //         }
    //
    //         let j = r#"{"int":1,"seq":["a","b"]}"#;
    //         let expected = Test {
    //             int: 1,
    //             seq: vec!["a".to_owned(), "b".to_owned()],
    //         };
    //         assert_eq!(expected, from_str(j).unwrap());
    //     }
    //
    //     #[test]
    //     fn test_enum() {
    //         #[derive(Deserialize, PartialEq, Debug)]
    //         enum E {
    //             Unit,
    //             Newtype(u32),
    //         }
    //
    //         let j = r#"E::Unit"#;
    //         let expected = E::Unit;
    //         assert_eq!(expected, from_str(j).unwrap());
    //
    //         let j = r#"E::Newtype(1)"#;
    //         let expected = E::Newtype(1);
    //         assert_eq!(expected, from_str(j).unwrap());
    //     }
}
