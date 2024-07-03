// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use super::Result;
use crate::error::Error;

use serde::{ser, Serialize};

const DEFAULT_INDEXT_CHARS: &str = "    ";

pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer::new(DEFAULT_INDEXT_CHARS);

    value.serialize(&mut serializer)?;
    Ok(serializer.buffer.join(""))
}

pub struct Serializer {
    // it is possible to generate serialized string by constructing
    // an AsonNode AST, however, it is not easy to maintain a mutable
    // tree in Rust, so the "generate string directly" approach is adopted
    // here.
    // Note that this method is partially redundant with the 'ason::write'.
    buffer: Vec<String>,

    indent_level: usize,
    indent_chars: String,

    flag_is_first_element: bool,
}

impl Serializer {
    fn new(indent_chars: &str) -> Self {
        Self {
            buffer: Vec::new(),
            indent_level: 0,
            indent_chars: indent_chars.to_owned(),
            flag_is_first_element: false,
        }
    }

    // insert text content
    fn append(&mut self, s: String) -> Result<()> {
        self.buffer.push(s);
        Ok(())
    }

    // insert the leading whitespaces
    fn insert_indent(&mut self) {
        let s = self.indent_chars.repeat(self.indent_level);
        self.buffer.push(s);
    }

    fn increase_level(&mut self) {
        self.indent_level += 1;
    }

    fn decrease_level(&mut self) {
        self.indent_level -= 1;
    }

    fn set_first_element_flag(&mut self) {
        self.flag_is_first_element = true;
    }

    fn clear_first_element_flag(&mut self) {
        self.flag_is_first_element = false;
    }

    fn is_first_element(&self) -> bool {
        self.flag_is_first_element
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<()> {
        match v {
            true => self.append("true".to_owned()),
            false => self.append("false".to_owned()),
        }
    }

    fn serialize_i8(self, v: i8) -> Result<()> {
        self.append(format!("{}@byte", v))
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        self.append(format!("{}@short", v))
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        // 'i32' is the default type for integer numbers,
        // so no explicit type name is needed.
        self.append(format!("{}", v))
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        self.append(format!("{}@long", v))
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.append(format!("{}@ubyte", v))
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        self.append(format!("{}@ushort", v))
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        self.append(format!("{}@uint", v))
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        self.append(format!("{}@ulong", v))
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        if v.is_infinite() {
            return Err(Error::Message("ASON does not support \"Inf\".".to_owned()));
        }

        if v.is_nan() {
            return Err(Error::Message("ASON does not support \"NaN\".".to_owned()));
        }

        if v == -0.0f32 {
            return Err(Error::Message("ASON does not support \"-0\".".to_owned()));
        }

        // 'f32' is the default type for floating-point numbers.
        // so there is no need for an explicit type name, although a
        // decimal point needs to be appended if there is no decimal point
        // in the literal.
        let mut s = v.to_string();
        if !s.contains('.') {
            s.push_str(".0");
        }

        self.append(s)
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        if v.is_infinite() {
            return Err(Error::Message("ASON does not support \"Inf\".".to_owned()));
        }

        if v.is_nan() {
            return Err(Error::Message("ASON does not support \"NaN\".".to_owned()));
        }

        if v == -0.0f64 {
            return Err(Error::Message("ASON does not support \"-0\".".to_owned()));
        }

        self.append(format!("{}@double", v))
    }

    fn serialize_char(self, v: char) -> Result<()> {
        let s = match v {
            '\\' => "\\\\".to_owned(),
            '\'' => "\\'".to_owned(),
            // '"' => "\\\"".to_owned(),
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

        self.append(format!("'{}'", s))
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        let s = format!(
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
        );
        self.append(s)
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        // [u8]
        // Similar to strings, during deserialization byte arrays can be transient,
        // owned, or borrowed.
        let s = format!(
            "h\"{}\"",
            v.iter()
                .map(|item| format!("{:02x}", item))
                .collect::<Vec<String>>()
                .join(":")
        );
        self.append(s)
    }

    fn serialize_none(self) -> Result<()> {
        self.append("Option::None".to_owned())
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.append("Option::Some(".to_owned())?;
        value.serialize(&mut *self)?;
        self.append(")".to_owned())
    }

    fn serialize_unit(self) -> Result<()> {
        // The type of `()` in Rust.
        // It represents an anonymous value containing no data.
        Err(Error::Message("ASON does not support \"Unit\".".to_owned()))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        // For example `struct Unit` or `PhantomData<T>`.
        // It represents a named value containing no data.
        Err(Error::Message(
            "ASON does not support \"Unit Struct\".".to_owned(),
        ))
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        // For example the` E::A` and `E::B` in `enum E { A, B }`.
        self.append(format!("{}::{}", name, variant))
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // For example `struct Millimeters(u8)`.
        Err(Error::Message(
            "ASON does not support \"New Type Struct\".".to_owned(),
        ))
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // For example the `E::N` in `enum E { N(u8) }`.
        self.append(format!("{}::{}(", name, variant))?;
        value.serialize(&mut *self)?;
        self.append(")".to_owned())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.append("[".to_owned())?;
        self.set_first_element_flag();
        self.increase_level();
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        // Note that fixed length vectors (arrays, lists)
        // will be treated as tuples, e.g.
        // [i32; 4]
        //
        // per: https://serde.rs/data-model.html

        self.append("(".to_owned())?;
        self.set_first_element_flag();
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        // A named tuple, for example `struct Rgb(u8, u8, u8)`.
        Err(Error::Message(
            "ASON does not support \"Tuple Struct\".".to_owned(),
        ))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        // For example the `E::T` in `enum E { T(u8, u8) }`.
        Err(Error::Message(
            "ASON does not support \"Tuple Variant\".".to_owned(),
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        // A variably sized heterogeneous key-value pairing,
        // for example `BTreeMap<K, V>`.
        // When serializing, the length may or may not be known before
        // iterating through all the entries. When deserializing,
        // the length is determined by looking at the serialized data.
        Err(Error::Message("ASON does not support \"Map\".".to_owned()))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        self.append("{".to_owned())?;
        self.set_first_element_flag();
        self.increase_level();
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        // For example the `E::S` in `enum E { S { r: u8, g: u8, b: u8 } }`.
        Err(Error::Message(
            "ASON does not support \"Struct Variant\".".to_owned(),
        ))
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.clear_first_element_flag(); // turn off the 'is_first_element' flag

        self.append("\n".to_owned())?;
        self.insert_indent();
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.decrease_level();
        self.append("\n".to_owned())?;
        self.insert_indent();
        self.append("]".to_owned())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let is_first_element = self.is_first_element();
        self.clear_first_element_flag(); // turn off the 'is_first_element' flag

        if !is_first_element {
            self.append(", ".to_owned())?;
        }

        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.append(")".to_owned())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn end(self) -> Result<()> {
        unreachable!()
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn end(self) -> Result<()> {
        unreachable!()
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, _key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn serialize_value<T>(&mut self, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn end(self) -> Result<()> {
        unreachable!()
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.clear_first_element_flag(); // turn off the 'is_first_element' flag

        self.append("\n".to_owned())?;
        self.insert_indent();
        self.append(format!("{}: ", key))?;
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.decrease_level();
        self.append("\n".to_owned())?;
        self.insert_indent();
        self.append("}".to_owned())
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, _key: &'static str, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn end(self) -> Result<()> {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use serde::Serialize;
    use serde_bytes::ByteBuf;

    use crate::serde::ser::to_string;

    #[test]
    fn test_primitive_types() {
        // bool
        {
            let v0: bool = false;
            assert_eq!(to_string(&v0).unwrap(), r#"false"#);

            let v1: bool = true;
            assert_eq!(to_string(&v1).unwrap(), r#"true"#);
        }

        // signed integers
        {
            let v0: i8 = 11;
            assert_eq!(to_string(&v0).unwrap(), r#"11@byte"#);

            let v1: i16 = 13;
            assert_eq!(to_string(&v1).unwrap(), r#"13@short"#);

            let v2: i32 = 17;
            assert_eq!(to_string(&v2).unwrap(), r#"17"#);

            let v3: i64 = 19;
            assert_eq!(to_string(&v3).unwrap(), r#"19@long"#);
        }

        // unsigned integers
        {
            let v0: u8 = 11;
            assert_eq!(to_string(&v0).unwrap(), r#"11@ubyte"#);

            let v1: u16 = 13;
            assert_eq!(to_string(&v1).unwrap(), r#"13@ushort"#);

            let v2: u32 = 17;
            assert_eq!(to_string(&v2).unwrap(), r#"17@uint"#);

            let v3: u64 = 19;
            assert_eq!(to_string(&v3).unwrap(), r#"19@ulong"#);
        }

        // floating-point f32
        {
            let v0: f32 = 123_f32;
            assert_eq!(to_string(&v0).unwrap(), r#"123.0"#);

            let v1: f32 = std::f32::consts::PI;
            assert_eq!(to_string(&v1).unwrap(), r#"3.1415927"#);
        }

        // floating-point f64
        {
            let v0: f64 = std::f64::consts::E;
            assert_eq!(to_string(&v0).unwrap(), r#"2.718281828459045@double"#);
        }

        // char
        {
            assert_eq!(to_string(&'a').unwrap(), r#"'a'"#);
            assert_eq!(to_string(&'文').unwrap(), r#"'文'"#);
            assert_eq!(to_string(&'🍒').unwrap(), r#"'🍒'"#);

            // escaped characters
            assert_eq!(to_string(&'\\').unwrap(), r#"'\\'"#);
            assert_eq!(to_string(&'\'').unwrap(), r#"'\''"#);

            // double quote does not necessary to be escaped
            assert_eq!(to_string(&'"').unwrap(), r#"'"'"#);
            assert_eq!(to_string(&'\"').unwrap(), r#"'"'"#);

            assert_eq!(to_string(&'\t').unwrap(), r#"'\t'"#);
            assert_eq!(to_string(&'\r').unwrap(), r#"'\r'"#);
            assert_eq!(to_string(&'\n').unwrap(), r#"'\n'"#);
            assert_eq!(to_string(&'\0').unwrap(), r#"'\0'"#);
        }

        // string
        {
            assert_eq!(to_string(&"abc文字🍒").unwrap(), r#""abc文字🍒""#);
            assert_eq!(to_string(&"abc\"\\\t\0xyz").unwrap(), r#""abc\"\\\t\0xyz""#);
        }
    }

    #[test]
    fn test_bytes() {
        let v0 = vec![11u8, 13, 17, 19];
        let v0b = ByteBuf::from(v0);
        assert_eq!(to_string(&v0b).unwrap(), r#"h"0b:0d:11:13""#);

        let v1 = b"abc";
        let v1b = ByteBuf::from(v1);
        assert_eq!(to_string(&v1b).unwrap(), r#"h"61:62:63""#);

        // DEPRECATED
        // let v2 = vec![23u8, 29, 31, 37];
        // let v2b = Binary::Hex(v2);
        // assert_eq!(to_string(&v2b).unwrap(), r#"Binary::Hex("17:1d:1f:25")"#);
    }

    #[test]
    fn test_option() {
        let v0: Option<i32> = None;
        assert_eq!(to_string(&v0).unwrap(), r#"Option::None"#);

        let v1: Option<i32> = Some(123);
        assert_eq!(to_string(&v1).unwrap(), r#"Option::Some(123)"#);
    }

    #[test]
    fn test_variant() {
        #[derive(Serialize)]
        enum Color {
            Red,
            Green,
            Blue,
            Grey(u8),
        }

        {
            let v0 = Color::Red;
            assert_eq!(to_string(&v0).unwrap(), r#"Color::Red"#);

            let v1 = Color::Green;
            assert_eq!(to_string(&v1).unwrap(), r#"Color::Green"#);

            let v2 = Color::Grey(11);
            assert_eq!(to_string(&v2).unwrap(), r#"Color::Grey(11@ubyte)"#);
        }

        // nested
        #[derive(Serialize)]
        enum Apperance {
            Transparent,
            Color(Color),
        }

        {
            let v0 = Apperance::Transparent;
            assert_eq!(to_string(&v0).unwrap(), r#"Apperance::Transparent"#);

            let v1 = Apperance::Color(Color::Blue);
            assert_eq!(to_string(&v1).unwrap(), r#"Apperance::Color(Color::Blue)"#);

            let v2 = Apperance::Color(Color::Grey(13));
            assert_eq!(
                to_string(&v2).unwrap(),
                r#"Apperance::Color(Color::Grey(13@ubyte))"#
            );
        }
    }

    //     #[test]
    //     fn test_datetime() {
    //         let date1 =
    //             Date::Rfc3339(DateTime::parse_from_rfc3339("2024-06-26T16:38:50+08:00").unwrap());
    //         let date2 = Date::Rfc3339(DateTime::parse_from_rfc3339("2024-06-26T16:38:50Z").unwrap());
    //
    //         assert_eq!(
    //             to_string(&date1).unwrap(),
    //             r#"Date::Rfc3339("2024-06-26T16:38:50+08:00")"#
    //         );
    //         assert_eq!(
    //             to_string(&date2).unwrap(),
    //             r#"Date::Rfc3339("2024-06-26T16:38:50Z")"#
    //         );
    //     }

    #[test]
    fn test_array() {
        assert_eq!(
            to_string(&vec![11, 13, 17, 19]).unwrap(),
            r#"[
    11
    13
    17
    19
]"#
        );

        assert_eq!(
            to_string(&"abc".as_bytes()).unwrap(),
            r#"[
    97@ubyte
    98@ubyte
    99@ubyte
]"#
        );

        assert_eq!(
            to_string(&vec!["foo", "bar", "2024"]).unwrap(),
            r#"[
    "foo"
    "bar"
    "2024"
]"#
        );

        assert_eq!(
            to_string(&vec![Some(11), None, Some(13)]).unwrap(),
            r#"[
    Option::Some(11)
    Option::None
    Option::Some(13)
]"#
        );

        // nested seq

        assert_eq!(
            to_string(&vec![vec![11, 13], vec![17, 19], vec![23, 29]]).unwrap(),
            r#"[
    [
        11
        13
    ]
    [
        17
        19
    ]
    [
        23
        29
    ]
]"#
        );
    }

    #[test]
    fn test_tuple() {
        assert_eq!(to_string(&(11, 13, 17, 19)).unwrap(), r#"(11, 13, 17, 19)"#);

        // a fixed-length array is treated as tuple
        assert_eq!(
            to_string(b"abc").unwrap(),
            r#"(97@ubyte, 98@ubyte, 99@ubyte)"#
        );

        assert_eq!(
            to_string(&("foo", "bar", "2024")).unwrap(),
            r#"("foo", "bar", "2024")"#
        );

        assert_eq!(
            to_string(&(Some(11), Option::<i32>::None, Some(13))).unwrap(),
            r#"(Option::Some(11), Option::None, Option::Some(13))"#
        );

        // nested tuple
        assert_eq!(
            to_string(&((11, 13), (17, 19), (23, 29))).unwrap(),
            r#"((11, 13), (17, 19), (23, 29))"#
        );
    }

    #[test]
    fn test_object() {
        #[derive(Serialize)]
        struct Object {
            id: i32,
            name: String,
            checked: bool,
        }

        let v0 = Object {
            id: 123,
            name: "foo".to_owned(),
            checked: true,
        };

        let expected0 = r#"{
    id: 123
    name: "foo"
    checked: true
}"#;
        assert_eq!(to_string(&v0).unwrap(), expected0);

        // nested struct
        #[derive(Serialize)]
        struct Address {
            code: i32,
            city: String,
        }

        #[derive(Serialize)]
        struct NestedObject {
            id: i32,
            name: String,
            address: Box<Address>,
        }

        let v1 = NestedObject {
            id: 456,
            name: "bar".to_owned(),
            address: Box::new(Address {
                code: 518000,
                city: "sz".to_owned(),
            }),
        };

        let expected1 = r#"{
    id: 456
    name: "bar"
    address: {
        code: 518000
        city: "sz"
    }
}"#;

        assert_eq!(to_string(&v1).unwrap(), expected1);
    }

    #[test]
    fn test_mix_array_and_tuple() {
        assert_eq!(
            to_string(&vec![(1, "foo"), (2, "bar")]).unwrap(),
            r#"[
    (1, "foo")
    (2, "bar")
]"#
        );

        assert_eq!(
            to_string(&(vec![11, 13], vec!["foo", "bar"])).unwrap(),
            r#"([
    11
    13
], [
    "foo"
    "bar"
])"#
        );
    }

    #[test]
    fn test_mix_array_and_object() {
        #[derive(Serialize)]
        struct Object {
            id: i32,
            name: String,
        }

        #[derive(Serialize)]
        struct ObjectList {
            id: i32,
            items: Vec<i32>,
        }

        assert_eq!(
            to_string(&vec![
                Object {
                    id: 11,
                    name: "foo".to_owned()
                },
                Object {
                    id: 13,
                    name: "bar".to_owned()
                }
            ])
            .unwrap(),
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
        );

        assert_eq!(
            to_string(&ObjectList {
                id: 456,
                items: vec![11, 13, 17, 19]
            })
            .unwrap(),
            r#"{
    id: 456
    items: [
        11
        13
        17
        19
    ]
}"#
        );
    }

    #[test]
    fn test_mix_tuple_and_object() {
        #[derive(Serialize)]
        struct Object {
            id: i32,
            name: String,
        }

        #[derive(Serialize)]
        struct ObjectDetail {
            id: i32,
            address: (i32, String),
        }

        assert_eq!(
            to_string(&(
                123,
                Object {
                    id: 11,
                    name: "foo".to_owned()
                }
            ))
            .unwrap(),
            r#"(123, {
    id: 11
    name: "foo"
})"#
        );

        assert_eq!(
            to_string(&ObjectDetail {
                id: 456,
                address: (11, "sz".to_owned())
            })
            .unwrap(),
            r#"{
    id: 456
    address: (11, "sz")
}"#
        );
    }

    #[test]
    fn test_mix_variant_object() {
        #[derive(Serialize)]
        struct Simple {
            id: i32,
            name: String,
        }

        #[derive(Serialize)]
        struct Complex {
            id: i32,
            checked: bool,
            fullname: String,
        }

        #[derive(Serialize)]
        enum Item {
            Empty,
            Minimal(i32),
            Simple(Simple),
            Complex(Complex),
        }

        assert_eq!(
            to_string(&vec![
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
            ])
            .unwrap(),
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
        );
    }
}
