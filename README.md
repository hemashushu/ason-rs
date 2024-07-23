# ASON

_ASON_ is a data format that evolved from JSON, introducing strong data typing and support for variant types, all while prioritizing readability and maintainability. ASON is well-suited for application configuration files, data interchange, and data storage.

<!-- (_ASON_ stands for _XiaoXuan Script Object Notation_) -->

**Features**

- **JSON Compatibility:** ASON integrates with base JSON and JSON5 syntax, making it easy for JSON users to transition to ASON.

- **Simple and Consistent Syntax:** ASON's syntax closely resembles JavaScript and Rust, featuring support for comments, omitting double quotes for structure (object) field names, and allowing trailing commas in the last array element. These features enhance familiarity and writing fluency.

- **Strong Data Typing:** ASON numbers can be explicitly typed (e.g., `u8`, `i32`, `f32`, `f64`), and integers can be represented in hexdecimal and binary formats. Additionally, new data types such as `DateTime`, `Tuple`, `ByteData`, `Char` are introduced, enabling more precise and rigorous data representation.

- **Native Variant Data Type Support, Eliminating the Null Value:** ASON natively supports variant data types (also known as _algebraic types_, similar to enums in Rust). This enables seamless serialization of complex data structures from high-level programming languages. Importantly, it eliminates the error-prone `null` value.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Example](#example)
- [Comparison](#comparison)
  - [Compared to JSON](#compared-to-json)
  - [Compared to YAML and TOML](#compared-to-yaml-and-toml)
- [Filename Extension](#filename-extension)
- [Library and APIs](#library-and-apis)
  - [Installation](#installation)
  - [Serialization and Deserialization](#serialization-and-deserialization)
  - [Rust Data and Corresponding ASON Text](#rust-data-and-corresponding-ason-text)
    - [Struct](#struct)
    - [Vec](#vec)
    - [Tuple](#tuple)
    - [Enum](#enum)
  - [Support for Rust Data Types](#support-for-rust-data-types)
  - [AST Parser and Writer](#ast-parser-and-writer)
  - [Utilities](#utilities)
- [Syntax Quick Reference](#syntax-quick-reference)
  - [Primitive Values](#primitive-values)
  - [Multi-Line Strings](#multi-line-strings)
  - [Auto-Trimming Leading Whitespace Strings](#auto-trimming-leading-whitespace-strings)
  - [Long Strings](#long-strings)
  - [Objects](#objects)
  - [Lists](#lists)
  - [Tuples](#tuples)
  - [Variants](#variants)
  - [Comments](#comments)
  - [Documents](#documents)
- [Specification](#specification)
- [Source code](#source-code)
- [License](#license)

<!-- /code_chunk_output -->

## Example

The following is an example of ASON text:

```json5
{
    string: "hello world"
    raw_string: r"[a-z]+\d+"
    integer_number: 123
    floating_point_number: 3.14
    number_with_explicit_type: 10u8
    boolean: true
    datetime: d"2023-03-24 12:30:00+08:00"
    bytedata: h"68 65 6c 6c 6f"

    list: [1, 2, 3]
    tuple: (1, "foo", true)
    object: {
        id: 123
        name: "Alice🍀"
    }

    variant: Option::None
    variant_with_single_value: Option::Some(123)
    variant_with_multiple_values: Color::RGB(255, 127, 63)
    variant_with_object_style_values: Shape::Rect{
        width: 200
        height: 100
    }
}
```

## Comparison

### Compared to JSON

ASON is an improvement over JSON. Overall, ASON's syntax is simpler, more  consistent, and more expressive than JSON. Specifically:

- Trailing commas can be omitted.
- Double quotes for object keys are omitted.
- Numeric data types are added.
- Hexadecimal and binary representations of integers are added.
- Hexadecimal representation of floating-point numbers are added.
- Support for long strings, "raw strings", and "auto-trimmed strings" is added.
- Support for line comments, block comments is added.
- The `Variant` data type is added, and the `null` value is removed.
- New data types such as `Char`, `DateTime`, `Tuple`, `ByteData` are added.
- Improvement: Strings are consistently represented using double quotes.
- Improvement: `List` requires all elements to be of the same data type.
- Improvement: A trailing comma is allowed at the end of the last element of `List`, `Tuple` and `Object`.

### Compared to YAML and TOML

All three formats are simple (ASON may have one more pair of braces) and can express the content well when the data is small. However, when the data is large, YAML uses indentation to represent hierarchy, so the number of space characters in the prefix needs to be carefully controlled, and it is easy to make mistakes when retreating multiple layers. In addition, its [specification](https://yaml.org/spec/) is quite complex. TOML is not good at expressing hierarchy, there are often redundant key names in the text, and the [object list](https://toml.io/en/v1.0.0#array-of-tables) is not as clear as other formats. ASON, on the other hand, has good consistency regardless of the size of the data.

## Filename Extension

The extension name for ASON file is `*.ason`, for example:

`sample.ason`, `package.ason`

## Library and APIs

The Rust library [ason](https://github.com/hemashushu/ason-rs) provides two sets of APIs for accessing ASON text: one based on [serde](https://github.com/serde-rs/serde) for serialization and deserialization, and the other based on AST (Abstract Syntax Tree) for low-level access.

In general, it is recommended to use the serde API since it is simple enough to meet most needs.

### Installation

Run the command `$ cargo add ason` in the directory of your Rust project to add the `ason` library.

### Serialization and Deserialization

Consider the following ASON text:

```json5
{
    name: "foo"
    type: Type::Application
    version: "0.1.0"
    dependencies: [
        {
            name: "random"
            version: Option::None
        }
        {
            name: "regex"
            version: Option::Some("1.0.1")
        }
    ]
}
```

This text consists of two types of objects: a top-level object with `name`, `type`, `version` and `dependencies` fields, and another object in the form of a list that contains `name` and `version` fields. We need to create two Rust structs corresponding to these two objects. First, create a struct named "Dependency":

```rust
#[derive(Serialize, Deserialize)]
struct Dependency {
    name: String,
    version: Option<String>,
}
```

Note that this struct has a `derive` attribute, in which `Serialize` and `Deserialize` are traits provided by the _serde_ serialization framework. Applying them to a struct or enum allows the struct or enum to be serialized and deserialized.

Then create a struct named "Package":

```rust
#[derive(Serialize, Deserialize)]
struct Package {
    name: String,

    #[serde(rename = "type")]
    package_type: PackageType,

    version: String,
    dependencies: Vec<Dependency>,
}
```

Since "type" is a keyword in Rust language, we uses "package_type" as the field name, and then uses the `#[serde(rename = "type")]` attribute to tell _serde_ to serialize the field as "type". Note that the value of this field is an enum, so we create an enum named "PackageType":

```rust
#[derive(Serialize, Deserialize)]
enum PackageType {
    Application,
    Library,
}
```

Now that the preparation is done, the following statements  deserialize the ASON documtextent into a Rust struct instance:

```rust
let text = "..."; // The above ASON text
let package = ason::from_str::<Package>(text).unwrap();
```

The following statements serialize a Rust struct instance to a string:

```rust
let package = Package{...}; // Feel free to build the `Package` instance
let text = ason::to_string(&package);
```

### Rust Data and Corresponding ASON Text

#### Struct

In general, we use structs in Rust to store a group of related data. Rust structs correspond to ASON `Object` data type. Here is an example of a struct named "User" and its instance:

```rust
#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String
}

let v1 = User {
    id: 123,
    name: String::from("John")
};
```

The corresponding ASON text for instance `v1` is:

```json5
{
    id: 123
    name: "John"
}
```

Real-world data is often complex, for example, a struct containing another struct to form a hierarchical relationship. The following code demonstrates struct `User` contains a child struct named `Address`:

```rust
#[derive(Serialize, Deserialize)]
struct Address {
    city: String,
    street: String
}

#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    address: Box<Address>
}

let v2 = User {
    id: 123,
    name: String::from("John"),
    address: Box::new(Address{
        city: String::from("Shenzhen"),
        street: String::from("Xinan")
    })
}
```

The corresponding ASON text for instance `v2`:

```json5
{
    id: 123
    name: "John"
    address: {
        city: "Shenzhen"
        street: "Xinan"
    }
}
```

#### Vec

`Vec` (vector) is another common data structure in Rust, which is used for storing a series of similar data. `Vec` corresponds to ASON `List` data type. The following code demonstrates adding a field named `orders` to the struct `User` to store order numbers:

```rust
#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    orders: Vec<i32>
}

let v3 = User {
    id: 123,
    name: String::from("John"),
    orders: vec![11, 13, 17, 19]
};
```

The corresponding ASON text for instance `v3` is:

```json5
{
    id: 123
    name: "John"
    orders: [11, 13, 17, 19]
}
```

The elements in a vector can be either simple data (such as `i32` in the above example) or complex data, such as struct. The following code demonstrates adding a field named `addresses` to the struct `User` to store shipping addresses:

```rust
#[derive(Serialize, Deserialize)]
struct Address {
    city: String,
    street: String
}

#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    addresses: Vec<Address>
}

let v4 = User {
    id: 123,
    name: String::from("John"),
    address: vec![
        Address {
            city: String::from("Guangzhou"),
            street: String::from("Tianhe")
        },
        Address {
            city: String::from("Shenzhen"),
            street: String::from("Xinan")
        },
    ]
};
```

The corresponding ASON text for instance `v4` is:

```json5
{
    id: 123
    name: "John"
    addresses: [
        {
            city: "Guangzhou"
            street: "Tianhe"
        }
        {
            city: "Shenzhen"
            street: "Xinan"
        }
    ]
}
```

#### Tuple

There is another common data type "tuple" in Rust, which can be considered as structs with omitted field names. For example, in the above example code `orders: Vec<i32>`, if you want the order list to include not only the order number but also the order status, you can use the tuple `(i32, String)` instead of `i32`. The modified code is:

```rust
#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    orders: Vec<(i32, String)>
}

let v5 = User {
    id: 123,
    name: String::from("John"),
    orders: vec![
        (11, String::from("ordered"),
        (13, String::from("shipped"),
        (17, String::from("delivered"),
        (19, String::from("cancelled")
    ]
};
```

The corresponding ASON text for instance `v5` is:

```json5
{
    id: 123
    name: "John"
    orders: [
        (11, "ordered")
        (13, "shipped")
        (17, "delivered")
        (19, "cancelled")
    ]
}
```

Rust tuples correspond to ASON `Tuple` data type. It should be noted that in some programming languages, tuples and vectors are not clearly distinguished, but in Rust they are completely different data types. Vectors require that all elements have the same data type (Rust arrays are similar to vectors, but vectors have a variable number of elements, while arrays have a fixed size that cannot be changed after creation), while tuples do not require that their member data types be the same, but do require a fixed number of members. ASON's definition of `Tuple` is consistent with Rust's.

#### Enum

In the above example, the order status is represented by a string. From historical lessons, we know that a batter solution is to use an enum. Rust enum corresponds to ASON `Variant` data type. The following code uses the enum `Status` to replace the `String` in `Vec<(i32, String)>`.

```rust
#[derive(Serialize, Deserialize)]
enum Status {
    Ordered,
    Shipped,
    Delivered,
    Cancelled
}

#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    orders: Vec<(i32, Status)>
}

let v6 = User {
    id: 123,
    name: String::from("John"),
    orders: vec![
        (11, Status::Ordered),
        (13, Status::Shipped),
        (17, Status::Delivered),
        (19, Status::Cancelled)
    ]
};
```

The corresponding ASON text for instance `v6` is:

```json5
{
    id: 123
    name: "John"
    orders: [
        (11, Status::Ordered)
        (13, Status::Shipped)
        (17, Status::Delivered)
        (19, Status::Cancelled)
    ]
}
```

Rust enum type is actually quite powerful, it can not only represent different categories of something but also carry data. For example, consider the following enum `Color`:

```rust
#[derive(Serialize, Deserialize)]
enum Color {
    Transparent,
    Grayscale(u8),
    Rgb(u8, u8, u8),
    Hsl{
        hue: i32,
        saturation: u8,
        lightness: u8
    }
}
```

There are four types of values in Rust enums:

- No value, e.g., `Color::Transparent`
- With one value, e.g., `Color::Grayscale(u8)`
- Tuple-like with multiple values, e.g., `Color::Rgb(u8, u8, u8)`
- Struct-like with multiple "key-value" pairs, e.g., `Color::Hsl{...}`

ASON `Variant` fully supports all types of values of Rust enums, consider the following instance:

```rust
let v7 = vec![
    Color::Transparent,
    Color::Grayscale(127),
    Color::Rgb(255, 127, 63),
    Color::Hsl{
        hue: 300,
        saturation: 100,
        lightness: 50
    }
];
```

The corresponding ASON text for instance `v7` is:

```json5
[
    Color::Transparent
    Color::Grayscale(127_u8)
    Color::Rgb(255_u8, 127_u8, 63_u8)
    Color::Hsl{
        hue: 300
        saturation: 100_u8
        lightness: 50_u8
    }
]
```

The ASON text closely resembles the Rust data literals, which is intentional. The designers aimed to reduce the learning curve for users by making ASON similar to existing data formats (JSON) and programming languages (Rust).

For more information on ASON data types and specifications, please refer to the subsequent chapters.

### Support for Rust Data Types

ASON natively supports most Rust data types, including tuples, enums and vectors. Since ASON is also strongly typed, both serialization and deserialization can ensure data accuracy. ASON is more compatible with Rust's data types than other data formats (such as JSON, YAML and TOML).

> ASON is a data format that perfectly matches Rust's data types.

The following is a list of supported Rust data types:

- Signed and unsigned integers, from i8/u8 to i64/u64
- Floating point numbers, including f32 and f64
- Boolean
- Char
- String
- Array, such as `[i32; 4]`
- Vec
- Enum
- Struct
- Tuple

However, for simplicity, some Rust data types are not supported, includes:

- Octal integer literals
- Unit (i.e. `()`)
- Unit struct, such as `sturct Foo;`
- New type struct, such as `struct Width(u32);`
- Tuple-like struct, such as `struct RGB(u8, u8, u8);`

It is worth nothing that the [serde framework's data model](https://serde.rs/data-model.html) does not include the `DateTime` type, so ASON `DateTime` cannot be directly serialized or deserialized to Rust's `chrono::DateTime`. If you serialize a `chrono::DateTime` type value, you will get a regular string. A workaround is to wrap the `chrono::DateTime` value as an `ason::Date` type. For more details, please refer to the 'test_serialize' unit test in `ason::serde::serde_date::tests` in the library source code.

In addition, serde treats fixed-length arrays such as `[i32; 4]` as tuples rather than vectors, so the Rust array `[11, 13, 17, 19]` will be serialized as ASON `Tuple` `(11, 13, 17, 19)`.

### AST Parser and Writer

These are low-level API provided by the library. They can be used to validate or format ASON text.

Consider the following ASON text:

```json5
{
    id: 123
    name: "John"
    orders: [11, 13]
}
```

The following code demostrates using the parser to convert the above text to an AST, and then using the writer to convert the AST to a string:

```rust
let text = "..."; // The above ASON text
let node = ason::parse_from(text).unwrap();

assert_eq!(
    node,
    AsonNode::Object(vec![
        NameValuePair {
            name: String::from("id"),
            value: Box::new(AsonNode::Number(Number::Int(123)))
        },
        NameValuePair {
            name: String::from("name"),
            value: Box::new(AsonNode::String_(String::from("John")))
        },
        NameValuePair {
            name: String::from("orders"),
            value: Box::new(AsonNode::Array(vec![
                AsonNode::Number(Number::Int(11)),
                AsonNode::Number(Number::Int(13))
            ]))
        }
    ])
);

let s = ason::write_to(&node);
assert_eq!(text, s);

```

### Utilities

The library also provides a utility which can be used to validate or format an ASON file.

First install the utility with the following command:

`$ cargo install ason`

This command will add the executable `ason` to the `~/.cargo/bin` directory.

The usage of utility is:

`$ ason <filename>`

For example:

`$ ason test.ason`

If the file "test.ason" has no errors, the program prints the formatted text to the terminal. The output can be redirected to a new file, e.g.:

`$ ason test.ason > test_formatted.ason`

## Syntax Quick Reference

ASON is composed of values and optional comments. There are two types of values: primitive and composite.

Primitive values are basic data types like integers, strings, booleans and datetimes. Composite values are more complex structures made up of multiple values, such as lists and objects.

### Primitive Values

Here are examples of different types of primitive values:

- Integers: `123`, `+456`, `-789`
- Floating-point numbers: `3.142`, `+1.414`, `-1.732`
- Floating-point with exponent: `2.998e10`, `6.674e-11`

  Underscores can be inserted between any digits of a number to group them, e.g. `123_456_789`, `6.626_070_e-34`

  The data type of a number can be explicitly specified by appending the type name after the number, e.g. `65u8`, `3.14f32`

  Underscores can also be inserted between the number and the type name, e.g. `933_199_u32`, `6.626e-34_f32`

> Each number in ASON has a specific data type. If not explicitly specified, the default data type for integers is `i32` and for floating-point numbers is `f64`. ASON supports the following numeric data types: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `f32`, `f64`

- Hexadecimal integers: `0x41`, `0x61_u8`
- Binary integers: `0b1100`, `0b0110_0001_u8`
- Floating-point numbers in [C/C++ language hexadecimal floating-point literal format](https://en.wikipedia.org/wiki/Hexadecimal#Hexadecimal_exponential_notation): `0x1.4p3`, `0x1.921f_b6p1_f32`

  Note that you cannot represent a floating-point number by simply appending the "f32" or "f64" suffix to a normal hexadecimal integer, for example `0x21_f32`. This is because the character "f" is one of the hexadecimal digit characters (i.e., `[0-9a-f]`), so `0x21_f32` will only be parsed as a normal hexadecimal integer `0x21f32`.

- Booleans: `true`, `false`
- Characters: `'a'`, `'文'`, `'😊'`
- Escape characters: `'\r'`, `'\n'`, `'\t'`, `'\\'`
- Unicode escape characters: `'\u{2d}'`, `'\u{6587}'`
- Strings: `"abc文字😊"`, `"foo\nbar"`
- Raw strings: `r"[a-z]+\d+"`, `r#"<\w+\s(\w+="[^"]+")*>"#`
- Date and time: `d"2024-03-16"`, `d"2024-03-16 16:30:50"`, `d"2024-03-16T16:30:50+08:00"`
- Byte data:  `h"11 13 17 19"`, `h"4d 6f 6f 6e"`

### Multi-Line Strings

ASON strings allows multiple lines, as shown in the following example:

```json5
{
    multiline_string: "1. Mercury Venus Earth
                     2. Mars Jupiter Saturn
                     3. Uranus Neptune"
}
```

This represents a string with three paragraphs (lines).

### Auto-Trimming Leading Whitespace Strings

ASON supports "auto-trimming leading whitespace string" to improve readability. For instance, in the above example, the second and third lines introduce unnecessary leading whitespaces. While we may not always need these leading spaces, writing each line at the beginning of the line would disrupt the document's indentation. Using the "auto-trimming leading whitespace string" can solve this problem, the modified text would be:

```json5
{
    auto_trimming_string: """
        1. Mercury Venus Earth
        2. Mars Jupiter Saturn
        3. Uranus Neptune
        """
}
```

### Long Strings

Sometimes a string may not need to span multiple lines, but its content is relatively long. To improve readability, ASON supports writing strings across multiple lines. Simply add a `\` symbol at the end of the line and start the new line. The subsequent text will be automatically appended to the current string. For example:

```json5
{
    long_string: "My very educated \
                mother just served \
                us nine pizzas"
}
```

The string in the example is equivalent to `"My very educated mother just served us nine pizzas"`. Note that all leading whitespaces in the lines of the text body will be automatically removed.

### Objects

An object can contain multiple values, each with a name called a _key_. A combination of a key and a value is called a _key-value pair_. An object is a collection of key-value pairs. For example:

```json5
{
    name: "ason",
    version: "1.0.1",
    edition: "2021",
}
```

The comma at the end of each key-value pair is optional. For instance, the ASON text above could be written as:

```json5
{
    name: "ason"
    version: "1.0.1"
    edition: "2021"
}
```

Note that ASON objects allow a comma at the end of the last key-value pair, which is disallowed in JSON. This feature is primarily intended to facilitate the rearrangement of "key-value pairs" (when editing ASON documents).

Of course, multiple key-value pairs can also be written on the same line. In this case, commas are required between key-value pairs. For example:

```json5
{name: "ason", version: "1.0.1", edition: "2021",}
```

The values within an object can be any type, including primitive values (such as numbers, strings, dates) and composite values (such as lists, objects, tuples). In the real world, an object typically contains other objects. For example:

```json5
{
    name: "ason"
    version: "1.0.1"
    edition: "2021"
    dependencies: {
        serde: "1.0"
        chrono: "0.4"
    }
    dev_dependencies: {
        pretty_assertions: "1.4"
    }
}
```

### Lists

A list is a collection of values of the same type, such as:

```json5
[11, 13, 17, 19]
```

Similar to objects, the elements in a list can also be written on separate lines, with optional commas at the end of each line, and a comma is allowed at the end of the last element. For example:

```json5
[
    "Alice",
    "Bob",
    "Carol",
    "Dan",
]
```

and

```json5
[
    "Alice"
    "Bob"
    "Carol"
    "Dan"
]
```

The elements in a list can be of any data type, but all elements in the same list must be of the same type. For instance, the following list is invalid:

```json5
// invalid list due to inconsistent data types of elements
[11, 13, "Alice", "Bob"]
```

If the elements in a list are objects, then the name and number of keys in each object, as well as the data type of the corresponding values, must be consistent. In other words, the type of object is determined by the type of all key-value pairs, and the type of key-value pair is determined by the key name and type of the value. For example, the following list is valid:

```json5
[
    {
        id: 123
        name: "Alice"
    }
    {
        id: 456
        name: "Bob"
    }
]
```

While the following list is invalid:

```json5
// invalid list due to inconsistent types of the two objects.
[
    {
        id: 123
        name: "Alice"
    }
    {
        id: 456
        score: 'A'
    }
]
```

If the elements in a list are lists, then the data type of the elements in each sublist must be the same. In other words, the type of list is determined by the data type of its elements, and the number of elements is irrelevant. For instance, the following list is valid:

```json5
[
    [11, 13, 17]
    [101, 103, 107, 109]
    [211, 223]
]
```

In the above example, the top-level list contains 3 sublists, and although the number of elements in each sublist is different (3, 4 and 2), the data type of the elements is `i32` for all three sublists, making them of the same type.

### Tuples

A tuple can be considered as an object that omits the keys, for example:

```json5
(11, "Alice", true)
```

Tuples are similar in appearance to lists, but tuples do not require the data types of each element to be consistent. Secondly, both the data type and number of the elements are part of the type of tuple, for example `("Alice", "Bob")` and `("Alice", "Bob", "Carol")` are different types of tuples because they don't have the same number of elements.

Similar to objects and lists, the elements of a tuple can also be written on separate lines, with optional commas at the end of each line, and there can be a comma at the end of the last element. For example:

```json5
(
    "Alice",
    11,
    true,
)
```

and

```json5
(
    "Alice"
    11
    true
)
```

### Variants

A variant consists of three parts: the variant type name, the variant member name, and the optional value. For example:

```json5
// Variant without value
Option::None
```

and

```json5
// Variant with a value
Option::Some(11)
```

In the two variants in the above example, "Option" is the variant type name, "None" and "Some" are the variant member names, and "11" is the variant value.

The types are the same as long as the variant type names are the same. For example, `Color::Red` and `Color::Green` are of the same type, while `Option::None` and `Color::Red` are of different types.

If a variant member carries a value, then the type of the value is also part of the type of the variant member. For example, `Option::Some(11)` and `Option::Some(13)` are of the same types, but `Option::Some(11)` and `Option::Some("John")` are of different types.

Therefor, the following list is valid because all elements have the same variant type name and the member `Some` has the same type:

```json5
[
    Option::None
    Option::Some(11)
    Option::None
    Option::Some(13)
]
```

However, the following list is ont valid, although the variant type names of all the elements are consistent, the type of the member `Some` is inconsistent:

```json5
[
    // Invalid list because of inconsistent element types
    Option::None
    Option::Some(11)
    Option::Some("John")
]
```

A variant can carry a value of any type, such as an object:

```json5
Option::Some({
    id: 123
    name: "Alice"
})
```

Or a tuple:

```json5
Option::Some((211, 223))
```

In fact, a variant can also carry multiple values, which can be either object-style or tuple-style, for example:

```json5
// Object-style variant
Shape:Rectangle{
    width: 307
    height: 311
}
```

and

```json5
// Tuple-style variant
Color::RGB(255, 127, 63)
```

### Comments

Like JavaScript and C/C++, ASON also supports two types of comments: line comments and block comments. Comments are for human reading and are completely ignored by the machine.

Line comments start with the `//` symbol and continue until the end of the line. For example:

```json5
// This is a line comment
{
    id: 123 // This is also a line comment
    name: "Bob"
}
```

Block comments start with the `/*` symbol and end with the `*/` symbol. For example:

```json5
/* This is a block comment */
{
    /*
     This is also a block comment
    */
    id: 123 
    name: /* definitely a block comment */ "Bob"
}
```

Unlike JavaScript and C/C++, ASON block comments support nesting. For example:

```json5
/* 
    This is the first level
    /*
        This is the second level
    */
    This is the first level again
*/  
```

The nesting feature of block comments makes it more convenient for us to comment on a piece of code that **already has a block comment**. If block comments do not support nesting, we need to remove the inner block comment first before adding a comment to the outer layer, because the inner block comment symbol `*/` will end all outer block comments prematurely.

### Documents

An ASON document can only contain one value (includes primitive value and composite value), like JSON, a typical ASON document is usually an object or a list. In fact, all types of values are allowed, not limited to objects or lists. For example, a tuple, a variant, even a number or a string is allowed. Just make sure that a document has exactly one value. For example, the following are both valid ASON documents:

```json5
(11, "Alice", true)
```

and

```json5
"Hello World!"
```

While the following two are invalid:

```json5
(11, "Alice", true)
'A' // Invalid ASON document because there is more than one value
```

and

```json5
"Hello World!"
true // Invalid ASON document because there is more than one value
```

## Specification

Please refer to the [Full Document](https://hemashushu.github.io/works/ason/).

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
