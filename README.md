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
- [File Extension](#file-extension)
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

## File Extension

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

...

## Specification

Please refer to the [Full Document](https://hemashushu.github.io/works/ason/).

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
