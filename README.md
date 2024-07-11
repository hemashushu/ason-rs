# ASON

_ASON_ is a data format that evolved from JSON, introducing strong data typing and support for variant types, all while prioritizing readability and maintainability.

ASON is well-suited for application configuration files, data interchange, and data storage.

(_ASON_ stands for _XiaoXuan Script Object Notation_)

**Features**

- **JSON Compatibility:** ASON integrates with most JSON and JSON5 syntax, making it easy for JSON users to transition to ASON.

- **Simple and Consistent Syntax:** ASON's syntax closely resembles JavaScript and Rust, featuring support for comments, omitting double quotes for structure (object) field names, and allowing trailing commas in the last array element. These features enhance familiarity and writing fluency.

- **Strong Data Typing:** ASON numbers can be explicitly typed (e.g., byte, int, long, double), and integers can be represented in hexdecimal and binary formats. Additionally, new data types such as `date`, `tuple`, `byte data`, `char` are introduced, enabling more precise and rigorous data representation.

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
  - [Serialization and  Deserialization](#serialization-and--deserialization)
  - [Support for Rust Data Types](#support-for-rust-data-types)
  - [Parser and Writer](#parser-and-writer)
  - [Utilities](#utilities)
- [Documentation](#documentation)
- [Source code](#source-code)
- [License](#license)

<!-- /code_chunk_output -->

## Example

The following is an example of ASON document:

```json5
{
    string: "hello world"
    raw_string: r"[a-z]+\d+"
    number: 123
    number_with_explicit_type: 123_456_789@long
    float: 3.14
    double: 6.62607015e-34@double
    bool: true
    date: d"2023-03-24 12:30:00+08:00"
    variant: Option::None
    variant_with_value: Option::Some(123)
    array: [1, 2, 3]
    tuple: (1, "foo", true)
    object: {
        id: 456
        name: "leaf"
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
- Support for line comments, block comments, and documentation comment is added.
- The `Variant` data type is added, and the `null` value is removed.
- New data types such as `Date`, `Tuple`, `Byte Data`, and `Char` are added.
- Improvement: Strings are consistently represented using double quotes.
- Improvement: `List` requires all elements to be of the same data type.
- Improvement: A trailing comma is allowed at the end of the last element of `List`, `Tuple` and `Object`.

### Compared to YAML and TOML

When the data document contains a small amount of contect, all three formats are simple (ASON may have one more pair of braces) and can express the content well. Therefor, any of them can be used as the configuration file for an application or project.

However, when the data document is large, YAML uses indentation to represent hierarchical relationships, so the number of space characters in the prefix needs to be carefully controlled, and it is easy to make mistakes when retreating multiple layers. TOML is not good at expressing hierarchical relationships and lists.

ASON, on the other hand, has good consistency regardless of the size of the data document.

## File Extension

The file extension for ASON document is `*.ason`. Filename example:

`sample.ason`, `package.ason`

## Library and APIs

The Rust library [ason](https://github.com/hemashushu/ason-rs) provides two set of APIs for accessing ASON: one based on [serde](https://github.com/serde-rs/serde) for serialization and deserialization, and the other which is named Parser and Writer for low-level access.

In general, it is recommended to use the serde API as it is simple enough to meet most needs.

### Installation

Run the command `$ cargo add ason` in the directory of your Rust project to add the `ason` library to your project.

### Serialization and  Deserialization

Consider the following ASON document:

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

As you can see, this document consists of two types of objects: a top-level object with `name` and `type` fields, and another object in the form of a list that only contains `name` and `version` fields. We need to create Rust structs corresponding to these two objects. First, create a struct named "Dependency":

```rust
#[derive(Serialize, Deserialize)]
struct Dependency {
    name: String,
    version: Option<String>,
}
```

Note that this struct has a `derive` attribute, in which `Serialize` and `Deserialize` are traits provided by the serde serialization framework. Applying them to a struct or enum allows the struct or enum to be serialized and deserialized.

Then create a struct named "Package":

```rust
#[derive(Serialize, Deserialize)]
struct Package {
    name: String,

    #[serde(rename = "type")]
    pkg_type: Type,

    version: String,
    dependencies: Vec<Dependency>,
}
```

Since "type" is a keyword in the Rust language, the above struct uses "pkg_type" as the field name, and then uses the `#[serde(rename = "type")]` attribute to tell serde to serialize the field as "type". We also need to note that the value of this field is an enum, so we create an enum named "Type":

```rust
#[derive(Serialize, Deserialize)]
enum Type {
    Application,
    Library,
}
```

Note that if the value of an enum contains a struct, or if a struct contains another struct, it is recommended to wrap the num or struct in a `Box`, e.g.

```rust
struct Address {...}
struct User {
    name: String,
    address: Box<Address>
}
```

Now that the preparation is done, the following statements can be used to deserialize the ASON document into a Rust struct instance:

```rust
let text = "..."; // The ASON document text
let package = ason::from_str::<Package>(text).unwrap();
```

the following statements can be used to serialize a Rust struct instance to a string:

```rust
let package = Package{...}; // Build Package instance
let text = ason::to_string(&package);
```

### Support for Rust Data Types

ASON natively supports most Rust data types, including tuples, enums and vectors. Since ASON is also strongly typed, both serialization and deserialization can ensure data accuracy. ASON is more compatible with Rust's type system than other data formats (such as JSON, YAML and TOML). The following is a list of supported Rust data types:

- Signed and unsigned integers, from i8 to i64 and from u8 to u64
- Floating point numbers, including f32 and f64
- Bool
- Char
- String
- Vec
- Enum
- Struct
- Tuple

However, for simplicity, some Rust data types are not supported, such as:

- &str
- &[...]
- Unit (i.e. `()`)
- Unit struct, such as `sturct Foo;`
- New type struct, such as `struct Width(u32);`
- Tuple struct, such as `struct RGB(u8, u8, u8);`
- Tuple enum, such as `enum Number { Complex(f32, f32), ...}`
- Struct enum, such as `enum Shape { Rect {width: u32, height: u32}, ...}`

It is worth nothing that the [serde framework's data model](https://serde.rs/data-model.html) does not include the `DateTime` type, so ASON's `Date` cannot be directly serialized or deserialized to Rust's `chrono::DateTime`. If you serialize a `chrono::DateTime` type value directly, you will get a regular string. A workaround is to wrap the `chrono::DateTime` value as an `ason::Date` type. For more details, please refer to the 'test_serialize' unit test in `ason::serde::serde_date::tests` in the library source code.

In addition, serde treats fixed-length arrays such as `[i32; 4]` as tuples rather than vectors, so the array `[11, 13, 17, 19]` will be serialized as "(11, 13, 17, 19)".

### Parser and Writer

This is an low-level API provided by the `ason` library. The parser is used to parse an ASON document into an AST (Abstract Syntax Tree), and the writer is used to convert the AST into a string. They can be used to achieve purposes such as validating ASON document syntax and formatting.

Consider the following ASON document:

```json5
{
    id: 123
    name: "foo"
    orders: [11, 13]
}
```

The following code demostrates using the parser to convert the above document to an AST, and then using the writer to convert the AST to a string:

```rust
let text = "..."; // The above ASON document text
let node = ason::process::parser::from_str(text).unwrap();

assert_eq!(
    node,
    AsonNode::Object(vec![
        NameValuePair {
            name: "id".to_owned(),
            value: Box::new(AsonNode::Number(NumberLiteral::Int(123)))
        },
        NameValuePair {
            name: "name".to_owned(),
            value: Box::new(AsonNode::String_("foo".to_owned()))
        },
        NameValuePair {
            name: "orders".to_owned(),
            value: Box::new(AsonNode::Array(vec![
                AsonNode::Number(NumberLiteral::Int(11)),
                AsonNode::Number(NumberLiteral::Int(13))
            ]))
        }
    ])
);

let s = ason::process:writer::to_string(&node);
assert_eq!(text, s);

```

### Utilities

The Rust `ason` library also provides a utility which can be used to read and validate, or format an ASON document.

First install the utility with the following command:

`$ cargo install ason`

This command will add the executable `ason` to the `~/.cargo/bin` directory.

This usage of utility is:

```bash
$ ason <file_name>
```

For example:

`$ ason test.ason`

If the document "test.ason" has no errors, the program prints the formatted document to the terminal. The output can be redirected to a new file, e.g.:

`$ ason test.ason > test_formatted.ason`

## Documentation

See the [document](https://hemashushu.github.io/works/ason/) for more information.

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
