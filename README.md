# ASON

_ASON_ is a data format that evolved from JSON, introducing strong data typing and support for variant types, all while prioritizing readability and maintainability.

ASON is well-suited for application configuration files and data transmission.

> _ASON_ stands for _XiaoXuan Script Object Notation_

**Features**

- **JSON Compatibility:** ASON integrates with most JSON and JSON5 syntax, making it easy for JSON users to transition to ASON.

- **Simple and Consistent Syntax:** ASON's syntax closely resembles JavaScript and Rust, featuring support for comments, omitting double quotes for structure (object) field names, and allowing trailing commas in the last array element. These features enhance familiarity and writing fluency.

- **Strong Data Typing:** ASON numbers can be explicitly typed (e.g., byte, int, long, double), and integers can be represented in hexdecimal and binary formats. Additionally, new data types such as `date`, `tuple`, `byte data`, `char` are introduced, enabling more precise and rigorous data representation.

- **Native Variant Data Type Support, Eliminating Null Values:** ASON natively supports variant data types (also known as _algebraic types_, similar to enums in Rust). This enables seamless serialization of complex data structures from high-level programming languages. Importantly, it eliminates the error-prone `null` value.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Example](#example)
- [File Extension](#file-extension)
- [Comparison](#comparison)
  - [JSON](#json)
  - [YAML and TOML](#yaml-and-toml)
- [Rust library](#rust-library)
  - [The base processor](#the-base-processor)
  - [Serde](#serde)
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
    number_with_explicit_data_type: 123_456_789@long
    float: 3.14
    double: 6.626_070_15e-34@double
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

## File Extension

The file extension for ASON document is `*.ason`. Filename example:

`sample.ason`, `package.ason`

## Comparison

### JSON

The syntax of ASON is similar to the JSON, but there are a few significant differences:

- Numbers can have explicit data types.
- Floating-point numbers do not support values such as `Inf`, `-Inf`, `-0` and `NaN`.
- Hexadecimal and binary numbers are supported.
- The `null` value is not allowed, and is replaced by a new type called `Variant`.
- Object keys do not support double quotes.
- Strings do not support single quotes.
- Arrays require all of their elements to be of the same data type.
- Multiple formats of strings and comments are supported.
- `Date`, `Tuple`, `Byte Data`, `Char` and `Variant` data types are added.
- Trailing commas can be omitted.
- A comma can be added to the end of the last element of an array, tuple, or object.

### YAML and TOML

TODO

## Rust library

Currently, only the Rust implementation of ASON serialization and deserialization library is provided.

Run the command `cargo add ason` in your project directory to add the ASON library to your project.

### The base processor

This library provides two functions:

- `fn parse(s: &str) -> Result<AsonNode, ParseError>` for deserialization;
- `fn write(n: &AsonNode) -> String` for serialization.

**Parser**

Suppose you have the following ASON text, which may come from a file or from the internet:

```json5
{
    id: 123
    name: "foo"
}
```

The following code shows how to parse this text into an ASON object and check the value of each member:

```rust
use ason::process::parser::from_str;

let text = r#"{
    id: 123
    name: "foo"
}"#;

let node = from_str(text).unwrap();

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
    ])
);
```

**Writer**

Suppose there is an object with two fields, their names and values are:

- name: "foo"
- version: "0.1.0"

The following code demonstrates how to convert this object into ASON text:

```rust
use use ason::process::writer::to_string;

let node = AsonNode::Object(vec![
    NameValuePair {
        name: "name".to_owned(),
        value: Box::new(AsonNode::String_("foo".to_owned())),
    },
    NameValuePair {
        name: "version".to_owned(),
        value: Box::new(AsonNode::String_("0.1.0".to_owned())),
    },
]);

let text = to_string(&node);

assert_eq!(
    text,
    r#"{
    name: "foo"
    version: "0.1.0"
}"#
    );
```

The output text should be:

```json5
{
    name: "foo"
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

### Serde

**Deserialization**

> A fixed-length array is treated as tuple

struct
unit enum
new type enum

does not support:
- &str
- &[u8]
- unit/()
- unit struct
- new type struct
- tuple struct
- tuple enum
- struct enum

**Serialization**

## Utilities

The Rust ASON library also provides a utility "ason" which can be used to read and validate, or format an ASON document.

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

`$ ason test.ason > new.ason`

## Documentation

See the [document](https://hemashushu.github.io/works/ason/) for more information.

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
