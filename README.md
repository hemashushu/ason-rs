# ASON

_ASON_ (stands for _XiaoXuan Script Object Notation_) is a data format that is easy for humans to read and write. It is similar to _JSON_, but has many improvements and enhancements.

_ASON_ is mainly used as a configuration file for applications, but can also be used for data transmission.

**Features**

- **Compatible with most of the syntax of _JSON_ and _JSON5_.** If you are already familiar with _JSON_, you do not need to learn a new data format.

- **Simple, rigorous and consistent syntax.** The syntax of _ASON_ is close to that of programming languages. For example, numbers can specify data types, multiple formats of comments and strings are supported, key names do not need to be enclosed in double quotes, trailing commas can be omitted, and so on. These features can help users accurately express the data they need, and are also facilitate the modification and maintenance of _ASON_ documents.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [ASON Document Example](#ason-document-example)
- [Comparison with JSON](#comparison-with-json)
- [File Extension](#file-extension)
- [Rust library](#rust-library)
  - [The base processor](#the-base-processor)
  - [Serde](#serde)
- [Utilities](#utilities)
- [Documentation](#documentation)
- [Source code](#source-code)
- [License](#license)

<!-- /code_chunk_output -->

## ASON Document Example

A typical _ASON_ document contains only one _ASON_ object. The following is a example of an _ASON_ document:

```json5
{
    string: "hello world"
    raw_string: r"[a-z]+"
    number: 123
    number_with_data_type: 123_456_789@long
    float: 3.14
    double: 6.626e-34@double
    bool: true
    date: d"2023-03-24 12:30:00+08:00"
    variant_with_value: Option::Some(123)
    variant_without_value: Option::None
    array: [1,2,3,]
    tuple: (1, "foo", true)
    object: {
        id: 123
        name: "leaf"
    }
}
```

## Comparison with JSON

The _ASON_ format is very similar to the _JSON_ format, but there are a few significant differences:

- Numbers can have explicit data types.
- Floating-point numbers do not support values such as `Inf`, `-Inf`, `-0` and `NaN`.
- Hexadecimal and binary numbers are supported.
- The `null` value is not allowed, and is replaced by a new type called `Variant`.
- Object keys do not support double quotes.
- Strings do not support single quotes.
- Arrays require all of their elements to be of the same data type.
- Multiple formats of strings and comments are supported.
- `Date`, `Tuple`, and `Variant` data types are added.
- Trailing commas can be omitted.
- A comma can be added to the end of the last element of an array, tuple, or object.

## File Extension

The file extension for _ASON_ document is `*.ason`. Filename example:

`sample.ason`, `package.ason`

## Rust library

Currently, only the Rust implementation of _ASON_ serialization and deserialization library is provided.

Run the command `cargo add ason` in your project directory to add the _ASON_ library to your project.

### The base processor

This library provides two functions:

- `fn parse(s: &str) -> Result<AsonNode, ParseError>` for deserialization;
- `fn write(n: &AsonNode) -> String` for serialization.

**Parser**

Suppose you have the following _ASON_ text, which may come from a file or from the internet:

```json5
{
    id: 123
    name: "foo"
}
```

The following code shows how to parse this text into an _ASON_ object and check the value of each member:

```rust
let text = "..."; // the ASON text above

let node = parse(text).unwrap();

if let AsonNode::Object(obj) = node {
    assert_eq!(
        &obj[0],
        &NameValuePair {
            name: "id".to_string(),
            value: Box::new(AsonNode::Number(NumberLiteral::Int(123)))
        }
    );

    assert_eq!(
        &obj[1],
        &NameValuePair {
            name: "name".to_string(),
            value: Box::new(AsonNode::String_("foo".to_string()))
        }
    );
}
```

**Writer**

Suppose there is an object with two fields, their names and values are:

- name: "foo"
- version: "0.1.0"

The following code demonstrates how to convert this object into _ASON_ text:

```rust
let node = AsonNode::Object(vec![
    NameValuePair {
        name: "name".to_string(),
        value: Box::new(AsonNode::String_("foo".to_string())),
    },
    NameValuePair {
        name: "version".to_string(),
        value: Box::new(AsonNode::String_("0.1.0".to_string())),
    },
]);

let text = write(&node);
println!("{}", text);
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

See the [document](https://hemashushu.github.io/works/xiaoxuan-script-object-notation) for more information.

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
