# XiaoXuan Script Object Notation

_XiaoXuan Script Object Notation_ (_ANON_) is a data format that is easy for humans to read and write. It is similar to _JSON_, but has many improvements and enhancements.

_ANON_ is mainly used as a configuration file for applications, but can also be used for data transmission.

**Features**

- **Compatible with most of the syntax of _JSON_ and _JSON5_.** If you are already familiar with _JSON_, you do not need to learn a new data format.

- **Simple, rigorous and consistent syntax.** The syntax of _ASON_ is close to that of programming languages. For example, numbers can specify data types, multiple formats of comments and strings are supported, key names do not need to be enclosed in double quotes, trailing commas can be omitted, and so on. These features can help users accurately express the data they need, and are also facilitate the modification and maintenance of _ASON_ documents.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [ASON Document Example](#ason-document-example)
- [Comparison with JSON](#comparison-with-json)
- [File Extension](#file-extension)
- [Shared Library and API](#shared-library-and-api)
  - [Rust ASON Shared Library](#rust-ason-shared-library)
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

It is worth mentioning that an _ASON_ document does not necessarily have to be an _ASON_ object, it can also be other _ASON_ values, such as an array, a tuple, or even a single number or string, all of which are valid _ASON_ values.

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

## Shared Library and API

Currently, only the Rust implementation of _ASON_ serialization and deserialization library is provided.

### Rust ASON Shared Library

Run the command `cargo add ason` in your project directory to add the _ASON_ library to your project.

This library provides two functions:

- `fn parse(s: &str) -> Result<AsonNode, ParseError>` for deserialization;
- `fn format(n: &AsonNode) -> String` for serialization.

**Deserialization**

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

**Serialization**

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

let text = format(&node);
println!("{}", text);
```

The output text should be:

```json5
{
    name: "foo"
    version: "0.1.0"
    dependencies: [{
        name: "random"
        version: Option::None
    },{
        name: "regex"
        version: Option::Some("1.0.1")
    }]
}
```

## Documentation

See the [document](https://hemashushu.github.io/works/xiaoxuan-script-object-notation) for more information.

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
