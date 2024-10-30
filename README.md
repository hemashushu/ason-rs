# ASON

_ASON_ is a data format evolved from JSON, featuring **strong data types** and support for **variant types**. It offers excellent readability and maintainability. ASON is well-suited for configuration files, data transfer, and data storage.

<!-- (_ASON_ stands for _XiaoXuan Script Object Notation_) -->

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [1 Features](#1-features)
- [2 Example](#2-example)
- [3 Comparison](#3-comparison)
  - [3.1 Compared to JSON](#31-compared-to-json)
  - [3.2 Compared to YAML and TOML](#32-compared-to-yaml-and-toml)
- [4 Filename Extension](#4-filename-extension)

<!-- /code_chunk_output -->

## 1 Features

- **JSON Compatibility:** ASON integrates with base JSON and JSON5 syntax, making it easy for JSON users to transition to ASON.

- **Simple and Consistent Syntax:** ASON's syntax closely resembles JavaScript and Rust, featuring support for comments, omitting double quotes for structure (object) field names, and allowing trailing commas in the last array element. These features enhance familiarity and writing fluency.

- **Strong Data Typing:** ASON numbers can be explicitly typed (e.g., `u8`, `i32`, `f32`, `f64`), and integers can be represented in hexdecimal and binary formats. Additionally, new data types such as `DateTime`, `Tuple`, `ByteData`, `Char` are introduced, enabling more precise and rigorous data representation.

- **Native Variant Data Type Support, Eliminating the Null Value:** ASON natively supports variant data types (also known as _algebraic types_, similar to _Enums_ in Rust). This enables seamless serialization of complex data structures from high-level programming languages. Importantly, it eliminates the error-prone `null` value.

## 2 Example

The following is an example of ASON text:

```json5
{
    string: "hello world"
    raw_string: r"[a-z]+\d+"
    integer_number: 123
    floating_point_number: 3.14
    number_with_explicit_type: 255_u8
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
    variant_with_value: Option::Some(123)
    tuple_style_variant: Color::RGB(255, 127, 63)
    object_style_variant: Shape::Rect{
        width: 200
        height: 100
    }
}
```

## 3 Comparison

### 3.1 Compared to JSON

ASON is an improvement over JSON. Overall, ASON's syntax is simpler, more consistent, and more expressive than JSON. Specifically:

- Trailing commas can be omitted.
- Double quotes for object keys are omitted.
- Numeric data types are added.
- Hexadecimal and binary representations of integers are added.
- Hexadecimal representation of floating-point numbers are added.
- Support for "long strings", "raw strings", and "auto-trimmed strings" is added.
- Support for "line comments", "block comments" is added.
- The `Variant` data type is added, and the `null` value is removed.
- New data types such as `Char`, `DateTime`, `Tuple`, `ByteData` are added.
- Improvement: Strings are consistently represented using double quotes.
- Improvement: `List` requires all elements to be of the same data type.
- Improvement: A trailing comma is allowed at the end of the last element of `List`, `Tuple` and `Object`.

### 3.2 Compared to YAML and TOML

All three formats are simple enough to express data well when the dataset is small. However, when dealing with larger datasets, the results can vary.

YAML uses indentation to represent hierarchy, so the number of space characters in the prefix needs to be carefully controlled, and it is easy to make mistakes when retreating multiple layers. In addition, its [specification](https://yaml.org/spec/) is quite complex.

TOML is not good at expressing hierarchy, there are often redundant key names in the text, and the [object list](https://toml.io/en/v1.0.0#array-of-tables) are not as clear as other formats. ASON, on the other hand, has good consistency regardless of the size of the data.

In contrast, ASON maintains its simplicity and consistency regardless dataset size.

## 4 Filename Extension

The extension name for ASON file is `*.ason`, for example:

`sample.ason`, `package.ason`

TODO