# XiaoXuan Object Notation

The _XiaoXuan Object Notation_ (abbr. _ANON_) is an easy-to-read and write data representation format used primarily as configuration file for application, also for data transfer.

Features:

- Simple, strict and consistent.
- Compatible with most of the syntax of _JSON_ and _JSON5_, familiar with _JSON_ means familiar _ANON_.
- Consistent with _XiaoXuan Lang_ data expression.

- - -

Table of Content:

<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [XiaoXuan Object Notation](#xiaoxuan-object-notation)
  - [Example](#example)
  - [Comparing with _JSON_](#comparing-with-json)
  - [File Extension Name](#file-extension-name)
  - [License](#license)

<!-- /code_chunk_output -->

## Example

The following is an example of _ANON_ with base data types, such as _String_, _Integer number_, _Floating point number_, _Boolean_ and _Date_, as well as _Array (List)_, _Tuple_ and nested _Object_.

```json
{
    string: "hello world"
    int: 123
    int_with_minus: -456
    int_with_data_type_name: 789@u32
    long: 123456
    long_with_data_type_name: 456789@u64

    // integer type names:
    // - i32 (or int)
    // - u32 (or uint)
    // - i64 (or long)
    // - u64 (or ulong)

    float: 3.14
    double: 6.626e-34@f64

    // floating point type names:
    // - f32 (or float)
    // - f64 (or double)

    bool: true
    date: d"2023-03-24"
    time: d"10:15:00"
    data_time: d"2023-03-24 10:15:00"

    // array
    array: [1,2,3,]

    // tuple
    tuple: (1, "foo", true)

    // nested object
    object: {
        id: 123
        name: "leaf"
    }

    // inline comma
    a:"a", b:"b", c:"c",

    // optional tailing commas
    x:"x",
    y:"y",
    z:"z",
}
```

Check out the test documents for more data representations.

## Comparing with _JSON_

_ANON_ is similar in appearance to JSON, but with the following differences:

- Numbers have an explicit datatype.
- _null_ values are not allowed.
- Floating point number do not support `Inf`, `-Inf`, `-0` and `NaN`.
- Single-quoted strings are not supported.
- Comments are supported.
- _Array_ requires all elements to be of the same datatype.
- Added _Date_ datatype.
- Added _Tuple_ datatype.
- Double quotes can be omitted from key names.
- Commas can be ommitted between items.

## File Extension Name

The extension name of the _ANON_ file is `*.anon`.

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).