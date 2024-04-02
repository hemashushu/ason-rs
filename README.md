# XiaoXuan Script Object Notation

_XiaoXuan Script Object Notation_ (_ASON_) is a data format designed to be easy for humans to read and write. It is similar to _JSON_, but with many improvements and enhancements. _ASON_ is mainly used as a configuration file for applications, but can also be used for data transmission.

**Features**

- **Compatible with most of the syntax of _JSON_ and _JSON5_.** If you are already familiar with _JSON_, you may not need to learn anything new to read and write _ASON_.
- **Simple, rigorous and consistent syntax.** The syntax style of _ASON_ is close to that of programming languages. For example, numbers have data types, multiple types comments and strings are supported, key names do not require double quotes, and so on. These features make it easy for users to write accurate _ASON_ documents, and are also beneficial for modification and maintenance.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Example](#example)
- [Documentation](#documentation)
- [File Extension](#file-extension)
- [License](#license)

<!-- /code_chunk_output -->

## Example

```json5
{
    string: "hello world"
    raw_string: r"[a-z]+"
    number: 123
    number_with_data_type_name: 123_456_789@long
    float: 3.14
    double: 6.626e-34@double
    bool: true
    date: d"2023-03-24 12:30:00+08:00"
    variant: Option::None
    array: [1,2,3,]
    tuple: (1, "foo", true)
    object: {
        id: 123
        name: "leaf"
    }
}
```

## Documentation

See the [document](https://hemashushu.github.io/works/xiaoxuan-script-object-notation) for more information.

## File Extension

The file extension for _ASON_ is `*.ason`.

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
