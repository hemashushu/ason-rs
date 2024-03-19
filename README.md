# XiaoXuan Script Object Notation

_XiaoXuan Script Object Notation_ (_ASON_) is a data format designed to be easy for humans to read and write. it is similar to _JSON_, but with some key differences. _ASON_ is primarily used as a configuration file for applications, but it can also be used as a data transfer format.

**Features**

- Compatible with most of the syntax of _JSON_ and _JSON5_.
  If you are already familiar with _JSON_, you can read and write _ASON_ without having to learning anything new.
- Simple, rigorous and consistent.
  _ASON_ improves and ehances _JSON_. For example, _ASON_ numbers have data types, support comments, key names do not require double quotes, `Boolean`, `Date` and `Tuple` types are added. In addition, the `null` value that often causes ambiguity is removed.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Example](#example)
- [Documentation](#documentation)
- [File Extension](#file-extension)
- [License](#license)

<!-- /code_chunk_output -->

## Example

```json
{
    string: "hello world"
    raw_string: r"[a-z]+"
    number: 123
    number_with_data_type_name: 123_456_789@long
    float: 3.14
    double: 6.626e-34@double
    bool: true
    date: d"2023-03-24 12:30:00+08:00"
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
