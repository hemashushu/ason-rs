# XiaoXuan Script Object Notation

_XiaoXuan Script Object Notation_ (_ASON_) is a data format designed to be easy for humans to read and write. It is similar to _JSON_, but with many improvements and enhancements. _ASON_ is mainly used as a configuration file for applications, but can also be used for data transmission.

**Features**

- **Compatible with most of the syntax of _JSON_ and _JSON5_.** If you are already familiar with _JSON_, you may not need to learn anything new to read and write _ASON_.
- **Simple, rigorous and consistent syntax.** The syntax style of _ASON_ is close to that of programming languages. For example, numbers have data types, multiple types comments and strings are supported, key names do not require double quotes, and so on. These features make it easy for users to write accurate _ASON_ documents, and are also beneficial for modification and maintenance.

**Table of Content**

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [ASON document example](#ason-document-example)
- [File Extension](#file-extension)
- [API](#api)
  - [Serialization](#serialization)
  - [Deserialization](#deserialization)
- [Documentation](#documentation)
- [Source code](#source-code)
- [License](#license)

<!-- /code_chunk_output -->

## ASON document example

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

## File Extension

The file extension for _ASON_ is `*.ason`.

## API

Run the following `Cargo` command in your project directory first to add this library to your project:

```bash
cargo add ason
```

### Serialization

`fn format(n: &AsonNode) -> String`

A typical _ASON_ document is an _ASON_ object, which consists of one or more "name-value" pairs. The following demostrates an _ASON_ object consisting of the "name" and "version" fields, and then converts it to text:

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
    NameValuePair {
        name: "dependencies".to_string(),
        value: Box::new(AsonNode::Array(vec![
            AsonNode::Object(vec![
                NameValuePair {
                    name: "name".to_string(),
                    value: Box::new(AsonNode::String_("random".to_string())),
                },
                NameValuePair {
                    name: "version".to_string(),
                    value: Box::new(AsonNode::Variant(VariantItem {
                        name: "Option::None".to_string(),
                        value: None,
                    })),
                },
            ]),
            AsonNode::Object(vec![
                NameValuePair {
                    name: "name".to_string(),
                    value: Box::new(AsonNode::String_("regex".to_string())),
                },
                NameValuePair {
                    name: "version".to_string(),
                    value: Box::new(AsonNode::Variant(VariantItem {
                        name: "Option::Some".to_string(),
                        value: Some(Box::new(AsonNode::String_("1.0.1".to_string()))),
                    })),
                },
            ]),
        ])),
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

### Deserialization

`fn parse(s: &str) -> Result<AsonNode, ParseError>`

Suppose you have an _ASON_ text as below, which may come from a file or from the internet:

```json5
{
    id: 123
    name: "foo"
    orders: [11, 13]
}
```

Now parse it into an _ASON_ object and check each of its members:

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

    assert_eq!(
        &obj[2],
        &NameValuePair {
            name: "orders".to_string(),
            value: Box::new(AsonNode::Array(vec![
                AsonNode::Number(NumberLiteral::Int(11)),
                AsonNode::Number(NumberLiteral::Int(13))
            ]))
        }
    );
}
```

## Documentation

See the [document](https://hemashushu.github.io/works/xiaoxuan-script-object-notation) for more information.

## Source code

- [Github](https://github.com/hemashushu/xiaoxuan-script-object-notation)

## License

Check out [LICENSE](./LICENSE) and [LICENSE.additional](./LICENSE.additional).
