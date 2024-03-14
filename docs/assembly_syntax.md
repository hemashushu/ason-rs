# XiaoXuan Object Notation Syntax

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

- [Values](#values)
  - [Number](#number)
    - [Integer Number](#integer-number)
    - [Floating Pointer Number](#floating-pointer-number)
    - [Hexadecimal Floating Point Number](#hexadecimal-floating-point-number)
    - [Number data types](#number-data-types)
  - [String](#string)
    - [Long String](#long-string)
    - [Multi-line String](#multi-line-string)
    - [Raw String](#raw-string)
    - [Raw String Variant](#raw-string-variant)
    - [Auto-trimmed String](#auto-trimmed-string)
  - [Byte Data](#byte-data)
  - [Boolean](#boolean)
  - [Date time](#date-time)
  - [array](#array)
  - [Tuple](#tuple)
  - [Object](#object)
    - [Object keys](#object-keys)
    - [Commas](#commas)
    - [Nested](#nested)
    - [Mixed](#mixed)
- [Comments](#comments)
  - [Line Comment](#line-comment)
  - [Block Comment](#block-comment)
  - [Document Comment](#document-comment)
  - [Mix Comments](#mix-comments)

<!-- /code_chunk_output -->

A _XiaoXuan Object Notation_ document is consist of values.

## Values

### Number

_XiaoXuan Object Notation_ supports integer numbers and floating-point numbers.

#### Integer Number

Three types of integer number representation are supported: decimal, hexadecimal and binary. An addition of plus or minus sign is supported. In addition, arbitrary underscores can be added between digits.

Example of integer numbers:

- `211`, `223_211`, `-2027`
- `0x1113`, `0x1719_abcd`, `-0xaabb`
- `0b1100`, `0b1010_0001`, `-0b1100`

#### Floating Pointer Number

Floating point numbers can be represented in decimal.

Example of floating point numbers:

`3.14`, `2.998e8`, `123.`,  `6.626e-34`, `-1.7588e11`, `-1.7588e+11`

Example of invalid numbers:

`0x3.14`, `0b11.10`, `.123`, `123e`

#### Hexadecimal Floating Point Number

Floating point numbers can be represented in hexadecimal also, the format of [hexadecimal floating pointer literals](https://en.cppreference.com/w/c/language/floating_constant) is `0xh.hhhp±d`, which is the same as it is in C/C++.

e.g., `0x1.23p4` means `(1x16^0 + 2x16^-1 + 3x16^-2) x 2^4 = (1.13671875 x 16)`, its value is `18.1875`.

Example of hexadecimal floating point number:

`0x1.921fb6p1`, `0x1.5bf0a8b145769p+1`, `0x1.23p-4`

_XiaoXuan Object Notation_ does not support `-0.0`, `+inf`, `-inf`, `nan`.

#### Number data types

// default integer numbers: int(i32)
// default floating-point numbers: float(f32)
//
// data type names:
// - int, uint          (i32/u32)
// - long, ulong        (i64/u64)
// - byte, ubyte        (i8/u8)
// - short, ushort      (i16/u16)
// - float, double      (f32/f64)

// 123@long     // with data type suffix

### Metric prefix

https://en.wikipedia.org/wiki/Metric_prefix
https://en.wikipedia.org/wiki/Binary_prefix
https://en.wikipedia.org/wiki/Unit_prefix

E,P,T,G,M,K     <-- ulong
m,u,n,p,f,a     <-- float

Binary prefixes
Zi,Ei,Pi,Ti,Gi,Mi,Ki

// 123u  //  fractional

only available for decimal numbers and the data type must be "unsign long" or "float".

### String

A string is a sequence of characters surrounded by a pair of quotes. Strings support any Unicode character (including emojis), and also support the following escape characters:

- `\t`: Horizontal tabulation
- `\r`: Carriage return (CR)
- `\n`: New line character (line feed, LF)
- `\0`: Null character
- `\u{...}`, Unicode code point, e.g. `\u{2d}`, `\u{6587}`
- `\"`: Doube quote
- `\\`: Escape character itself

The following escape characters are used in other language, but they are not supported in the _XiaoXuan Object Notation_:

- `\v`: Vertical tabulation
- `\f`: Page breaking control character
- `\x..`: ASCII code

Example of strings:

`"abc文字😊"`, `"\u{2d}\u{6587}\0"`, `"foo\nbar"`

> Strings are encoded using UTF-8.

#### Long String

Sometimes the contents of a string may be too long to fit on one line. _XiaoXuan Object Notation_ supports spliting long string into multiple lines by inserting a backslash and a line break in the middle of the string. e.g.

```text
"Hel\
    lo, \
    World!"
```

The above string is equivalent to `"Hello, World!"`. Note that the leading whitespace is automatically trimmed from each line.

#### Multi-line String

String also support multiple lines, simply insert line breaks into the string as usual, e.g.

```text
"Hello,
    World! I'm XiaoXuan
    Core Assembly."
```

The above string is equivalent to `"Hello,\n    World! I'm XiaoXuan\n    Core Assembly."`.

Note that leading whitespace is not automatically removed with this format.

#### Raw String

Adding a letter `r`` before the first quote to indicate a string is a _raw string_.

The raw strings do not escape any characters, all content will be output as is, e.g.

`r"Hello\nWorld!"` is equivalent to `"Hello\\nWorld!"`

#### Raw String Variant

Since raw strings don't support escaping characters, if you need to output the "double quote", you can use the variant of raw strings, i.e. use `r#"..."#` to enclose the string, e.g.

`r#"One "two" three"#` is equivalent to `"One \"two\" three"`.

#### Auto-trimmed String

Auto-trimmed strings are used to write long text, it's similar to the raw string, where characters are not escaped, but the leading whitespace on each line is automatically trimmed based on the number of leading spaces in the first line.

Auto-trimmed string starts with `r|"`, and then the content start in a new line, and ends with `"|` which is in **a separate line** (leading whitespaces are staill allowed), for example:

```text
r|"
    id:
      123
    name:
      foo
"|
```

In the above example, since there are 4 leading space characters in the first line, so each line truncates max 4 leading spaces. The string is equivalent to `"id:\n  123\nname:  foo"`. Note that the last new-line symbol `\n` of the string is removed automatically.

### Byte Data

Byte data is used to represent a piece of binary data in memory or on the storage, starting with the letter `h` followed by a pair of double quotes. Inside the quotes is the content of the data, which uses two letters `[0-9a-zA-Z]` to represent a byte, and the characters `[ -:\t\r\n]` are ignored in the content. For example, the following represents the same 4-byte data:

```text
h"0011aabb",
h"0011AABB",
h"00 11 aa bb",
h"00-11-aa-bb",
h"00:11:aa:bb",
h"00 11
  aa bb"
```

### Boolean

TODO

`true`, `false`

### Date time

TODO

`d"2023-03-24"`
`d"10:15:00"`
`d"2023-03-24 10:15:00"`

### array

TODO

`[1,2,3]`
or
`[1,2,3,]`

Array requires all elements to be of the same datatype.

multiline

```json5

[
    1
    2
    3
]
```

or

```json5
[
    1,
    2,
    3,
]
```

### Tuple

`(1, "foo", true)`
or
`(1, "foo", true,)`

Tuple does NOT require all elements to be of the same datatype.

multiline

```json5
(
1
"foo"
true
)
```

or

```json5
(
1,
"foo",
true,
)
```

### Object

name-value pair

#### Object names

TODO::

```json5
{
    name1: value1
    name2: value2
}
```

Object names are the names of members.

Object names consist of characters `[a-zA-Z0-9_]`, and cannot start with a number.

Example of valid object names:

`user`, `id`, `name`, `address`, `main_number`

#### Commas

```json5
{
    key1: value1,
    key2: value2,
}
```

```json5
{
    key1: value1, key2: value2,
}
```

#### Nested

```json5
{
    key1: value1
    key2: {
        key21: value21
        key22: value22
    }
}
```

#### Mixed

```json5
{
    key1: value1
    key2: [value21, value22]
}
```

```json5
{
    key1: value1
    key2: [
        {key21: value21}
        {key22: value22}
    ]
}
```

## Comments

_XiaoXuan Object Notation_ supports 3 styles of comments: line comments, block comments and document comments.

### Line Comment

Line comments start with symbol `//` and continue until the end of the line, e.g.

```clojure
(module
    // this is a comment
    (function $test   // this is a comment also
    )
)
```

### Block Comment

Block comments start with the symbol `/*` and end with the symbol `*/`, and nested block comments are supported, for example:

```clojure
(module
    (function $test
        /* this is a block comment */
        /* level one /* level two */*/
    )
)
```

### Document Comment

Document comments are used to write long text related to modules, structures, functions and so on.

Document comments starts with `"""`, and then the content start in a new line, and ends with `"""` which is occured a separate line, for example:

```text
"""
NAME
    ls - list directory contents

DESCRIPTION
    List information about the FILEs (the current directory by default).
    Sort entries alphabetically if none of -cftuvSUX nor --sort is
    specified.
"""
```

leading spaces are allowed, for example:

```text
(function $add (param $left i32) (param $right i32) (result i32)
    """
    Add two integer numbers.
    """

    (code
        ...
    )
)

```

### Mix Comments

Line comment symbol `//` and document comment symbol `"""` within valid block comments are ignored, e.g.

```clojure
(module
    /* block comment // still block comment */
)
```

```clojure
(module
    /*
    block comment part 1
        // /*
        block comment part 2
        */
    block comment part 3
    */
)
```

```clojure
(module
    /* block comment
        """
        still block comment
        """
    */
)
```

Practically any type of comment symbol is ignored in other types of valid comments, i.e.,:

- Block comment symbol `/*` and document comment symbol `"""` within valid line comments
- Block comment symbol `/*` and line comment symbol `//` within valid document comments
