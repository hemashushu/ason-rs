// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use ason::{format, parse};

use pretty_assertions::assert_eq;

fn read_file(file_path: &str) -> String {
    // note:
    //
    // in the VSCode editor `Debug` environment, the `current_dir()` returns
    // the project's root folder.
    // while in the `$ cargo test` (and VSCode editor `Run Test`) environment,
    // the `current_dir()` returns the current crate path if there are
    // multiple crates in the current project.

    let mut pwd = std::env::current_dir().unwrap();
    pwd.push("tests");
    pwd.push("resources");
    pwd.push(file_path);

    let source_file_path = pwd.to_str().unwrap();
    std::fs::read_to_string(source_file_path).unwrap()
}

#[test]
fn test_file_01() {
    let s = read_file("01-object.ason");
    let n = parse(&s).unwrap();
    let t = format(&n);

    assert_eq!(
        &t,
        r#"{
    integer: 123
    integer_negative: -123
    byte: 11@byte
    ubyte: 13@ubyte
    short: 17@short
    ushort: 19@ushort
    int: 23
    uint: 29@uint
    long: 31@long
    ulong: 37@ulong
    floating_point: 3.14
    float: 3.14
    double: 6.626@double
    float_with_exp: 602200000000000000000000.0
    float_with_exp_negative: 0.000000000066738
    suffix_K: 1000
    suffix_M: 1000000
    suffix_G: 1000000000
    suffix_T: 1000000000000@long
    suffix_P: 1000000000000000@long
    suffix_E: 1000000000000000000@long
    suffix_m: 0.001
    suffix_u: 0.000001
    suffix_n: 0.000000001
    suffix_p: 0.000000000001
    suffix_f: 0.000000000000001
    suffix_a: 0.0000000000000000009999999
    suffix_Ki: 1024
    suffix_Mi: 1048576
    suffix_Gi: 1073741824
    suffix_Ti: 1099511627776@long
    suffix_Pi: 1125899906842624@long
    suffix_Ei: 1152921504606846976@long
    both_metric_suffix_and_type_long: 1000000000@long
    both_metric_suffix_and_type_double: 0.000001@double
    hex_integer: 48879
    hex_integer_negative: -57005
    hex_floating_point: 3.1415927
    hex_byte: 127@byte
    hex_ubyte: 255@ubyte
    hex_short: 32767@short
    hex_ushort: 65535@ushort
    hex_int: 2147483647
    hex_uint: 4294967295@uint
    hex_long: 9223372036854775807@long
    hex_ulong: 18446744073709551615@ulong
    hex_float: 3.1415927
    hex_double: 2.718281828459045@double
    bin_integer: 9
    bin_integer_negative: 9
    bin_byte: 127@byte
    bin_ubyte: 255@ubyte
    bin_short: 32767@short
    bin_ushort: 65535@ushort
    bin_int: 2147483647
    bin_uint: 4294967295@uint
    bin_long: 140737488355327@long
    bin_ulong: 281474976710655@ulong
    bool_true: true
    bool_false: false
    data_time: d"2023-02-23T10:23:45+00:00"
    data_time_tz: d"2023-02-23T10:23:45+08:00"
    char: 'c'
    char_unicode: '文'
    char_escape: '\n'
    char_escape_zero: '\0'
    char_escape_unicode: '河'
    string: "hello world"
    string_unicode: "中文🍀emoji👋🏻"
    multiline_string: "one
        two
        three"
    multiline_string_with_new_line_escape: "onetwothree"
    string_escape_chars: "double quote:\"
        single quote:'
        slash:\\
        tab:\t
        line feed:
"
    string_escape_unicode: "河马&萱"
    raw_string: "hello"
    raw_string_variant: "hello \"programming\" world"
    auto_trimmed_string: "heading 1
  heading 2
    heading 3"
    variant_none: Option::None
    variant_some: Option::Some(123)
    variant_object: Option::Some({
        id: 123
    })
    new_line: "value1"
    new_line_variant: "value2"
    space: "value3"
    line_comment: 101
    line_comment_in_tail: 103
    block_comment: 107
    multiline_block_comment: 109
    document_comment: 113
    inline_comma_1: 211
    inline_comma_2: 223
    inline_comma_3: 227
    tail_comma_1: 229
    tail_comma_2: 233
    tail_comma_3: 239
}"#
    )
}

#[test]
fn test_file_02() {
    let s = read_file("02-array.ason");
    let n = parse(&s).unwrap();
    let t = format(&n);

    assert_eq!(
        t,
        r#"{
    number_array: [1,2,3]
    string_array: ["one","two","three"]
    array_with_trailing_comma: [1,2,3,4]
    mulitline_array: [1,2,3]
    mulitline_array_with_commas: [1,2,3]
    mulitline_array_with_trailing_comma: [1,2,3]
}"#
    );
}

#[test]
fn test_file_03() {
    let s = read_file("03-tuple.ason");
    let n = parse(&s).unwrap();
    let t = format(&n);

    assert_eq!(
        t,
        r#"{
    tuple: (1,"foo",true)
    tuple_with_trailing_comma: (1,"foo",true)
    mulitline_tuple: (1,"foo",true)
    mulitline_tuple_with_commas: (1,"foo",true)
    mulitline_tuple_with_trailing_comma: (1,"foo",true)
}"#
    );
}

#[test]
fn test_file_04() {
    let s = read_file("04-nested.ason");
    let n = parse(&s).unwrap();
    let t = format(&n);

    assert_eq!(
        t,
        r#"{
    id: 123
    name: "hello"
    orders: [(1,"foo",true),(2,"bar",false)]
    group: {
        active: true
        permissions: [{
            number: 11
            title: "read"
        },{
            number: 13
            title: "write"
        }]
    }
}"#
    );
}
