// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use ason::{parse_from_str, AsonNode, NumberLiteral};

#[test]
fn test_parse_files() {
    assert_eq!(
        parse_from_str(
            r#"
        123
        "#
        )
        .unwrap(),
        AsonNode::Number(NumberLiteral::Int(123))
    );
}
