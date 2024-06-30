// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

/*
use std::process;
use ason::{parse, write};

fn main() {
    let mut args = std::env::args();

    // 0 = program itself
    // 1 = source file
    if args.len() != 2 {
        // https://doc.rust-lang.org/cargo/reference/environment-variables.html
        eprintln!("XiaoXuan Script Object Notation Parser {}", env!("CARGO_PKG_VERSION"));
        eprintln!();
        eprintln!("Usage:");
        eprintln!("    ason <source_file>");
        eprintln!();
        eprintln!("Reference: https://github.com/hemashushu/xiaoxuan-script-object-notation");
        process::exit(1);
    }

    let source_file = args.nth(1).unwrap();

    let source = match std::fs::read_to_string(&source_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "Can not read the specified file: {}\nReason: {}",
                &source_file,
                e
            );
            process::exit(1);
        }
    };

    let node = match parse(&source) {
        Ok(n) => n,
        Err(e) => {
            eprintln!(
                "Can not parse the specified file: {}\nReason: {}",
                &source_file, e.message
            );
            process::exit(1);
        }
    };

    let text = write(&node);
    println!("{}", text);
}
*/

fn main() {
    todo!()
}