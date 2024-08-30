// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.
/*
use std::process;

use ason::{parse_from, print_to};

fn main() {
    let mut args = std::env::args();
    // args 0 = program executable file itself
    // args 1 = source file, "-" for STDIN
    //
    // run with Cargo:
    // `$ cargo run -- <filename>`
    //
    // e.g.
    // `$ cargo run -- examples/01-primitive.ason`
    // `$ cargo run -- -`
    //
    // `$ ason examples/01-primitive.ason`
    // `$ echo '{id: 123}' | ason -`

    if args.len() != 2 {
        // https://doc.rust-lang.org/cargo/reference/environment-variables.html
        eprintln!("ASON Utility {}", env!("CARGO_PKG_VERSION"));
        eprintln!();
        eprintln!("Usage:");
        eprintln!("    ason <filename>");
        eprintln!("");
        eprintln!("    Pass \"-\" to <filename> for STDIN");
        eprintln!("");
        eprintln!("Example:");
        eprintln!("    ason examples/01-primitive.ason");
        eprintln!("    echo '{{id: 123}}' | ason -");
        eprintln!();
        eprintln!("Source:");
        eprintln!("    https://github.com/hemashushu/ason-rs");
        eprintln!();
        eprintln!("Document:");
        eprintln!("    https://hemashushu.github.io/works/ason");
        process::exit(1);
    }

    let source_file = args.nth(1).unwrap();

    let source = match std::fs::read_to_string(&source_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "Can not read the specified file: {}\nReason: {}",
                &source_file, e
            );
            process::exit(1);
        }
    };

    let node = match parse_from(&source) {
        Ok(n) => n,
        Err(e) => {
            eprintln!(
                "Fail to parse the specified file: {}\nMessage: {}",
                &source_file, e
            );
            process::exit(1);
        }
    };

    let text = print_to(&node);
    println!("{}", text);
}

 */

fn main() {
    todo!()
}