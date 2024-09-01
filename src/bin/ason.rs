// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::{io::Read, process};

use ason::{parse_from_str, print_to_string};

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
        eprintln!("USAGE:");
        eprintln!("    ason <filename>");
        eprintln!();
        eprintln!("    Pass \"-\" to <filename> for STDIN");
        eprintln!();
        eprintln!("EXAMPLES:");
        eprintln!("    ason examples/01-primitive.ason");
        eprintln!("    echo '{{id: 123}}' | ason -");
        eprintln!();
        eprintln!("SOURCE:");
        eprintln!("    https://github.com/hemashushu/ason-rs");
        eprintln!();
        eprintln!("DOCUMENT:");
        eprintln!("    https://hemashushu.github.io/works/ason");
        process::exit(1);
    }

    let filepath = args.nth(1).unwrap();

    let text = if filepath == "-" {
        let mut buf = String::new();
        match std::io::stdin().lock().read_to_string(&mut buf) {
            Ok(_) => buf,
            Err(e) => {
                eprintln!("Fail to read the content from STDIN, message: {}", e);
                process::exit(1);
            }
        }
    } else {
        match std::fs::read_to_string(&filepath) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "Fail to read the specified file: {}, message: {}",
                    &filepath, e
                );
                process::exit(1);
            }
        }
    };

    let node = match parse_from_str(&text) {
        Ok(n) => n,
        Err(e) => {
            let msg = if filepath == "-" {
                format!("Fail to parse the input text, message: {}", e)
            } else {
                format!(
                    "Fail to parse the specified file: {}, message: {}",
                    &filepath, e
                )
            };
            eprintln!("{}", msg);
            process::exit(1);
        }
    };

    let text = print_to_string(&node);

    println!("{}", text);
}
