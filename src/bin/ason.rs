// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::{io::Read, process};

use ason::{parse_from_str, print_to_string};

fn main() {
    let mut args = std::env::args();

    // run with Cargo
    // --------------
    //
    // `$ cargo run -- <filename>`
    //
    // e.g.
    // `$ cargo run -- examples/01-primitive.ason`
    // `$ cargo run -- -v examples/06-error.ason`
    //
    // run standalone
    // --------------
    //
    // `$ ason examples/01-primitive.ason`
    // `$ echo '{id: 123}' | ason -`

    if args.len() < 2 || args.len() > 3 {
        // https://doc.rust-lang.org/cargo/reference/environment-variables.html
        eprintln!("ASON Utility {}", env!("CARGO_PKG_VERSION"));
        eprintln!();
        eprintln!("Usage:");
        eprintln!("    ason [OPTION] <filename>");
        eprintln!();
        eprintln!("Options:");
        eprintln!("    -v, --verify     verify document only");
        eprintln!();
        eprintln!("    Pass \"-\" to <filename> for STDIN");
        eprintln!();
        eprintln!("Examples:");
        eprintln!("    - ason examples/01-primitive.ason");
        eprintln!("    - ason -v examples/06-error.ason");
        eprintln!("    - echo '{{id: 123}}' | ason -");
        eprintln!();
        eprintln!("Source:");
        eprintln!("    https://github.com/hemashushu/ason-rs");
        eprintln!();
        eprintln!("Document:");
        eprintln!("    https://hemashushu.github.io/works/ason");
        process::exit(1);
    }

    // args 0 = program executable file itself
    let (is_verify, filepath) = if args.len() == 2 {
        (false, args.nth(1).unwrap())
    } else {
        (true, args.nth(2).unwrap())
    };

    let source = if filepath == "-" {
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

    let node = match parse_from_str(&source) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("{}", e.with_source(&source));
            process::exit(1);
        }
    };

    if is_verify {
        println!("Document validation passed.")
    } else {
        println!("{}", print_to_string(&node));
    }
}
