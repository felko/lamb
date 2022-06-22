#![feature(never_type, box_syntax, box_patterns, let_chains)]

#[macro_use] extern crate lalrpop_util;

use clap::Parser;
use bumpalo::Bump;

pub mod surface;
pub mod core;

use surface::parse_from_file;

/// A compiler for a simple functional programming language
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The path of the file to compile
    #[clap(value_parser)]
    path: String,
}

fn main() {
    let args = Args::parse();
    let source_storage = Bump::new();
    match parse_from_file(&args.path, &source_storage) {
        Ok(expr) => {
            println!("Parser: {expr:?}");
            let mut tc = core::Typechecker::new();
            match tc.infer_expr(expr) {
                Ok((expr_elab, scheme)) => {
                    println!("Elab: {expr_elab}");
                    println!("Type: {scheme}");
                },
                Err(error) => println!("{error:?}")
            }
        },
        Err(error) => println!("{error:?}"),
    }
}
