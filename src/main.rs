#[macro_use] extern crate lalrpop_util;

use clap::Parser;
use bumpalo::Bump;

pub mod surface;

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
        Ok(expr) => println!("{expr:?}"),
        Err(error) => println!("{error:?}"),
    }
}
