#![feature(never_type, box_syntax, box_patterns, let_chains, trait_alias)]

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use std::{fs::File, io::Read};

use bumpalo::Bump;
use clap::Parser;

mod pipeline;

mod core;
mod env;
mod surface;
mod tc;

use crate::core::uncurry;
use crate::pipeline::*;
use crate::surface::parse;
use crate::tc::typecheck;

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

    let pipeline = PipelineBuilder::new()
        .then_try("parsing", parse, false)
        .then_try("typechecking", typecheck, true)
        .then_mut("uncurrying", uncurry, true)
        .build();

    let mut file =
        File::open(&args.path).unwrap_or_else(|_| panic!("IO error: Failed to open file"));
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .unwrap_or_else(|_| panic!("IO error: failed to read file"));
    let source = source_storage.alloc(contents);

    match pipeline.run(source) {
        Ok(_) => {}
        Err(err) => {
            println!("Error: {err}");
        }
    };
}
