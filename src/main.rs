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
mod pretty;
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

    let mut file =
        File::open(&args.path).unwrap_or_else(|_| panic!("IO error: Failed to open file"));
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .unwrap_or_else(|_| panic!("IO error: failed to read file"));
    let source = source_storage.alloc(contents);

    let pipeline = PipelineBuilder::new()
        .then(LogDebugPass::new("parsing", true, FailliblePass::from(parse)))
        .then(LogDebugPass::new(
            "typechecking",
            true,
            FailliblePass::new(typecheck),
        ))
        .then(LogDebugPass::new("uncurrying", true, InplacePass::new(uncurry)))
        .build();

    pipeline.run(source);
}
