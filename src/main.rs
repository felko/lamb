#![feature(
    never_type,
    box_syntax,
    box_patterns,
    let_chains,
    if_let_guard,
    trait_alias,
    iterator_try_collect
)]

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate im;

use std::{fs::File, io::Read};

use bumpalo::Bump;
use clap::Parser;

mod anf;
mod common;
mod core;
mod env;
mod lifted;
mod pipeline;
mod pretty;
mod surface;
mod tc;

use crate::anf::{anf_convert, closure_convert};
use crate::core::uncurry;
use crate::lifted::lift;
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
        .then(LogPrettyPass::new(
            "parsing",
            true,
            FailliblePass::new(parse),
        ))
        .then(LogPrettyPass::new(
            "typechecking",
            true,
            FailliblePass::new(typecheck),
        ))
        .then(LogPrettyPass::new(
            "uncurrying",
            true,
            InplacePass::new(uncurry),
        ))
        .then(LogPrettyPass::new(
            "anf",
            true,
            SimplePass::new(anf_convert),
        ))
        .then(LogPrettyPass::new(
            "closure conversion",
            true,
            InplacePass::new(closure_convert),
        ))
        .then(LogPrettyPass::new(
            "lifting",
            true,
            SimplePass::new(lift),
        ))
        .build();

    pipeline.run(source);
}
