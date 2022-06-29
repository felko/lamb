mod syntax;
mod convert;

pub use syntax::*;
pub use convert::*;

use crate::core;

pub fn anf_convert<'src>(module: core::Module<'src>) -> Module<'src> {
    let converter = ANFConverter::new();
    converter.convert_module(module)
}
