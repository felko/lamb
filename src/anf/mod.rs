pub mod closure_conversion;
pub mod convert;
pub mod syntax;

pub use closure_conversion::*;
pub use convert::*;
pub use syntax::*;

use crate::core;

pub fn anf_convert(module: core::Module) -> Module {
    let converter = ANFConverter::new();
    converter.convert_module(module)
}

pub fn closure_convert(module: &mut Module) {
    let converter = ClosureConverter::new();
    converter.convert_module(module)
}
