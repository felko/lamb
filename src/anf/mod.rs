pub mod syntax;
pub mod convert;
pub mod closure_conversion;

pub use syntax::*;
pub use convert::*;
pub use closure_conversion::*;

use crate::core;

pub fn anf_convert(module: core::Module) -> Module {
    let converter = ANFConverter::new();
    converter.convert_module(module)
}

pub fn closure_convert(module: &mut Module) {
    let converter = ClosureConverter::new();
    converter.convert_module(module)
}
