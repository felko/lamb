pub mod check;
pub mod prim;
pub mod syntax;

pub use check::*;
pub use syntax::*;

use crate::core;
use crate::surface;

pub fn typecheck(module: surface::Module) -> Result<core::Module, TypeError> {
    let mut tc = Typechecker::new();
    tc.run(module)
}
