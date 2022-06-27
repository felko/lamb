pub mod check;
pub mod prim;
pub mod syntax;
pub mod error;

pub use check::*;
pub use syntax::*;
pub use error::*;

use crate::core;
use crate::surface;

pub fn typecheck(module: surface::Module) -> Result<core::Module, TypeError> {
    let mut tc = Typechecker::new();
    tc.run(module)
}
