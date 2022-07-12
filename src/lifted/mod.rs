pub mod syntax;
pub mod lift;

pub use syntax::*;
pub use lift::*;

pub fn lift(module: crate::anf::Module) -> Module {
    let mut lifter = Lifter::new();
    lifter.lift_module(module)
}

