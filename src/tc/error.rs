use crate::pretty::*;
use crate::tc::Type;

#[derive(Debug)]
pub enum TypeError<'src> {
    ScopeError(&'src str),
    OccursCheckFailure(u32, Type<'src>),
    UnificationFailure(Type<'src>, Type<'src>),
    AlreadyDefined(&'src str),
    AmbiguousType(Vec<String>, Type<'src>),
}

impl<'a, 'src> PrettyPrec<'a> for TypeError<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            TypeError::ScopeError(name) => allocator
                .text("Scope error:")
                .append(allocator.space())
                .append(allocator.text("variable"))
                .append(allocator.space())
                .append(allocator.text(name.to_owned()).single_quotes())
                .append(allocator.space())
                .append("is not in scope"),
            TypeError::OccursCheckFailure(var, type_) => allocator
                .text("Occurs check failed:")
                .append(allocator.space())
                .append(allocator.text(var.to_string()).single_quotes())
                .append(allocator.space())
                .append("~!")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator)),
            TypeError::UnificationFailure(lhs, rhs) => allocator
                .text("Unification failed:")
                .append(allocator.space())
                .append(lhs.pretty_prec(0, allocator))
                .append(allocator.space())
                .append("~!")
                .append(allocator.space())
                .append(rhs.pretty_prec(0, allocator)),
            TypeError::AlreadyDefined(name) => allocator
                .text("Name")
                .append(allocator.space())
                .append(allocator.text(name.to_owned()).single_quotes())
                .append(allocator.space())
                .append("is alread defined"),
            TypeError::AmbiguousType(vars, type_) => allocator
                .text("Ambiguous type:")
                .append(allocator.space())
                .append(
                    allocator
                        .text("could not deduce variables")
                        .append(allocator.space())
                        .append(allocator.intersperse(
                            vars.iter().map(|var| allocator.text(var.clone())),
                            allocator.text(", "),
                        )),
                )
                .append(allocator.space())
                .append(allocator.text("in type"))
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator)),
        }
    }
}
