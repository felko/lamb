use crate::pretty::*;
use crate::tc::Type;

#[derive(Debug)]
pub enum TypeError<'src> {
    ScopeError(&'src str),
    OccursCheckFailure(u32, Type<'src>),
    UnificationFailure(Type<'src>, Type<'src>),
    AlreadyDefined(&'src str),
    AmbiguousType(Vec<String>, Type<'src>),
    NotATuple(Type<'src>),
    ProjectionOutOfBounds(Vec<Type<'src>>, u8),
    AmbiguousTuple,
}

impl<'a, 'src> PrettyPrec<'a> for TypeError<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        use TypeError::*;
        match self {
            ScopeError(name) => allocator
                .text("Scope error:")
                .append(allocator.space())
                .append(allocator.text("variable"))
                .append(allocator.space())
                .append(allocator.text(name.to_owned()).single_quotes())
                .append(allocator.space())
                .append("is not in scope"),
            OccursCheckFailure(var, type_) => allocator
                .text("Occurs check failed:")
                .append(allocator.space())
                .append(allocator.text(var.to_string()).single_quotes())
                .append(allocator.space())
                .append("~!")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator)),
            UnificationFailure(lhs, rhs) => allocator
                .text("Unification failed:")
                .append(allocator.space())
                .append(lhs.pretty_prec(0, allocator))
                .append(allocator.space())
                .append("~!")
                .append(allocator.space())
                .append(rhs.pretty_prec(0, allocator)),
            AlreadyDefined(name) => allocator
                .text("Name")
                .append(allocator.space())
                .append(allocator.text(name.to_owned()).single_quotes())
                .append(allocator.space())
                .append("is alread defined"),
            AmbiguousType(vars, type_) => allocator
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
            NotATuple(type_) => allocator
                .text("Not a tuple:")
                .append(allocator.space())
                .append(
                    allocator
                        .text("attempted to take projection on a value of type")
                        .append(allocator.space())
                        .append(type_.pretty_prec(0, allocator))
                        .append(allocator.space())
                        .append("which is not a tuple"),
                ),
            ProjectionOutOfBounds(element_types, index) => {
                let tuple_length = element_types.len();
                allocator
                    .text("Projection out of bounds:")
                    .append(allocator.space())
                    .append(
                        allocator
                            .text("attempted to take projection at index")
                            .append(allocator.space())
                            .append(index.to_string())
                            .append(allocator.space())
                            .append(allocator.text("on a tuple value of type"))
                            .append(allocator.space())
                            .append(Type::Tuple(element_types).pretty_prec(0, allocator))
                            .append(allocator.space())
                            .append("which only has")
                            .append(allocator.space())
                            .append(tuple_length.to_string())
                            .append(allocator.space())
                            .append("elements"),
                    )
            }
            AmbiguousTuple => allocator.text("Ambiguous tuple: cannot infer tuple type only from projection")
        }
    }
}
