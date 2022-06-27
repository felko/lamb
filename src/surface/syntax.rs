use crate::pretty::*;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type<'src> {
    Name(&'src str),
    Func(Vec<Type<'src>>, Box<Type<'src>>),
    Int,
    Bool,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Binding<'src> {
    Typed(&'src str, Type<'src>),
    Inferred(&'src str),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr<'src> {
    Lit(i32),
    Var(&'src str),
    Abs(Vec<Binding<'src>>, Box<Expr<'src>>),
    Add(Box<Expr<'src>>, Box<Expr<'src>>),
    Let {
        name: &'src str,
        type_params: Vec<&'src str>,
        params: Vec<Binding<'src>>,
        return_type: Option<Type<'src>>,
        body: Box<Expr<'src>>,
        cont: Box<Expr<'src>>,
    },
    App(Box<Expr<'src>>, Vec<Expr<'src>>),
    Ann(Box<Expr<'src>>, Type<'src>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl<'src> {
    Func {
        name: &'src str,
        type_params: Vec<&'src str>,
        params: Vec<Binding<'src>>,
        return_type: Option<Type<'src>>,
        body: Expr<'src>,
    },
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Module<'src> {
    pub declarations: Vec<Decl<'src>>,
}

impl<'src, 'a> PrettyPrec<'a> for Module<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        allocator.intersperse(
            self.declarations
                .iter()
                .map(|decl| decl.clone().pretty_prec(0, allocator)),
            allocator.hardline().append(allocator.hardline()),
        )
    }
}

impl<'src, 'a> PrettyPrec<'a> for Decl<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Decl::Func {
                name,
                type_params,
                params,
                return_type,
                body,
            } => {
                let mut result = allocator
                    .text("def")
                    .annotate(ColorSpec::new().set_bold(true).clone())
                    .append(allocator.space())
                    .append(allocator.text(name.to_owned()))
                    .append(allocator.space());

                if !type_params.is_empty() {
                    result = result.append(
                        allocator
                            .intersperse(
                                type_params
                                    .iter()
                                    .map(|type_param| (*type_param).to_owned()),
                                allocator.text(",").append(allocator.space()),
                            )
                            .angles(),
                    );
                }

                result = result.append(allocator.concat(params.iter().map(|param| {
                    param
                        .clone()
                        .pretty_prec(0, allocator)
                        .append(allocator.space())
                })));

                if let Some(return_type) = return_type {
                    result = result
                        .append(":")
                        .append(return_type.pretty_prec(0, allocator));
                }
                let body_pretty = body.pretty_prec(0, allocator);
                result.append("=").append(
                    allocator
                        .space()
                        .append(allocator.hardline())
                        .append(body_pretty.indent(PRETTY_INDENT_SIZE)),
                )
            }
        }
    }
}

impl<'src, 'a> PrettyPrec<'a> for Type<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Type::Name(name) => allocator.text(name.to_owned()),
            Type::Func(param_types, return_type) => allocator
                .intersperse(
                    param_types
                        .iter()
                        .map(|param_type| param_type.clone().pretty_prec(0, allocator)),
                    allocator.space().append("->").append(allocator.space()),
                )
                .append(allocator.space())
                .append("->")
                .append(allocator.space())
                .append(return_type.pretty_prec(0, allocator)),
            Type::Int => allocator.text("Int"),
            Type::Bool => allocator.text("Bool"),
        }
    }
}

impl<'src, 'a> PrettyPrec<'a> for Binding<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Binding::Typed(name, type_) => allocator
                .text("(")
                .append(name.to_owned())
                .append(allocator.space())
                .append(":")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator))
                .append(")"),
            Binding::Inferred(name) => allocator.text(name.to_owned()),
        }
    }
}

impl<'src, 'a> PrettyPrec<'a> for Expr<'src> {
    fn pretty_prec(self, prec: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Expr::Lit(value) => allocator.text(value.to_string()),
            Expr::Var(name) => allocator.text(name.to_owned()),
            Expr::Abs(params, body) => {
                let result =
                    allocator
                        .text("fun")
                        .annotate(ColorSpec::new().set_bold(true).clone())
                        .append(allocator.space())
                        .append(allocator.intersperse(
                            params
                                .iter()
                                .map(|param| param.clone().pretty_prec(0, allocator)),
                            allocator.space(),
                        ))
                        .append(allocator.space())
                        .append("=>")
                        .append(allocator.hardline())
                        .append(body.pretty_prec(0, allocator).indent(PRETTY_INDENT_SIZE));
                if prec > 0 {
                    result.parens()
                } else {
                    result
                }
            }
            Expr::Add(lhs, rhs) => lhs
                .pretty_prec(1, allocator)
                .append(allocator.space())
                .append("+")
                .append(allocator.space())
                .append(rhs.pretty_prec(1, allocator)),
            Expr::Let {
                name,
                type_params,
                params,
                return_type,
                body,
                cont,
            } => {
                let mut result = allocator
                    .text("let")
                    .annotate(ColorSpec::new().set_bold(true).clone())
                    .append(allocator.space())
                    .append(allocator.text(name.to_owned()))
                    .append(allocator.space());

                if !type_params.is_empty() {
                    result = result.append(
                        allocator
                            .intersperse(
                                type_params
                                    .iter()
                                    .map(|type_param| (*type_param).to_owned()),
                                allocator.text(",").append(allocator.space()),
                            )
                            .angles(),
                    )
                }

                result = result.append(allocator.concat(params.iter().map(|param| {
                    param
                        .clone()
                        .pretty_prec(0, allocator)
                        .append(allocator.space())
                })));

                if let Some(return_type) = return_type {
                    result = result
                        .append(":")
                        .append(return_type.pretty_prec(0, allocator));
                }
                let body_pretty = body.pretty_prec(0, allocator);
                result = result.append("=").append(
                    allocator
                        .space()
                        .append(allocator.hardline())
                        .append(body_pretty.indent(PRETTY_INDENT_SIZE))
                        .append(allocator.hardline())
                        .append(
                            allocator
                                .text("in")
                                .annotate(ColorSpec::new().set_bold(true).clone()),
                        )
                        .append(allocator.hardline())
                        .append(cont.pretty_prec(0, allocator).indent(PRETTY_INDENT_SIZE)),
                );
                if prec > 0 {
                    result.parens()
                } else {
                    result
                }
            }
            Expr::App(callee, args) => {
                let result = callee
                    .pretty_prec(2, allocator)
                    .append(allocator.space())
                    .append(allocator.intersperse(
                        args.iter().map(|arg| arg.clone().pretty_prec(2, allocator)),
                        allocator.space(),
                    ));
                if prec > 0 {
                    result.parens()
                } else {
                    result
                }
            }
            Expr::Ann(expr, type_) => expr
                .pretty_prec(1, allocator)
                .append(allocator.space())
                .append(":")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator))
                .parens(),
        }
    }
}
