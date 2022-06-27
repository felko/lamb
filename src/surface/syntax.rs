use std::fmt::Display;

use crate::pretty::*;
use pretty::DocAllocator as PrettyAllocator;

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

impl<'src> Display for Module<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.declarations
            .iter()
            .try_for_each(|decl| writeln!(f, "{decl}"))
    }
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

fn show_type_params<'src>(
    f: &mut std::fmt::Formatter<'_>,
    type_params: Vec<&'src str>,
) -> std::fmt::Result {
    if !type_params.is_empty() {
        write!(f, "<{}", type_params[0])?;
        type_params
            .iter()
            .skip(1)
            .try_for_each(|param| write!(f, ", {}", param))?;
        write!(f, ">")
    } else {
        Ok(())
    }
}

impl<'src> Display for Decl<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Func {
                name,
                type_params,
                params,
                return_type,
                body,
            } => {
                write!(f, "def {name}")?;
                show_type_params(f, type_params.clone())?;
                params
                    .iter()
                    .try_for_each(|binding| write!(f, "{binding} "))?;
                if let Some(return_type) = return_type {
                    write!(f, ": {return_type} ")?;
                }
                write!(f, "= {body}")
            }
        }
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

impl<'src> Display for Type<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name(name) => name.fmt(f),
            Type::Func(param_types, return_type) => {
                for binding in param_types {
                    write!(f, "{binding} -> ")?;
                }
                write!(f, "{return_type}")
            }
            Type::Int => "Int".fmt(f),
            Type::Bool => "Bool".fmt(f),
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

impl<'src> Display for Binding<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::Typed(name, type_) => write!(f, "({} : {})", name, type_),
            Binding::Inferred(name) => write!(f, "{}", name),
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

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lit(value) => write!(f, "{value}"),
            Expr::Var(name) => write!(f, "{name}"),
            Expr::Abs(params, body) => {
                write!(f, "(fun ")?;
                for binding in params {
                    write!(f, "{binding} ")?;
                }
                write!(f, "=> ")?;
                write!(f, "{body})")
            }
            Expr::Add(lhs, rhs) => {
                write!(f, "{lhs} + {rhs}")
            }
            Expr::Let {
                name,
                type_params,
                params,
                return_type,
                body,
                cont,
            } => {
                write!(f, "(let {name}")?;
                show_type_params(f, type_params.clone())?;
                for binding in params {
                    write!(f, "{binding} ")?;
                }
                if let Some(return_type) = return_type {
                    write!(f, ": {return_type} ")?;
                }
                write!(f, "= {body} in {cont})")
            }
            Expr::App(callee, args) => {
                write!(f, "({callee}")?;
                for arg in args {
                    write!(f, " {arg}")?;
                }
                write!(f, ")")
            }
            Expr::Ann(expr, type_) => {
                write!(f, "({expr} : {type_})")
            }
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
