use std::collections::HashMap;

use crate::pretty::*;

#[derive(Debug, Clone)]
pub struct Module<'src> {
    pub values: HashMap<&'src str, ValueDecl<'src>>,
}

#[derive(Debug, Clone)]
pub struct ValueDecl<'src> {
    pub name: &'src str,
    pub type_params: Vec<String>,
    pub type_: Type<'src>,
    pub value: Expr<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'src> {
    Name { name: &'src str },
    QVar(String),
    Func(Vec<Type<'src>>, Box<Type<'src>>),
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub struct Scheme<'src> {
    pub variables: Vec<String>,
    pub type_: Type<'src>,
}

#[derive(Debug, Clone)]
pub struct Binding<'src> {
    pub name: &'src str,
    pub type_: Type<'src>,
}

#[derive(Debug, Clone)]
pub enum Atom<'src> {
    Var {
        name: &'src str,
        type_args: Vec<Type<'src>>,
    },
    Lit(i32),
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Atom(Atom<'src>),
    Abs {
        params: Vec<Binding<'src>>,
        return_type: Type<'src>,
        body: Box<Expr<'src>>,
    },
    Add(Atom<'src>, Atom<'src>),
    Let {
        name: &'src str,
        type_params: Vec<String>,
        type_: Type<'src>,
        value: Box<Expr<'src>>,
        cont: Box<Expr<'src>>,
    },
    App {
        callee: Box<Atom<'src>>,
        args: Vec<Atom<'src>>,
    },
}

impl<'src, 'a> PrettyPrec<'a> for Module<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        allocator.intersperse(
            self.values
                .iter()
                .map(|(_, decl)| decl.clone().pretty_prec(0, allocator)),
            allocator.hardline().append(allocator.hardline()),
        )
    }
}

impl<'src, 'a> PrettyPrec<'a> for ValueDecl<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        let mut result = allocator
            .text("def")
            .annotate(ColorSpec::new().set_bold(true).clone())
            .append(allocator.space())
            .append(allocator.text(self.name.to_owned()));

        if !self.type_params.is_empty() {
            result = result.append(allocator.space()).append(
                allocator
                    .intersperse(
                        self.type_params
                            .iter()
                            .map(|type_param| (*type_param).to_owned()),
                        allocator.text(",").append(allocator.space()),
                    )
                    .angles(),
            );
        }

        result = result
            .append(allocator.space())
            .append(":")
            .append(allocator.space())
            .append(self.type_.pretty_prec(0, allocator))
            .append(allocator.space());

        let value_pretty = self.value.pretty_prec(0, allocator);
        result
            .append("=")
            .append(allocator.hardline())
            .append(value_pretty.indent(PRETTY_INDENT_SIZE))
    }
}

impl<'src, 'a> PrettyPrec<'a> for Type<'src> {
    fn pretty_prec(self, prec: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Type::Name { name } => allocator.text(name.to_owned()),
            Type::Func(param_types, return_type) => {
                let result = allocator
                    .intersperse(
                        param_types
                            .iter()
                            .map(|param_type| param_type.clone().pretty_prec(1, allocator)),
                        allocator.space().append("->").append(allocator.space()),
                    )
                    .append(allocator.space())
                    .append("->")
                    .append(allocator.space())
                    .append(return_type.pretty_prec(0, allocator));
                if prec > 0 {
                    result.parens()
                } else {
                    result
                }
            }
            Type::Int => allocator.text("Int"),
            Type::Bool => allocator.text("Bool"),
            Type::QVar(name) => allocator.text(name),
        }
    }
}

impl<'src, 'a> PrettyPrec<'a> for Binding<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        allocator
            .text("(")
            .append(self.name.to_owned())
            .append(allocator.space())
            .append(":")
            .append(allocator.space())
            .append(self.type_.pretty_prec(0, allocator))
            .append(")")
    }
}

impl<'src, 'a> PrettyPrec<'a> for Atom<'src> {
    fn pretty_prec(self, prec: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Atom::Lit(value) => allocator.text(value.to_string()),
            Atom::Var { name, type_args } => {
                let result = allocator.text(name.to_owned());
                if !type_args.is_empty() {
                    result.append(
                        allocator
                            .intersperse(
                                type_args
                                    .iter()
                                    .map(|type_arg| type_arg.clone().pretty_prec(0, allocator)),
                                allocator.text(",").append(allocator.space()),
                            )
                            .angles(),
                    )
                } else {
                    result
                }
            }
        }
    }
}

impl<'src, 'a> PrettyPrec<'a> for Expr<'src> {
    fn pretty_prec(self, prec: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Expr::Atom(atom) => atom.pretty_prec(0, allocator),
            Expr::Abs {
                params,
                return_type,
                body,
            } => {
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
                        .append(":")
                        .append(allocator.space())
                        .append(return_type.pretty_prec(0, allocator))
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
                type_,
                value,
                cont,
            } => {
                let mut result = allocator
                    .text("let")
                    .annotate(ColorSpec::new().set_bold(true).clone())
                    .append(allocator.space())
                    .append(allocator.text(name.to_owned()));

                if !type_params.is_empty() {
                    result = result.append(allocator.space()).append(
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

                result = result
                    .append(allocator.space())
                    .append(":")
                    .append(allocator.space())
                    .append(type_.pretty_prec(0, allocator))
                    .append(allocator.space());

                let value_pretty = value.pretty_prec(0, allocator);
                result = result.append("=").append(allocator.hardline()).append(
                    value_pretty
                        .indent(PRETTY_INDENT_SIZE)
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
            Expr::App { callee, args } => {
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
        }
    }
}
