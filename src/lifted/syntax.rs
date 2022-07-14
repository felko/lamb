use std::collections::HashMap;

pub use crate::anf::{Binding, Scheme, Type, Value};
pub use crate::common::Literal;
use crate::pretty::*;

#[derive(Debug, Clone)]
pub struct JoinPoint<'src> {
    pub name: String,
    pub params: Vec<Binding<'src>>,
    pub return_type: Type<'src>,
    pub body: Expr<'src>,
}

#[derive(Debug, Clone)]
pub struct Module<'src> {
    pub functions: HashMap<String, FunDecl<'src>>,
}

#[derive(Debug, Clone)]
pub struct FunDecl<'src> {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<Binding<'src>>,
    pub return_type: Type<'src>,
    pub body: Expr<'src>,
    pub join_points: Vec<JoinPoint<'src>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Halt {
        value: Value<'src>,
    },
    Jump {
        name: String,
        args: Vec<Value<'src>>,
    },
    LetAdd {
        name: String,
        lhs: Value<'src>,
        rhs: Value<'src>,
        cont: Box<Expr<'src>>,
    },
    LetVal {
        name: String,
        type_: Type<'src>,
        value: Value<'src>,
        cont: Box<Expr<'src>>,
    },
    LetApp {
        name: String,
        type_: Type<'src>,
        callee: String,
        type_args: Vec<Type<'src>>,
        args: Vec<Value<'src>>,
        cont: Box<Expr<'src>>,
    },
    LetTuple {
        name: String,
        types: Vec<Type<'src>>,
        elements: Vec<Value<'src>>,
        cont: Box<Expr<'src>>,
    },
    LetProj {
        name: String,
        type_: Type<'src>,
        tuple: Value<'src>,
        index: u8,
        cont: Box<Expr<'src>>,
    },
    If {
        cond: Value<'src>,
        return_type: Type<'src>,
        then: Box<Expr<'src>>,
        else_: Box<Expr<'src>>,
    },
}

impl<'a, 'src> PrettyPrec<'a> for Module<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        allocator.intersperse(
            self.functions
                .iter()
                .map(|(_, decl)| decl.clone().pretty_prec(0, allocator)),
            allocator.hardline().append(allocator.hardline()),
        )
    }
}

impl<'src, 'a> PrettyPrec<'a> for JoinPoint<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        allocator
            .text("join")
            .annotate(ColorSpec::new().set_bold(true).clone())
            .append(allocator.space())
            .append(allocator.text(self.name))
            .append(
                allocator
                    .intersperse(
                        self.params
                            .iter()
                            .map(|param| param.clone().pretty_prec(0, allocator)),
                        ", ",
                    )
                    .parens(),
            )
            .append(allocator.space())
            .append(":")
            .append(allocator.space())
            .append(self.return_type.pretty_prec(0, allocator))
            .append(allocator.space())
            .append("=")
            .append(
                allocator.hardline().append(
                    self.body
                        .pretty_prec(0, allocator)
                        .indent(PRETTY_INDENT_SIZE),
                ),
            )
            .append(allocator.hardline())
    }
}

impl<'src, 'a> PrettyPrec<'a> for FunDecl<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        let mut result = allocator
            .text("def")
            .annotate(ColorSpec::new().set_bold(true).clone())
            .append(allocator.space())
            .append(allocator.text(self.name.to_owned()));

        if !self.type_params.is_empty() {
            result = result.append(
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
            .append(
                allocator
                    .intersperse(
                        self.params
                            .iter()
                            .map(|param| param.clone().pretty_prec(0, allocator)),
                        ", ",
                    )
                    .parens(),
            )
            .append(allocator.space())
            .append(":")
            .append(allocator.space())
            .append(self.return_type.pretty_prec(0, allocator));

        let body_pretty = self.body.pretty_prec(0, allocator);
        result
            .append(allocator.space())
            .append("=")
            .append(
                allocator
                    .hardline()
                    .append(body_pretty.indent(PRETTY_INDENT_SIZE)),
            )
            .append(allocator.concat(self.join_points.iter().map(|join_point| {
                allocator
                    .hardline()
                    .append(join_point.clone().pretty_prec(0, allocator))
            })))
    }
}

impl<'a, 'src> PrettyPrec<'a> for Expr<'src> {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Expr::Halt { value } => allocator
                .text("halt")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(value.pretty_prec(0, allocator)),
            Expr::Jump { name, args } => allocator
                .text("jump")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(name)
                .append(
                    allocator
                        .intersperse(
                            args.iter().map(|arg| arg.clone().pretty_prec(0, allocator)),
                            ", ",
                        )
                        .parens(),
                ),
            Expr::LetAdd {
                name,
                lhs,
                rhs,
                cont,
            } => allocator
                .text("let")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(allocator.text(name))
                .append(allocator.space())
                .append("=")
                .append(allocator.space())
                .append(lhs.pretty_prec(0, allocator))
                .append(allocator.space())
                .append("+")
                .append(allocator.space())
                .append(rhs.pretty_prec(0, allocator))
                .append(allocator.hardline())
                .append(cont.pretty_prec(0, allocator)),
            Expr::LetVal {
                name,
                type_,
                value,
                cont,
            } => allocator
                .text("let")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(allocator.text(name))
                .append(allocator.space())
                .append(":")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator))
                .append(allocator.space())
                .append("=")
                .append(allocator.space())
                .append(value.pretty_prec(0, allocator))
                .append(allocator.hardline())
                .append(cont.pretty_prec(0, allocator)),
            Expr::LetApp {
                name,
                type_,
                callee,
                type_args,
                args,
                cont,
            } => {
                let mut result = allocator
                    .text("let")
                    .annotate(ColorSpec::new().set_bold(true).clone())
                    .append(allocator.space())
                    .append(allocator.text(name))
                    .append(allocator.space())
                    .append(":")
                    .append(allocator.space())
                    .append(type_.pretty_prec(0, allocator))
                    .append(allocator.space())
                    .append("=")
                    .append(allocator.space())
                    .append(callee);

                if !type_args.is_empty() {
                    result = result.append(allocator.space()).append(
                        allocator
                            .intersperse(
                                type_args
                                    .iter()
                                    .map(|type_arg| type_arg.clone().pretty_prec(0, allocator)),
                                ", ",
                            )
                            .angles(),
                    )
                }

                result
                    .append(
                        allocator
                            .intersperse(
                                args.iter().map(|arg| arg.clone().pretty_prec(0, allocator)),
                                ", ",
                            )
                            .parens(),
                    )
                    .append(allocator.hardline())
                    .append(cont.pretty_prec(0, allocator))
            }
            Expr::LetTuple {
                name,
                types,
                elements,
                cont,
            } => allocator
                .text("let")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(allocator.text(name))
                .append(allocator.space())
                .append(":")
                .append(allocator.space())
                .append(if types.len() == 1 {
                    types[0]
                        .clone()
                        .pretty_prec(0, allocator)
                        .append(",")
                        .parens()
                } else {
                    allocator
                        .intersperse(
                            types
                                .iter()
                                .map(|element| element.clone().pretty_prec(0, allocator)),
                            ", ",
                        )
                        .parens()
                })
                .append(allocator.space())
                .append("=")
                .append(allocator.space())
                .append(if elements.len() == 1 {
                    elements[0]
                        .clone()
                        .pretty_prec(0, allocator)
                        .append(",")
                        .parens()
                } else {
                    allocator
                        .intersperse(
                            elements
                                .iter()
                                .map(|element| element.clone().pretty_prec(0, allocator)),
                            ", ",
                        )
                        .parens()
                })
                .append(allocator.hardline())
                .append(cont.pretty_prec(0, allocator)),
            Expr::LetProj {
                name,
                type_,
                tuple,
                index,
                cont,
            } => allocator
                .text("let")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(allocator.text(name))
                .append(allocator.space())
                .append(":")
                .append(allocator.space())
                .append(type_.pretty_prec(0, allocator))
                .append(allocator.space())
                .append("=")
                .append(allocator.space())
                .append(tuple.pretty_prec(1, allocator))
                .append(".")
                .append(index.to_string())
                .append(allocator.hardline())
                .append(cont.pretty_prec(0, allocator)),
            Expr::If {
                cond,
                return_type,
                then,
                else_,
            } => allocator
                .text("if")
                .annotate(ColorSpec::new().set_bold(true).clone())
                .append(allocator.space())
                .append(cond.pretty_prec(0, allocator))
                .append(allocator.space())
                .append(
                    allocator
                        .text("returns")
                        .annotate(ColorSpec::new().set_bold(true).clone()),
                )
                .append(allocator.space())
                .append(return_type.pretty_prec(0, allocator))
                .append(allocator.space())
                .append(
                    allocator
                        .text("then")
                        .annotate(ColorSpec::new().set_bold(true).clone()),
                )
                .append(allocator.hardline())
                .append(then.pretty_prec(0, allocator).indent(PRETTY_INDENT_SIZE))
                .append(allocator.hardline())
                .append(
                    allocator
                        .text("else")
                        .annotate(ColorSpec::new().set_bold(true).clone()),
                )
                .append(allocator.hardline())
                .append(else_.pretty_prec(0, allocator).indent(PRETTY_INDENT_SIZE)),
        }
    }
}
