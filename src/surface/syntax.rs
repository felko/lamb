use std::fmt::Display;

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
        for decl in &self.declarations {
            write!(f, "{decl}\n")?;
        }
        Ok(())
    }
}

fn show_type_params<'src>(f: &mut std::fmt::Formatter<'_>, type_params: Vec<&'src str>) -> std::fmt::Result {
    if !type_params.is_empty() {
        write!(f, "<{}", type_params[0])?;
        for i in 1..type_params.len() {
            write!(f, ", {}", type_params[i])?;
        }
        write!(f, ">")
    } else {
        Ok(())
    }
}

impl<'src> Display for Decl<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Func {name, type_params, params, return_type, body} => {
                write!(f, "def {name}")?;
                show_type_params(f, type_params.clone())?;
                for binding in params {
                    write!(f, "{binding} ")?;
                }
                if let Some(return_type) = return_type {
                    write!(f, ": {return_type} ")?;
                }
                write!(f, "= {body}")
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

impl<'src> Display for Binding<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::Typed(name, type_) => write!(f, "({} : {})", name, type_),
            Binding::Inferred(name) => write!(f, "{}", name),
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
                write!(
                    f, "(let {name}")?;
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
        }
    }
}
