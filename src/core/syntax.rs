use std::collections::HashMap;
use std::fmt::Display;

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
pub enum Expr<'src> {
    Lit(i32),
    Var {
        name: &'src str,
        type_args: Vec<Type<'src>>,
    },
    Abs {
        params: Vec<Binding<'src>>,
        return_type: Type<'src>,
        body: Box<Expr<'src>>,
    },
    Add(Box<Expr<'src>>, Box<Expr<'src>>),
    Let {
        name: &'src str,
        type_params: Vec<String>,
        type_: Type<'src>,
        value: Box<Expr<'src>>,
        cont: Box<Expr<'src>>,
    },
    App {
        callee: Box<Expr<'src>>,
        args: Vec<Expr<'src>>,
    },
}

impl<'src> Display for Module<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.values
            .values()
            .try_for_each(|decl| writeln!(f, "{decl}"))
    }
}

fn fmt_list<T: Display>(
    f: &mut std::fmt::Formatter<'_>,
    delimiters: (&str, &str),
    elements: Vec<T>,
) -> std::fmt::Result {
    if !elements.is_empty() {
        write!(f, "{}{}", delimiters.0, elements[0])?;
        elements
            .iter()
            .skip(1)
            .try_for_each(|elem| write!(f, ", {}", elem))?;
        write!(f, "{}", delimiters.1)
    } else {
        Ok(())
    }
}

impl<'src> Display for ValueDecl<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "def {}", self.name)?;
        fmt_list(f, ("<", ">"), self.type_params.clone())?;
        write!(f, " : {} = {}", self.type_, self.value)
    }
}

impl<'src> Display for Scheme<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.variables.is_empty() {
            write!(f, "{}", self.type_)
        } else {
            write!(f, "∀")?;
            for variable in &self.variables {
                write!(f, " {variable}")?;
            }
            write!(f, ". {}", self.type_)
        }
    }
}

impl<'src> Display for Type<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name { name } => name.fmt(f),
            Type::QVar(name) => name.fmt(f),
            Type::Func(param_types, return_type) => {
                write!(f, "(")?;
                for param_type in param_types {
                    write!(f, "{param_type} -> ")?;
                }
                write!(f, "{return_type})")
            }
            Type::Int => "Int".fmt(f),
            Type::Bool => "Bool".fmt(f),
        }
    }
}

impl<'src> Display for Binding<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} : {})", self.name, self.type_)
    }
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lit(value) => write!(f, "{value}"),
            Expr::Var { name, type_args } => {
                write!(f, "{name}")?;
                fmt_list(f, ("<", ">"), type_args.clone())
            }
            Expr::Abs {
                params,
                return_type,
                body,
            } => {
                write!(f, "(fun ")?;
                for binding in params {
                    write!(f, "{binding} ")?;
                }
                write!(f, ": {return_type} => ")?;
                write!(f, "{body})")
            }
            Expr::Add(lhs, rhs) => {
                write!(f, "{lhs} + {rhs}")
            }
            Expr::Let {
                name,
                type_params,
                type_,
                value,
                cont,
            } => {
                write!(f, "(let {name}")?;
                fmt_list(f, ("<", ">"), type_params.clone())?;
                write!(f, " : {type_} = {value} in {cont})")
            }
            Expr::App { callee, args } => {
                write!(f, "({callee}")?;
                for arg in args {
                    write!(f, " {arg}")?;
                }
                write!(f, ")")
            }
        }
    }
}
