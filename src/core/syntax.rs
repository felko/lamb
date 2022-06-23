use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

pub type UVar = u16;

#[derive(Debug, Clone)]
pub struct Module<'src> {
    pub functions: HashMap<&'src str, FuncDecl<'src>>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl<'src> {
    pub name: &'src str,
    pub type_params: Vec<String>,
    pub params: Vec<Binding<'src>>,
    pub return_type: Type<'src>,
    pub body: Expr<'src>,
}

#[derive(Debug, Clone)]
pub enum TVar<'src> {
    Unbound { var: UVar, level: u8 },
    Bound(Type<'src>),
}

#[derive(Debug, Clone)]
pub enum Type<'src> {
    Name { name: &'src str },
    QVar(String),
    TVar(Rc<RefCell<TVar<'src>>>),
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
    Var(&'src str),
    Abs(Vec<Binding<'src>>, Box<Expr<'src>>),
    Add(Box<Expr<'src>>, Box<Expr<'src>>),
    Let {
        name: &'src str,
        type_params: Vec<String>,
        params: Vec<Binding<'src>>,
        return_type: Type<'src>,
        body: Box<Expr<'src>>,
        cont: Box<Expr<'src>>,
    },
    App(Box<Expr<'src>>, Vec<Expr<'src>>),
}

impl<'src> Display for Module<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, func) in &self.functions {
            write!(f, "{func}\n")?;
        }
        Ok(())
    }
}

fn show_type_params<'src>(
    f: &mut std::fmt::Formatter<'_>,
    type_params: Vec<String>,
) -> std::fmt::Result {
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

impl<'src> Display for FuncDecl<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "def {}", self.name)?;
        show_type_params(f, self.type_params.clone())?;
        for binding in &self.params {
            write!(f, " {binding}")?;
        }
        write!(f, " : {} = {}", self.return_type, self.body)
    }
}

impl<'src> Display for TVar<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TVar::Unbound { var, .. } => write!(f, "${var}"),
            TVar::Bound(type_) => type_.fmt(f),
        }
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
            Type::TVar(tvar) => tvar.borrow().fmt(f),
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
                    write!(f, " {binding}")?;
                }
                write!(f, " : {return_type} = {body} in {cont})")
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
