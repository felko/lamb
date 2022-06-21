#![allow(dead_code, unused_variables)]

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

use crate::core;
use crate::core::syntax::{Scheme, TVar, Type, UVar};
use crate::surface;

type Level = u8;

#[derive(Debug)]
pub enum TypeError<'src> {
    ScopeError(&'src str),
    OccursCheckFailure(TVar<'src>, Type<'src>),
    UnificationFailure(Type<'src>, Type<'src>),
}

pub struct Environment<K, V> {
    top_level: HashMap<K, V>,
    scopes: Vec<HashMap<K, V>>,
}

impl<K, V> Environment<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Environment<K, V> {
        Environment {
            top_level: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.top_level.is_empty() && self.scopes.iter().all(|scope| scope.is_empty())
    }

    fn current_scope(&mut self) -> &mut HashMap<K, V> {
        self.scopes.last_mut().unwrap_or(&mut self.top_level)
    }

    pub fn lookup(&self, key: K) -> Option<&V> {
        let mut depth = self.scopes.len() - 1;
        while !self.scopes[depth].contains_key(&key) {
            depth -= 1;
        }
        self.scopes[depth]
            .get(&key)
            .or_else(|| self.top_level.get(&key))
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.current_scope().insert(key, value);
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn clear(&mut self) {
        self.top_level.clear();
        self.scopes.clear();
    }
}

impl<K: Display, V: Display> Display for Environment<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_scope<K: Display, V: Display>(
            f: &mut std::fmt::Formatter<'_>,
            scope: &HashMap<K, V>,
        ) -> std::fmt::Result {
            let assocs: Vec<(&K, &V)> = scope.iter().collect();
            if assocs.is_empty() {
                write!(f, "∅")
            } else {
                write!(f, "{} : {}", assocs[0].0, assocs[0].1)?;
                for i in 1..assocs.len() {
                    write!(f, ", {} : {}", assocs[i].0, assocs[i].1)?;
                }
                Ok(())
            }
        }
        if self.top_level.is_empty() {
            let mut first_non_empty_scope_index = 0;
            while first_non_empty_scope_index < self.scopes.len()
                && self.scopes[first_non_empty_scope_index].is_empty()
            {
                first_non_empty_scope_index += 1;
            }
            if first_non_empty_scope_index < self.scopes.len() {
                fmt_scope(f, &self.scopes[first_non_empty_scope_index])?;
                for i in (first_non_empty_scope_index + 1)..self.scopes.len() {
                    let scope = &self.scopes[i];
                    if !scope.is_empty() {
                        write!(f, "; ")?;
                        fmt_scope(f, &scope)?;
                    }
                }
            }
        } else {
            fmt_scope(f, &self.top_level)?;
            for scope in &self.scopes {
                if !scope.is_empty() {
                    write!(f, "; ")?;
                    fmt_scope(f, &scope)?;
                }
            }
        }
        Ok(())
    }
}

pub struct Typechecker<'src> {
    pub definitions: HashMap<&'src str, Scheme<'src>>,
    pub environment: Environment<&'src str, Scheme<'src>>,
    pub level: Level,
    pub uvar_supply: UVar,
    indent: u8,
}

impl<'src> Typechecker<'src> {
    const INDENT_SIZE: usize = 4;

    pub fn new() -> Typechecker<'src> {
        Typechecker {
            definitions: HashMap::new(),
            environment: Environment::new(),
            level: 0,
            uvar_supply: 0,
            indent: 0,
        }
    }

    pub fn reset(&mut self) {
        self.environment.clear();
        self.level = 0;
        self.uvar_supply = 0;
        self.indent = 0;
    }

    fn fresh_uvar(&mut self) -> UVar {
        let uvar = self.uvar_supply;
        self.uvar_supply += 1;
        uvar
    }

    fn fresh_tvar(&mut self) -> TVar<'src> {
        TVar::Unbound {
            var: self.fresh_uvar(),
            level: self.level,
        }
    }

    fn instantiate_type(
        &mut self,
        subst: &HashMap<String, Type<'src>>,
        type_: Type<'src>,
    ) -> Type<'src> {
        match type_ {
            Type::Name { name } => Type::Name { name },
            Type::QVar(name) => match subst.get(&name) {
                Some(type_) => type_.clone(),
                None => Type::QVar(name),
            },
            Type::TVar(var) => {
                let tvar = (*var).borrow().clone();
                match tvar {
                    TVar::Bound(type_) => self.instantiate_type(subst, type_),
                    TVar::Unbound { .. } => Type::TVar(var),
                }
            },
            Type::Func(parameter_types, box return_type) => {
                let parameter_types = parameter_types
                    .iter()
                    .map(|parameter_type| self.instantiate_type(&subst, parameter_type.clone()))
                    .collect();
                let return_type = self.instantiate_type(subst, return_type);
                Type::Func(parameter_types, box return_type)
            }
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
        }
    }

    pub fn instantiate(&mut self, scheme: Scheme<'src>) -> Type<'src> {
        let Scheme { variables, type_ } = scheme;
        let subst = variables
            .iter()
            .map(|var| (var.clone(), Type::TVar(Rc::new(RefCell::new(self.fresh_tvar())))))
            .collect();
        self.instantiate_type(&subst, type_)
    }

    pub fn occurs_check(&self, uvar: UVar, type_: &Type<'src>) -> Result<(), TypeError<'src>> {
        Ok(())
    }

    pub fn unify(&mut self, lhs: Type<'src>, rhs: Type<'src>) -> Result<(), TypeError<'src>> {
        println!("{}─ {} ~ {}", "│   ".repeat(self.indent as usize), lhs, rhs);
        self.indent += 1;
        let result = match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::QVar(a), Type::QVar(b)) if a == b => Ok(()),
            (Type::TVar(tvar_ref), type_) | (type_, Type::TVar(tvar_ref)) => {
                let tvar = (*tvar_ref).borrow().clone();
                match tvar {
                    TVar::Unbound { var, level } => {
                        self.occurs_check(var, &type_)?;
                        *(*tvar_ref).borrow_mut() = TVar::Bound(type_);
                        Ok(())
                    }
                    TVar::Bound(bound_type) => self.unify(bound_type, type_),
                }
            },
            (Type::Func(params1, box ret1), Type::Func(params2, box ret2)) => {
                let mut arg_index = 0;
                let (arity1, arity2) = (params1.len(), params2.len());
                while arg_index < arity1 && arg_index < arity2 {
                    self.unify(params1[arg_index].clone(), params2[arg_index].clone())?;
                    arg_index += 1;
                }
                match arity1.cmp(&arity2) {
                    Ordering::Equal => self.unify(ret1, ret2),
                    Ordering::Less => self.unify(
                        ret1,
                        Type::Func(params2.iter().cloned().skip(arg_index).collect(), box ret2),
                    ),
                    Ordering::Greater => self.unify(
                        Type::Func(params1.iter().cloned().skip(arg_index).collect(), box ret1),
                        ret2,
                    ),
                }
            }
            (lhs, rhs) => Err(TypeError::UnificationFailure(lhs, rhs)),
        };
        self.indent -= 1;
        result
    }

    pub fn surface_type_to_core_type(&self, type_: surface::Type<'src>) -> Type<'src> {
        match type_ {
            surface::Type::Name(name) => Type::Name { name },
            surface::Type::Var(name) => Type::QVar(name.to_owned()),
            surface::Type::Func(params, box ret) => Type::Func(
                params
                    .iter()
                    .cloned()
                    .map(|param_type| self.surface_type_to_core_type(param_type))
                    .collect(),
                box self.surface_type_to_core_type(ret),
            ),
            surface::Type::Int => Type::Int,
            surface::Type::Bool => Type::Bool,
        }
    }

    pub fn with_scope<T, F>(&mut self, params: Vec<surface::Binding<'src>>, f: F) -> T
    where
        F: FnOnce(&mut Self, Vec<Type<'src>>, Vec<core::Binding<'src>>) -> T,
    {
        self.environment.new_scope();
        let mut param_types = Vec::new();
        let mut params_elab = Vec::new();
        for binding in params {
            match binding {
                surface::Binding::Inferred(name) => {
                    let param_type = Rc::new(RefCell::new(self.fresh_tvar()));
                    self.environment.insert(
                        name,
                        Scheme {
                            variables: Vec::new(),
                            type_: Type::TVar(Rc::clone(&param_type)),
                        },
                    );
                    param_types.push(Type::TVar(Rc::clone(&param_type)));
                    params_elab.push(core::Binding {
                        name,
                        type_: Type::TVar(param_type),
                    });
                }
                surface::Binding::Typed(name, type_) => {
                    let param_type = self.surface_type_to_core_type(type_);
                    self.environment.insert(
                        name,
                        Scheme {
                            variables: Vec::new(),
                            type_: param_type.clone(),
                        },
                    );
                    param_types.push(param_type.clone());
                    params_elab.push(core::Binding {
                        name,
                        type_: param_type,
                    });
                }
            }
        }
        let result = f(self, param_types, params_elab);
        self.environment.pop_scope();
        result
    }

    pub fn infer(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Type<'src>), TypeError<'src>> {
        println!(
            "{}┬ {} ⊢ {} ⇑ ?",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr
        );
        self.indent += 1;
        let (expr_elab, expr_type) = match expr {
            surface::Expr::Lit(value) => Ok((core::Expr::Lit(value), Type::Int)),
            surface::Expr::Var(name) => {
                if let Some(scheme) = self.environment.lookup(name) {
                    Ok((core::Expr::Var(name), self.instantiate(scheme.clone())))
                } else {
                    Err(TypeError::ScopeError(name))
                }
            }
            surface::Expr::Add(box lhs, box rhs) => {
                let lhs_elab = self.check(lhs, Type::Int)?;
                let rhs_elab = self.check(rhs, Type::Int)?;
                Ok((core::Expr::Add(box lhs_elab, box rhs_elab), Type::Int))
            }
            surface::Expr::App(box callee, args) => {
                let (callee_elab, callee_type) = self.infer(callee)?;
                let mut param_vars: Vec<Rc<RefCell<TVar<'src>>>> = Vec::new();
                for _ in 0..args.len() {
                    param_vars.push(Rc::new(RefCell::new(self.fresh_tvar())));
                }
                let param_types = param_vars.iter().map(|var| Type::TVar(Rc::clone(&var))).collect();
                let return_var = Rc::new(RefCell::new(self.fresh_tvar()));
                self.unify(
                    callee_type,
                    Type::Func(
                        param_types,
                        box Type::TVar(Rc::clone(&return_var)),
                    ),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.into_iter().zip(param_vars) {
                    let arg_elab = self.check(arg, Type::TVar(param_type))?;
                    args_elab.push(arg_elab);
                }
                Ok((core::Expr::App(box callee_elab, args_elab), Type::TVar(return_var)))
            }
            surface::Expr::Abs(params, box body) => {
                self.with_scope(params, |self_, param_types, params_elab| {
                    let (body_elab, body_type) = self_.infer(body)?;
                    let func_type = Type::Func(param_types, box body_type);
                    Ok((core::Expr::Abs(params_elab, box body_elab), func_type))
                })
            }
            surface::Expr::Let(name, params, return_type, value, cont) => {
                panic!()
            }
        }?;
        self.indent -= 1;
        println!(
            "{}└ {} ⊢ {} ⇑ {}",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr_elab,
            expr_type
        );
        Ok((expr_elab, expr_type))
    }

    pub fn check(
        &mut self,
        expr: surface::Expr<'src>,
        mut type_: Type<'src>,
    ) -> Result<core::Expr<'src>, TypeError<'src>> {
        type_ = match type_ {
            Type::TVar(var) => {
                let tvar = (*var).borrow().clone();
                match tvar {
                    TVar::Bound(type_) => type_,
                    TVar::Unbound { .. } => Type::TVar(var),
                }
            },
            type_ => type_,
        };
        println!(
            "{}┬ {} ⊢ {} ⇓ {}",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr,
            type_
        );
        self.indent += 1;
        let expr_elab = match (expr, type_.clone()) {
            (surface::Expr::Lit(value), Type::Int) => Ok(core::Expr::Lit(value)),
            (surface::Expr::Var(name), expected_type) => {
                if let Some(scheme) = self.environment.lookup(name) {
                    let var_type = self.instantiate(scheme.clone());
                    self.unify(var_type, expected_type)?;
                    Ok(core::Expr::Var(name))
                } else {
                    Err(TypeError::ScopeError(name))
                }
            }
            (surface::Expr::Add(box lhs, box rhs), Type::Int) => {
                let lhs_elab = self.check(lhs, Type::Int)?;
                let rhs_elab = self.check(rhs, Type::Int)?;
                Ok(core::Expr::Add(box lhs_elab, box rhs_elab))
            }
            (surface::Expr::App(box callee, args), expected_type) => {
                let (callee_elab, callee_type) = self.infer(callee)?;
                let param_types: Vec<Type<'src>> = args
                    .iter()
                    .map(|_| Type::TVar(Rc::new(RefCell::new(self.fresh_tvar()))))
                    .collect();
                self.unify(
                    callee_type,
                    Type::Func(param_types.iter().cloned().collect(), box expected_type),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.iter().cloned().zip(param_types.iter().cloned()) {
                    let arg_elab = self.check(arg, param_type)?;
                    args_elab.push(arg_elab);
                }
                Ok(core::Expr::App(box callee_elab, args_elab))
            }
            (surface::Expr::Abs(params, box body), expected_type) => {
                self.with_scope(params, |self_, param_types, params_elab| {
                    let return_type = self_.fresh_tvar();
                    let (body_elab, body_type) = self_.infer(body)?;
                    let func_type = Type::Func(param_types, box body_type);
                    Ok(core::Expr::Abs(params_elab, box body_elab))
                })
            }
            (surface::Expr::Let(_, _, _, _, _), expected_type) => panic!(),
            (expr, expected_type) => {
                let (expr_elab, expr_type) = self.infer(expr)?;
                self.unify(expr_type, expected_type)?;
                Ok(expr_elab)
            }
        }?;
        self.indent -= 1;
        println!(
            "{}└ {} ⊢ {} ⇓ {}",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr_elab,
            type_
        );
        Ok(expr_elab)
    }
}
