use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
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
    AlreadyDefined(&'src str),
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
        if !self.scopes.is_empty() {
            let mut depth = self.scopes.len();
            loop {
                depth -= 1;

                if depth == 0 || self.scopes[depth].contains_key(&key) {
                    break;
                }
            }
            self.scopes[depth]
                .get(&key)
                .or_else(|| self.top_level.get(&key))
        } else {
            self.top_level.get(&key)
        }
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
        Ok(())
    }
}

#[allow(dead_code)]
fn show_skolems(skolems: &HashSet<String>) -> String {
    let mut s = "{".to_owned();
    let skolems = Vec::from_iter(skolems.iter().cloned());
    if !skolems.is_empty() {
        s.push_str(&skolems[0].as_ref());
        for i in 1..skolems.len() {
            s.push_str(", ");
            s.push_str(&skolems[i].as_ref());
        }
    }
    s.push('}');
    s
}

pub struct Typechecker<'src> {
    pub definitions: HashMap<&'src str, Scheme<'src>>,
    pub environment: Environment<&'src str, Scheme<'src>>,
    pub level: Level,
    pub uvar_supply: UVar,
    indent: u8,
}

impl<'src> Typechecker<'src> {
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
            }
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

    fn instantiate(&mut self, scheme: Scheme<'src>) -> Type<'src> {
        let Scheme { variables, type_ } = scheme;
        let subst = variables
            .iter()
            .map(|var| {
                (
                    var.clone(),
                    Type::TVar(Rc::new(RefCell::new(self.fresh_tvar()))),
                )
            })
            .collect();
        self.instantiate_type(&subst, type_)
    }

    fn generate_rigid_var_name(&self, uvar: UVar) -> String {
        let mut var_name = String::new();
        if uvar < 26 {
            var_name.push(
                std::char::from_u32(u8::try_from('A').unwrap() as u32 + uvar as u32).unwrap(),
            );
        } else {
            var_name.push('A');
            var_name.push_str(uvar.to_string().as_ref());
        }
        var_name
    }

    fn generalize(&mut self, skolems: &mut HashSet<String>, type_: Type<'src>) -> Type<'src> {
        // println!(
        //     "{}┬ generalizing {}, variables: {}, level: {}",
        //     "│   ".repeat(self.indent as usize),
        //     type_,
        //     show_skolems(&skolems),
        //     self.level,
        // );
        self.indent += 1;
        let type_gen = match type_ {
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Name { name } => Type::Name { name },
            Type::QVar(name) => Type::QVar(name),
            Type::TVar(tvar_ref) => {
                let tvar = (*tvar_ref).borrow().clone();
                match tvar {
                    TVar::Bound(type_) => self.generalize(skolems, type_),
                    TVar::Unbound { var, level } if level > self.level => {
                        let var_name = self.generate_rigid_var_name(var);
                        skolems.insert(var_name.clone());
                        Type::QVar(var_name)
                    }
                    _ => Type::TVar(tvar_ref),
                }
            }
            Type::Func(param_types, box return_type) => {
                let gen_param_types = param_types
                    .iter()
                    .map(|param_type| self.generalize(skolems, param_type.clone()))
                    .collect();
                let gen_return_type = self.generalize(skolems, return_type);
                Type::Func(gen_param_types, box gen_return_type)
            }
        };
        self.indent -= 1;
        // println!(
        //     "{}└ generalized into {}, variables: {}, level: {}",
        //     "│   ".repeat(self.indent as usize),
        //     type_gen.clone(),
        //     show_skolems(&skolems),
        //     self.level,
        // );
        type_gen
    }

    fn generalize_expr(
        &mut self,
        skolems: &mut HashSet<String>,
        expr: core::Expr<'src>,
    ) -> core::Expr<'src> {
        // println!(
        //     "{}┬ generalizing {}, variables: {}, level: {}",
        //     "│   ".repeat(self.indent as usize),
        //     expr,
        //     show_skolems(&skolems),
        //     self.level,
        // );
        self.indent += 1;
        let expr_gen = match expr {
            core::Expr::Lit(value) => core::Expr::Lit(value),
            core::Expr::Var(name) => core::Expr::Var(name),
            core::Expr::Add(box lhs, box rhs) => core::Expr::Add(
                box self.generalize_expr(skolems, lhs),
                box self.generalize_expr(skolems, rhs),
            ),
            core::Expr::App(box callee, args) => core::Expr::App(
                box self.generalize_expr(skolems, callee),
                args.iter()
                    .cloned()
                    .map(|arg| self.generalize_expr(skolems, arg))
                    .collect(),
            ),
            core::Expr::Abs(params, box body) => core::Expr::Abs(
                params
                    .iter()
                    .cloned()
                    .map(|core::Binding { name, type_ }| core::Binding {
                        name,
                        type_: self.generalize(skolems, type_),
                    })
                    .collect(),
                box self.generalize_expr(skolems, body),
            ),
            core::Expr::Let {
                name,
                type_params,
                params,
                return_type,
                box body,
                box cont,
            } => core::Expr::Let {
                name,
                type_params,
                params: params
                    .iter()
                    .cloned()
                    .map(|core::Binding { name, type_ }| core::Binding {
                        name,
                        type_: self.generalize(skolems, type_),
                    })
                    .collect(),
                return_type: self.generalize(skolems, return_type),
                body: box self.generalize_expr(skolems, body),
                cont: box self.generalize_expr(skolems, cont),
            },
        };
        self.indent -= 1;
        // println!(
        //     "{}└ generalized into {}, variables: {}, level: {}",
        //     "│   ".repeat(self.indent as usize),
        //     expr_gen.clone(),
        //     show_skolems(&skolems),
        //     self.level,
        // );
        expr_gen
    }

    fn occurs_check(&self, tvar_ref: Rc<RefCell<TVar<'src>>>, type_: Type<'src>) -> bool {
        match type_ {
            Type::Int => true,
            Type::Bool => true,
            Type::Name { .. } => true,
            Type::QVar(_) => true,
            Type::TVar(tvar_ref2) if Rc::ptr_eq(&tvar_ref, &tvar_ref2) => false,
            Type::TVar(tvar_ref2) => {
                let tvar2 = (*tvar_ref2).borrow().clone();
                match tvar2 {
                    TVar::Unbound {
                        var: var2,
                        level: level2,
                    } => {
                        let min_level = match (*tvar_ref).borrow().clone() {
                            TVar::Unbound { level, .. } => level.min(level2),
                            _ => level2,
                        };
                        *(*tvar_ref2).borrow_mut() = TVar::Unbound {
                            var: var2,
                            level: min_level,
                        };
                        true
                    }
                    TVar::Bound(type_) => self.occurs_check(tvar_ref, type_),
                }
            }
            Type::Func(param_types, box return_type) => {
                param_types
                    .iter()
                    .all(|param_type| self.occurs_check(tvar_ref.clone(), param_type.clone()))
                    && self.occurs_check(tvar_ref, return_type)
            }
        }
    }

    fn unify(&mut self, lhs: Type<'src>, rhs: Type<'src>) -> Result<(), TypeError<'src>> {
        println!(
            "{}─ {} ~ {}, level: {}",
            "│   ".repeat(self.indent as usize),
            lhs,
            rhs,
            self.level
        );
        self.indent += 1;
        let result = match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::QVar(a), Type::QVar(b)) if a == b => Ok(()),
            (Type::TVar(tvar_ref), type_) | (type_, Type::TVar(tvar_ref)) => {
                let tvar = (*tvar_ref).borrow().clone();
                match tvar {
                    TVar::Unbound { .. } => {
                        if self.occurs_check(tvar_ref.clone(), type_.clone()) {
                            *(*tvar_ref).borrow_mut() = TVar::Bound(type_);
                            Ok(())
                        } else {
                            Err(TypeError::OccursCheckFailure(tvar.clone(), type_))
                        }
                    }
                    TVar::Bound(bound_type) => self.unify(bound_type, type_),
                }
            }
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

    fn surface_type_to_core_type(&self, type_: surface::Type<'src>) -> Type<'src> {
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

    fn with_scope<T, F>(&mut self, params: Vec<surface::Binding<'src>>, f: F) -> T
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

    fn infer(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Type<'src>), TypeError<'src>> {
        println!(
            "{}┬ {} ⊢ {} ⇑ ?",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr,
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
                let param_types = param_vars
                    .iter()
                    .map(|var| Type::TVar(Rc::clone(&var)))
                    .collect();
                let return_var = Rc::new(RefCell::new(self.fresh_tvar()));
                self.unify(
                    callee_type,
                    Type::Func(param_types, box Type::TVar(Rc::clone(&return_var))),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.into_iter().zip(param_vars) {
                    let arg_elab = self.check(arg, Type::TVar(param_type))?;
                    args_elab.push(arg_elab);
                }
                Ok((
                    core::Expr::App(box callee_elab, args_elab),
                    Type::TVar(return_var),
                ))
            }
            surface::Expr::Abs(params, box body) => {
                self.with_scope(params, |self_, param_types, params_elab| {
                    let (body_elab, body_type) = self_.infer(body)?;
                    let func_type = Type::Func(param_types, box body_type);
                    Ok((core::Expr::Abs(params_elab, box body_elab), func_type))
                })
            }
            surface::Expr::Let {
                name,
                params,
                return_type,
                box body,
                box cont,
            } => {
                self.level += 1;
                let (type_params, params_elab, param_types, return_type, body_elab) = self
                    .with_scope(params, |self_, mut param_types, mut params_elab| {
                        let mut body_elab;
                        let mut body_type;
                        let mut skolems = HashSet::new();
                        if let Some(return_type) = return_type {
                            body_type = self_.surface_type_to_core_type(return_type);
                            body_elab = self_.check(body, body_type.clone())?;
                        } else {
                            (body_elab, body_type) = self_.infer(body)?;
                        }
                        self_.level -= 1;
                        body_type = self_.generalize(&mut skolems, body_type);
                        param_types = param_types
                            .iter()
                            .map(|param_type| self_.generalize(&mut skolems, param_type.clone()))
                            .collect();
                        params_elab = params_elab
                            .iter()
                            .map(|core::Binding { name, type_ }| core::Binding {
                                name,
                                type_: self_.generalize(&mut skolems, type_.clone()),
                            })
                            .collect();
                        body_elab = self_.generalize_expr(&mut skolems, body_elab);
                        Ok((
                            Vec::from_iter(skolems),
                            params_elab,
                            param_types,
                            body_type,
                            body_elab,
                        ))
                    })?;
                self.environment.new_scope();
                let type_ = if params_elab.is_empty() {
                    return_type.clone()
                } else {
                    Type::Func(param_types, box return_type.clone())
                };
                self.environment.insert(
                    name,
                    Scheme {
                        variables: type_params.clone(),
                        type_,
                    },
                );
                let (cont_elab, cont_type) = self.infer(cont)?;
                self.environment.pop_scope();
                Ok((
                    core::Expr::Let {
                        name,
                        type_params,
                        params: params_elab,
                        return_type,
                        body: box body_elab,
                        cont: box cont_elab,
                    },
                    cont_type,
                ))
            }
        }?;
        self.indent -= 1;
        println!(
            "{}└ {} ⊢ {} ⇑ {}",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr_elab,
            expr_type,
        );
        Ok((expr_elab, expr_type))
    }

    fn check(
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
            }
            type_ => type_,
        };
        println!(
            "{}┬ {} ⊢ {} ⇓ {}, level: {}",
            "│   ".repeat(self.indent as usize),
            self.environment,
            expr,
            type_,
            self.level,
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
                    let (body_elab, body_type) = self_.infer(body)?;
                    let func_type = Type::Func(param_types, box body_type);
                    self_.unify(func_type, expected_type.clone())?;
                    Ok(core::Expr::Abs(params_elab, box body_elab))
                })
            }
            (
                surface::Expr::Let {
                    name,
                    params,
                    return_type,
                    box body,
                    box cont,
                },
                expected_type,
            ) => {
                self.level += 1;
                let (type_params, params_elab, param_types, return_type, body_elab) = self
                    .with_scope(params, |self_, mut param_types, mut params_elab| {
                        let mut body_elab;
                        let mut body_type;
                        let mut skolems = HashSet::new();
                        if let Some(return_type) = return_type {
                            body_type = self_.surface_type_to_core_type(return_type);
                            body_elab = self_.check(body, body_type.clone())?;
                        } else {
                            (body_elab, body_type) = self_.infer(body)?;
                        }
                        self_.level -= 1;
                        body_type = self_.generalize(&mut skolems, body_type);
                        param_types = param_types
                            .iter()
                            .map(|param_type| self_.generalize(&mut skolems, param_type.clone()))
                            .collect();
                        params_elab = params_elab
                            .iter()
                            .map(|core::Binding { name, type_ }| core::Binding {
                                name,
                                type_: self_.generalize(&mut skolems, type_.clone()),
                            })
                            .collect();
                        body_elab = self_.generalize_expr(&mut skolems, body_elab);
                        let type_params = Vec::from_iter(skolems);
                        Ok((type_params, params_elab, param_types, body_type, body_elab))
                    })?;

                self.environment.new_scope();
                let type_ = if params_elab.is_empty() {
                    return_type.clone()
                } else {
                    Type::Func(param_types, box return_type.clone())
                };
                self.environment.insert(
                    name,
                    Scheme {
                        variables: type_params.clone(),
                        type_,
                    },
                );
                let cont_elab = self.check(cont, expected_type)?;
                self.environment.pop_scope();
                Ok(core::Expr::Let {
                    name,
                    type_params,
                    params: params_elab,
                    return_type,
                    body: box body_elab,
                    cont: box cont_elab,
                })
            }
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
            type_,
        );
        Ok(expr_elab)
    }

    pub fn infer_expr(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Scheme<'src>), TypeError<'src>> {
        self.level += 1;
        let (mut expr_elab, mut expr_type) = self.infer(expr)?;
        self.level -= 1;
        let mut skolems = HashSet::new();
        expr_elab = self.generalize_expr(&mut skolems, expr_elab);
        expr_type = self.generalize(&mut skolems, expr_type);
        let scheme = Scheme {
            variables: Vec::from_iter(skolems),
            type_: expr_type,
        };
        Ok((expr_elab, scheme))
    }

    pub fn run(
        &mut self,
        module: surface::Module<'src>,
    ) -> Result<core::Module<'src>, TypeError<'src>> {
        let mut functions = HashMap::new();
        for decl in module.declarations {
            match decl {
                surface::Decl::Func {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    self.level += 1;
                    let (decl_elab, scheme) =
                        self.with_scope(params, |self_, param_types, mut params_elab| {
                            let mut body_elab;
                            let mut body_type;
                            let mut skolems = HashSet::new();
                            if let Some(return_type) = return_type {
                                body_type = self_.surface_type_to_core_type(return_type);
                                body_elab = self_.check(body, body_type.clone())?;
                            } else {
                                (body_elab, body_type) = self_.infer(body)?;
                            }
                            self_.level -= 1;
                            body_type = self_.generalize(&mut skolems, body_type);
                            params_elab = params_elab
                                .iter()
                                .map(|core::Binding { name, type_ }| core::Binding {
                                    name,
                                    type_: self_.generalize(&mut skolems, type_.clone()),
                                })
                                .collect();
                            body_elab = self_.generalize_expr(&mut skolems, body_elab);
                            let type_params = Vec::from_iter(skolems);
                            Ok((
                                core::FuncDecl {
                                    name,
                                    type_params: type_params.clone(),
                                    params: params_elab,
                                    return_type: body_type.clone(),
                                    body: body_elab,
                                },
                                Scheme {
                                    variables: type_params,
                                    type_: Type::Func(param_types, box body_type),
                                },
                            ))
                        })?;
                    if functions.contains_key(&name) {
                        Err(TypeError::AlreadyDefined(name))?
                    } else {
                        functions.insert(name, decl_elab);
                        self.environment.insert(name, scheme);
                    }
                }
            }
            self.reset();
        }
        Ok(core::Module { functions })
    }
}
