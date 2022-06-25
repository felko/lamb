use slotmap::{SecondaryMap, SlotMap};
use std::cmp::Ordering;
use std::collections::HashMap;

use crate::core;
use crate::core::prim::PRIMITIVES;
use crate::core::syntax::{Scheme, TVar, TVarKey, Type};
use crate::env::Env;
use crate::surface;

type Level = u8;

#[derive(Debug)]
pub enum TypeError<'src> {
    ScopeError(&'src str),
    OccursCheckFailure(TVarKey, Type<'src>),
    UnificationFailure(Type<'src>, Type<'src>),
    AlreadyDefined(&'src str),
    AmbiguousType(Vec<String>, Type<'src>),
}

type Instantiation<'src> = HashMap<&'src str, Option<TVarKey>>;

pub struct Typechecker<'src> {
    pub definitions: HashMap<&'src str, Scheme<'src>>,
    pub env: Env<&'src str, Scheme<'src>>,
    pub level: Level,
    pub bindings: SlotMap<TVarKey, TVar<'src>>,
    tvar_supply: u32,
}

impl<'src> Typechecker<'src> {
    pub fn new() -> Typechecker<'src> {
        Typechecker {
            definitions: HashMap::new(),
            env: Env::with_toplevel(PRIMITIVES.clone()),
            level: 0,
            bindings: SlotMap::with_key(),
            tvar_supply: 0,
        }
    }

    pub fn reset(&mut self) {
        self.env.clear();
        self.level = 0;
        self.bindings.clear();
        self.tvar_supply = 0;
    }

    fn fresh_tvar(&mut self) -> TVarKey {
        let index = self.tvar_supply;
        let tvar_key = self.bindings.insert(TVar::Unbound {
            level: self.level,
            index,
        });
        self.tvar_supply += 1;
        tvar_key
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
            Type::TVar(tvar_key) => match &self.bindings[tvar_key] {
                TVar::Bound(type_) => self.instantiate_type(subst, type_.clone()),
                TVar::Unbound { .. } => Type::TVar(tvar_key),
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

    fn instantiate(&mut self, scheme: Scheme<'src>) -> (Type<'src>, Vec<Type<'src>>) {
        let Scheme { variables, type_ } = scheme;
        let mut type_params = Vec::new();
        for _ in 0..variables.len() {
            type_params.push(Type::TVar(self.fresh_tvar()));
        }
        let subst = variables.iter().cloned().zip(type_params.clone()).collect();
        (self.instantiate_type(&subst, type_), type_params)
    }

    fn generate_rigid_var_name(&self, tvar_index: u32) -> String {
        let mut var_name = String::new();
        if tvar_index < 26 {
            var_name.push(
                std::char::from_u32(u8::try_from('A').unwrap() as u32 + tvar_index as u32).unwrap(),
            );
        } else {
            var_name.push('A');
            var_name.push_str(tvar_index.to_string().as_ref());
        }
        var_name
    }

    fn generalize(
        &mut self,
        skolems: &mut SecondaryMap<TVarKey, String>,
        type_: Type<'src>,
    ) -> Type<'src> {
        match type_ {
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Name { name } => Type::Name { name },
            Type::QVar(name) => Type::QVar(name),
            Type::TVar(tvar_key) => match &self.bindings[tvar_key].clone() {
                TVar::Bound(type_) => self.generalize(skolems, type_.clone()),
                TVar::Unbound { index, level } if *level > self.level => {
                    let var_name = self.generate_rigid_var_name(*index);
                    skolems.insert(tvar_key, var_name.clone());
                    Type::QVar(var_name)
                }
                _ => Type::TVar(tvar_key),
            },
            Type::Func(param_types, box return_type) => {
                let gen_param_types = param_types
                    .iter()
                    .map(|param_type| self.generalize(skolems, param_type.clone()))
                    .collect();
                let gen_return_type = self.generalize(skolems, return_type);
                Type::Func(gen_param_types, box gen_return_type)
            }
        }
    }

    fn generalize_expr(
        &mut self,
        skolems: &mut SecondaryMap<TVarKey, String>,
        expr: core::Expr<'src>,
    ) -> core::Expr<'src> {
        match expr {
            core::Expr::Lit(value) => core::Expr::Lit(value),
            core::Expr::Var { name, type_args } => core::Expr::Var {
                name,
                type_args: type_args
                    .iter()
                    .map(|type_arg| self.generalize(skolems, type_arg.clone()))
                    .collect(),
            },
            core::Expr::Add(box lhs, box rhs) => core::Expr::Add(
                box self.generalize_expr(skolems, lhs),
                box self.generalize_expr(skolems, rhs),
            ),
            core::Expr::App { box callee, args } => core::Expr::App {
                callee: box self.generalize_expr(skolems, callee),
                args: args
                    .iter()
                    .cloned()
                    .map(|arg| self.generalize_expr(skolems, arg))
                    .collect(),
            },
            core::Expr::Abs {
                params,
                return_type,
                box body,
            } => core::Expr::Abs {
                params: params
                    .iter()
                    .cloned()
                    .map(
                        |core::Binding {
                             name,
                             type_: param_type,
                         }| core::Binding {
                            name,
                            type_: self.generalize(skolems, param_type),
                        },
                    )
                    .collect(),
                return_type: self.generalize(skolems, return_type),
                body: box self.generalize_expr(skolems, body),
            },
            core::Expr::Let {
                name,
                type_params,
                type_,
                box value,
                box cont,
            } => core::Expr::Let {
                name,
                type_params,
                type_: self.generalize(skolems, type_),
                value: box self.generalize_expr(skolems, value),
                cont: box self.generalize_expr(skolems, cont),
            },
        }
    }

    fn occurs_check(&mut self, tvar_key: TVarKey, type_: &Type<'src>) -> bool {
        match type_ {
            Type::Int => true,
            Type::Bool => true,
            Type::Name { .. } => true,
            Type::QVar(_) => true,
            Type::TVar(tvar_key2) if tvar_key == *tvar_key2 => false,
            Type::TVar(tvar_key2) => {
                let [tvar_ref, tvar_ref2] = unsafe {
                    self.bindings
                        .get_disjoint_unchecked_mut([tvar_key, *tvar_key2])
                };
                match (*tvar_ref2).clone() {
                    TVar::Unbound {
                        level: level2,
                        index,
                    } => {
                        let min_level = match *tvar_ref {
                            TVar::Unbound { level, .. } => level.min(level2),
                            _ => level2,
                        };
                        *tvar_ref2 = TVar::Unbound {
                            level: min_level,
                            index,
                        };
                        true
                    }
                    TVar::Bound(type_) => self.occurs_check(tvar_key, &type_),
                }
            }
            Type::Func(param_types, box return_type) => {
                param_types
                    .iter()
                    .all(|param_type| self.occurs_check(tvar_key, param_type))
                    && self.occurs_check(tvar_key, return_type)
            }
        }
    }

    fn find(&mut self, type_: Type<'src>) -> Type<'src> {
        match type_ {
            Type::TVar(tvar_key) => match &self.bindings[tvar_key] {
                TVar::Bound(parent) => {
                    let repr = self.find(parent.clone());
                    self.bindings[tvar_key] = TVar::Bound(repr.clone());
                    repr
                }
                _ => Type::TVar(tvar_key),
            },
            type_ => type_,
        }
    }

    fn unify(&mut self, lhs: Type<'src>, rhs: Type<'src>) -> Result<(), TypeError<'src>> {
        match (self.find(lhs), self.find(rhs)) {
            (t, u) if t == u => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::QVar(a), Type::QVar(b)) if a == b => Ok(()),
            (Type::TVar(tvar_key), type_) | (type_, Type::TVar(tvar_key)) => {
                match &self.bindings[tvar_key] {
                    TVar::Unbound { .. } => {
                        if self.occurs_check(tvar_key, &type_) {
                            self.bindings[tvar_key] = TVar::Bound(type_);
                            Ok(())
                        } else {
                            Err(TypeError::OccursCheckFailure(tvar_key, type_))
                        }
                    }
                    TVar::Bound(bound_type) => self.unify(bound_type.clone(), type_),
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
        }
    }

    fn surface_type_to_core_type(
        &mut self,
        subst: &mut Instantiation<'src>,
        type_: surface::Type<'src>,
    ) -> Type<'src> {
        match type_ {
            surface::Type::Name(name) => match subst.get_mut(&name) {
                Some(Some(tvar_key)) => Type::TVar(*tvar_key),
                Some(tvar_ref) => {
                    let tvar_key2 = self.fresh_tvar();
                    *tvar_ref = Some(tvar_key2);
                    Type::TVar(tvar_key2)
                }
                None => Type::Name { name },
            },
            surface::Type::Func(params, box ret) => Type::Func(
                params
                    .iter()
                    .cloned()
                    .map(|param_type| self.surface_type_to_core_type(subst, param_type))
                    .collect(),
                box self.surface_type_to_core_type(subst, ret),
            ),
            surface::Type::Int => Type::Int,
            surface::Type::Bool => Type::Bool,
        }
    }

    fn elab_param(
        &mut self,
        subst: &mut Instantiation<'src>,
        param: surface::Binding<'src>,
    ) -> core::Binding<'src> {
        match param {
            surface::Binding::Inferred(name) => {
                let param_type = self.fresh_tvar();
                self.env.insert(
                    name,
                    Scheme {
                        variables: Vec::new(),
                        type_: Type::TVar(param_type),
                    },
                );
                core::Binding {
                    name,
                    type_: Type::TVar(param_type),
                }
            }
            surface::Binding::Typed(name, type_) => {
                let param_type = self.surface_type_to_core_type(subst, type_);
                self.env.insert(
                    name,
                    Scheme {
                        variables: Vec::new(),
                        type_: param_type.clone(),
                    },
                );
                core::Binding {
                    name,
                    type_: param_type,
                }
            }
        }
    }

    fn elab_params(
        &mut self,
        subst: &mut Instantiation<'src>,
        params: Vec<surface::Binding<'src>>,
    ) -> Vec<core::Binding<'src>> {
        params
            .iter()
            .map(|param| self.elab_param(subst, param.clone()))
            .collect()
    }

    fn new_scope_with_params(&mut self, params: Vec<core::Binding<'src>>) {
        self.env.new_scope();
        for core::Binding { name, type_ } in params {
            self.env.insert(
                name,
                Scheme {
                    variables: Vec::new(),
                    type_: type_.clone(),
                },
            );
        }
    }

    fn infer(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Type<'src>), TypeError<'src>> {
        match expr {
            surface::Expr::Lit(value) => Ok((core::Expr::Lit(value), Type::Int)),
            surface::Expr::Var(name) => {
                if let Some(scheme) = self.env.lookup(name) {
                    let (instantiated, type_args) = self.instantiate(scheme.clone());
                    Ok((core::Expr::Var { name, type_args }, instantiated))
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
                let mut param_vars: Vec<TVarKey> = Vec::new();
                for _ in 0..args.len() {
                    param_vars.push(self.fresh_tvar());
                }
                let param_types = param_vars.iter().map(|var| Type::TVar(*var)).collect();
                let return_var = self.fresh_tvar();
                self.unify(
                    callee_type,
                    Type::Func(param_types, box Type::TVar(return_var)),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.into_iter().zip(param_vars) {
                    let arg_elab = self.check(arg, Type::TVar(param_type))?;
                    args_elab.push(arg_elab);
                }
                Ok((
                    core::Expr::App {
                        callee: box callee_elab,
                        args: args_elab,
                    },
                    Type::TVar(return_var),
                ))
            }
            surface::Expr::Abs(params, box body) => {
                let mut subst = HashMap::new();
                let params_elab = self.elab_params(&mut subst, params);
                self.new_scope_with_params(params_elab.clone());
                let (body_elab, body_type) = self.infer(body)?;
                self.env.pop_scope();
                let func_type = Type::Func(
                    params_elab
                        .iter()
                        .map(|binding| binding.type_.clone())
                        .collect(),
                    box body_type.clone(),
                );
                Ok((
                    core::Expr::Abs {
                        params: params_elab,
                        return_type: body_type,
                        body: box body_elab,
                    },
                    func_type,
                ))
            }
            surface::Expr::Let {
                name,
                type_params,
                params,
                return_type,
                box body,
                box cont,
            } => {
                // infer let-bound expression
                let (value_elab, scheme) = {
                    self.level += 1;
                    let mut subst = HashMap::from_iter(
                        type_params
                            .iter()
                            .map(|type_param| (type_param.clone(), None)),
                    );
                    let mut params_elab = self.elab_params(&mut subst, params);
                    self.new_scope_with_params(params_elab.clone());
                    let mut body_elab;
                    let mut body_type;
                    if let Some(return_type) = return_type {
                        body_type = self.surface_type_to_core_type(&mut subst, return_type);
                        body_elab = self.check(body, body_type.clone())?;
                    } else {
                        (body_elab, body_type) = self.infer(body)?;
                    }
                    self.env.pop_scope();
                    self.level -= 1;

                    // generalize inferred or checked type
                    let mut skolems = SecondaryMap::new();
                    body_type = self.generalize(&mut skolems, body_type);
                    params_elab = params_elab
                        .iter()
                        .map(|core::Binding { name, type_ }| core::Binding {
                            name,
                            type_: self.generalize(&mut skolems, type_.clone()),
                        })
                        .collect();
                    body_elab = self.generalize_expr(&mut skolems, body_elab);

                    let mut type_params = Vec::new();
                    for (tvar_key, type_param) in skolems {
                        self.bindings.remove(tvar_key);
                        type_params.push(type_param);
                    }

                    if params_elab.is_empty() {
                        match body_type {
                            core::Type::Func(_, _) | _ if type_params.is_empty() => Ok((
                                body_elab,
                                Scheme {
                                    variables: type_params,
                                    type_: body_type,
                                },
                            )),
                            body_type => Err(TypeError::AmbiguousType(type_params, body_type)),
                        }
                    } else {
                        Ok((
                            core::Expr::Abs {
                                params: params_elab.clone(),
                                return_type: body_type.clone(),
                                body: box body_elab,
                            },
                            Scheme {
                                variables: type_params,
                                type_: Type::Func(
                                    params_elab
                                        .iter()
                                        .map(|param| param.type_.clone())
                                        .collect(),
                                    box body_type,
                                ),
                            },
                        ))
                    }
                }?;
                self.env.new_scope();
                self.env.insert(name, scheme.clone());
                let (cont_elab, cont_type) = self.infer(cont)?;
                self.env.pop_scope();
                Ok((
                    core::Expr::Let {
                        name,
                        type_params: scheme.variables,
                        type_: scheme.type_,
                        value: box value_elab,
                        cont: box cont_elab,
                    },
                    cont_type,
                ))
            }
            surface::Expr::Ann(box expr, type_) => {
                let expr_type = self.surface_type_to_core_type(&mut HashMap::new(), type_);
                let expr_elab = self.check(expr, expr_type.clone())?;
                Ok((expr_elab, expr_type))
            }
        }
    }

    fn check(
        &mut self,
        expr: surface::Expr<'src>,
        type_: Type<'src>,
    ) -> Result<core::Expr<'src>, TypeError<'src>> {
        match (expr, self.find(type_)) {
            (surface::Expr::Lit(value), Type::Int) => Ok(core::Expr::Lit(value)),
            (surface::Expr::Var(name), expected_type) => {
                if let Some(scheme) = self.env.lookup(name) {
                    let (var_type, type_args) = self.instantiate(scheme.clone());
                    self.unify(var_type, expected_type)?;
                    Ok(core::Expr::Var { name, type_args })
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
                let param_types: Vec<Type<'src>> =
                    args.iter().map(|_| Type::TVar(self.fresh_tvar())).collect();
                self.unify(
                    callee_type,
                    Type::Func(param_types.iter().cloned().collect(), box expected_type),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.iter().cloned().zip(param_types.iter().cloned()) {
                    let arg_elab = self.check(arg, param_type)?;
                    args_elab.push(arg_elab);
                }
                Ok(core::Expr::App {
                    callee: box callee_elab,
                    args: args_elab,
                })
            }
            (surface::Expr::Abs(mut params, body), Type::Func(mut param_types, return_type)) => {
                let param_count = params.len();
                let param_type_count = param_types.len();
                if param_count < param_type_count {
                    let remaining_param_types = param_types.split_off(param_count);
                    let mut params_typed = Vec::new();
                    for (param, param_type) in params.iter().zip(param_types) {
                        let binding = match param {
                            surface::Binding::Inferred(name) => Ok(core::Binding {
                                name,
                                type_: param_type,
                            }),
                            surface::Binding::Typed(name, expected_param_type) => {
                                let expected_param_type = self.surface_type_to_core_type(
                                    &mut HashMap::new(),
                                    expected_param_type.clone(),
                                );
                                self.unify(param_type.clone(), expected_param_type)?;
                                Ok(core::Binding {
                                    name,
                                    type_: param_type,
                                })
                            }
                        }?;
                        params_typed.push(binding);
                    }
                    self.new_scope_with_params(params_typed);
                    let expected_body_type = Type::Func(remaining_param_types, return_type);
                    let body_elab = self.check(*body, expected_body_type)?;
                    self.env.pop_scope();
                    Ok(body_elab)
                } else {
                    let remaining_params = params.split_off(param_type_count);
                    let mut params_typed = Vec::new();
                    for (param, param_type) in params.iter().zip(param_types) {
                        let binding = match param {
                            surface::Binding::Inferred(name) => Ok(core::Binding {
                                name,
                                type_: param_type,
                            }),
                            surface::Binding::Typed(name, expected_param_type) => {
                                let expected_param_type = self.surface_type_to_core_type(
                                    &mut HashMap::new(),
                                    expected_param_type.clone(),
                                );
                                self.unify(param_type.clone(), expected_param_type)?;
                                Ok(core::Binding {
                                    name,
                                    type_: param_type,
                                })
                            }
                        }?;
                        params_typed.push(binding);
                    }
                    self.new_scope_with_params(params_typed);
                    let body_elab =
                        self.check(surface::Expr::Abs(remaining_params, body), *return_type)?;
                    self.env.pop_scope();
                    Ok(body_elab)
                }
            }
            (
                surface::Expr::Let {
                    name,
                    type_params,
                    params,
                    return_type,
                    box body,
                    box cont,
                },
                expected_type,
            ) => {
                // infer let-bound expression
                let (value, scheme) = {
                    self.level += 1;
                    let mut subst = HashMap::from_iter(
                        type_params
                            .iter()
                            .map(|type_param| (type_param.clone(), None)),
                    );
                    let mut params_elab = self.elab_params(&mut subst, params);
                    self.new_scope_with_params(params_elab.clone());
                    let mut body_elab;
                    let mut body_type;
                    if let Some(return_type) = return_type {
                        body_type = self.surface_type_to_core_type(&mut subst, return_type);
                        body_elab = self.check(body, body_type.clone())?;
                    } else {
                        (body_elab, body_type) = self.infer(body)?;
                    }
                    self.env.pop_scope();
                    self.level -= 1;

                    // generalize inferred or checked type
                    let mut skolems = SecondaryMap::new();
                    body_type = self.generalize(&mut skolems, body_type);
                    params_elab = params_elab
                        .iter()
                        .map(|core::Binding { name, type_ }| core::Binding {
                            name,
                            type_: self.generalize(&mut skolems, type_.clone()),
                        })
                        .collect();
                    body_elab = self.generalize_expr(&mut skolems, body_elab);

                    let mut type_params = Vec::new();
                    for (tvar_key, type_param) in skolems {
                        self.bindings.remove(tvar_key);
                        type_params.push(type_param);
                    }

                    if params_elab.is_empty() {
                        match body_type {
                            core::Type::Func(_, _) | _ if type_params.is_empty() => Ok((
                                body_elab,
                                Scheme {
                                    variables: type_params,
                                    type_: body_type,
                                },
                            )),
                            body_type => {
                                Err(TypeError::AmbiguousType(type_params.clone(), body_type))
                            }
                        }
                    } else {
                        Ok((
                            core::Expr::Abs {
                                params: params_elab.clone(),
                                return_type: body_type.clone(),
                                body: box body_elab,
                            },
                            Scheme {
                                variables: type_params,
                                type_: Type::Func(
                                    params_elab
                                        .iter()
                                        .map(|param| param.type_.clone())
                                        .collect(),
                                    box body_type,
                                ),
                            },
                        ))
                    }
                }?;

                self.env.new_scope();
                self.env.insert(name, scheme.clone());
                let cont_elab = self.check(cont, expected_type)?;
                self.env.pop_scope();
                Ok(core::Expr::Let {
                    name,
                    type_params: scheme.variables,
                    type_: scheme.type_,
                    value: box value,
                    cont: box cont_elab,
                })
            }
            (surface::Expr::Ann(box expr, ann_type), expected_type) => {
                let ann_type = self.surface_type_to_core_type(&mut HashMap::new(), ann_type);
                self.unify(ann_type, expected_type.clone())?;
                let expr_elab = self.check(expr, expected_type)?;
                Ok(expr_elab)
            }
            (expr, expected_type) => {
                let (expr_elab, expr_type) = self.infer(expr)?;
                self.unify(expr_type, expected_type)?;
                Ok(expr_elab)
            }
        }
    }

    pub fn infer_expr(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Scheme<'src>), TypeError<'src>> {
        self.level += 1;
        let (mut expr_elab, mut expr_type) = self.infer(expr)?;
        self.level -= 1;
        let mut skolems = SecondaryMap::new();
        expr_elab = self.generalize_expr(&mut skolems, expr_elab);
        expr_type = self.generalize(&mut skolems, expr_type);

        let mut type_params = Vec::new();
        for (tvar_key, type_param) in skolems {
            self.bindings.remove(tvar_key);
            type_params.push(type_param);
        }

        let scheme = Scheme {
            variables: type_params,
            type_: expr_type,
        };
        Ok((expr_elab, scheme))
    }

    pub fn run(
        &mut self,
        module: surface::Module<'src>,
    ) -> Result<core::Module<'src>, TypeError<'src>> {
        let mut values = HashMap::new();
        for decl in module.declarations {
            match decl {
                surface::Decl::Func {
                    name,
                    type_params,
                    params,
                    return_type,
                    body,
                } => {
                    let decl_elab = {
                        self.level += 1;
                        let mut subst = HashMap::from_iter(
                            type_params
                                .iter()
                                .map(|type_param| (type_param.clone(), None)),
                        );
                        let mut params_elab = self.elab_params(&mut subst, params);
                        self.new_scope_with_params(params_elab.clone());
                        let (mut body_elab, mut body_type) = if let Some(return_type) = return_type
                        {
                            let body_type = self.surface_type_to_core_type(&mut subst, return_type);
                            let body_elab = self.check(body, body_type.clone())?;
                            Ok((body_elab, body_type))
                        } else {
                            self.infer(body)
                        }?;
                        self.env.pop_scope();
                        self.level -= 1;

                        let mut skolems = SecondaryMap::new();
                        body_type = self.generalize(&mut skolems, body_type);
                        params_elab = params_elab
                            .iter()
                            .map(|core::Binding { name, type_ }| core::Binding {
                                name,
                                type_: self.generalize(&mut skolems, type_.clone()),
                            })
                            .collect();
                        body_elab = self.generalize_expr(&mut skolems, body_elab);

                        let mut type_params = Vec::new();
                        for (tvar_key, type_param) in skolems {
                            self.bindings.remove(tvar_key);
                            type_params.push(type_param);
                        }

                        let (type_, value) = if params_elab.is_empty() {
                            match body_type {
                                core::Type::Func(_, _) => {
                                    Ok((body_type, body_elab))
                                }
                                _ if type_params.is_empty() => {
                                    Ok((body_type, body_elab))
                                }
                                body_type => {
                                    Err(TypeError::AmbiguousType(type_params.clone(), body_type))
                                }
                            }
                        } else {
                            Ok((
                                Type::Func(
                                    params_elab
                                        .iter()
                                        .map(|param| param.type_.clone())
                                        .collect(),
                                    box body_type.clone(),
                                ),
                                core::Expr::Abs {
                                    params: params_elab,
                                    return_type: body_type,
                                    body: box body_elab,
                                },
                            ))
                        }?;

                        Ok(core::ValueDecl {
                            name,
                            type_params,
                            type_,
                            value,
                        })
                    }?;

                    if values.contains_key(&name) {
                        Err(TypeError::AlreadyDefined(name))?
                    } else {
                        values.insert(name, decl_elab.clone());
                        self.env.insert(
                            name,
                            Scheme {
                                variables: decl_elab.type_params,
                                type_: decl_elab.type_,
                            },
                        );
                    }
                }
            }
            self.reset();
        }
        Ok(core::Module { values })
    }
}
