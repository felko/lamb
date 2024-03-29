use slotmap::{SecondaryMap, SlotMap};
use std::cmp::Ordering;
use std::collections::{hash_map, HashMap};

use crate::core::syntax as core;
use crate::env::Env;
use crate::surface;
use crate::tc;
use crate::tc::error::TypeError;
use crate::tc::prim::PRIMITIVES;
use crate::tc::syntax::{Scheme, TVar, TVarKey, Type};

type Level = u8;

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
            env: Env::new(
                PRIMITIVES
                    .iter()
                    .map(|(name, scheme)| (*name, Typechecker::scheme_core_to_tc(scheme)))
                    .collect(),
            ),
            level: 0,
            bindings: SlotMap::with_key(),
            tvar_supply: 0,
        }
    }

    fn scheme_core_to_tc(scheme: &core::Scheme<'src>) -> Scheme<'src> {
        let core::Scheme { variables, type_ } = scheme;
        Scheme {
            variables: variables.clone(),
            type_: Typechecker::type_core_to_tc(type_),
        }
    }

    fn type_core_to_tc(type_: &core::Type<'src>) -> Type<'src> {
        match type_ {
            core::Type::Name { name } => Type::Name { name },
            core::Type::QVar(name) => Type::QVar(name.clone()),
            core::Type::Func(param_types, box return_type) => Type::Func(
                param_types
                    .iter()
                    .map(Typechecker::type_core_to_tc)
                    .collect(),
                box Typechecker::type_core_to_tc(return_type),
            ),
            core::Type::Tuple(element_types) => tc::Type::Tuple(
                element_types
                    .iter()
                    .map(Typechecker::type_core_to_tc)
                    .collect(),
            ),
            core::Type::Int => Type::Int,
            core::Type::Bool => Type::Bool,
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
                    .map(|parameter_type| self.instantiate_type(subst, parameter_type.clone()))
                    .collect();
                let return_type = self.instantiate_type(subst, return_type);
                Type::Func(parameter_types, box return_type)
            }
            Type::Tuple(element_types) => Type::Tuple(
                element_types
                    .iter()
                    .map(|element_type| self.instantiate_type(subst, element_type.clone()))
                    .collect(),
            ),
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
            Type::Tuple(element_types) => Type::Tuple(
                element_types
                    .iter()
                    .map(|element_type| self.generalize(skolems, element_type.clone()))
                    .collect(),
            ),
        }
    }

    fn generalize_binding(
        &mut self,
        skolems: &mut SecondaryMap<TVarKey, String>,
        binding: tc::Binding<'src>,
    ) -> tc::Binding<'src> {
        tc::Binding {
            name: binding.name,
            type_: self.generalize(skolems, binding.type_),
        }
    }

    fn generalize_expr(
        &mut self,
        skolems: &mut SecondaryMap<TVarKey, String>,
        expr: tc::Expr<'src>,
    ) -> tc::Expr<'src> {
        match expr {
            tc::Expr::Lit(value) => tc::Expr::Lit(value),
            tc::Expr::Var { name, type_args, type_ } => tc::Expr::Var {
                name,
                type_args: type_args
                    .iter()
                    .map(|type_arg| self.generalize(skolems, type_arg.clone()))
                    .collect(),
                type_: self.generalize(skolems, type_),
            },
            tc::Expr::Add(box lhs, box rhs) => tc::Expr::Add(
                box self.generalize_expr(skolems, lhs),
                box self.generalize_expr(skolems, rhs),
            ),
            tc::Expr::App { box callee, args } => tc::Expr::App {
                callee: box self.generalize_expr(skolems, callee),
                args: args
                    .iter()
                    .cloned()
                    .map(|arg| self.generalize_expr(skolems, arg))
                    .collect(),
            },
            tc::Expr::Abs {
                params,
                return_type,
                box body,
            } => tc::Expr::Abs {
                params: params
                    .iter()
                    .cloned()
                    .map(|param| self.generalize_binding(skolems, param))
                    .collect(),
                return_type: self.generalize(skolems, return_type),
                body: box self.generalize_expr(skolems, body),
            },
            tc::Expr::Let {
                name,
                type_params,
                params,
                return_type,
                box body,
                box cont,
            } => tc::Expr::Let {
                name,
                type_params,
                params: params
                    .iter()
                    .cloned()
                    .map(|param| self.generalize_binding(skolems, param))
                    .collect(),
                return_type: self.generalize(skolems, return_type),
                body: box self.generalize_expr(skolems, body),
                cont: box self.generalize_expr(skolems, cont),
            },
            tc::Expr::If {
                box cond,
                return_type,
                box then,
                box else_,
            } => tc::Expr::If {
                cond: box self.generalize_expr(skolems, cond),
                return_type: self.generalize(skolems, return_type),
                then: box self.generalize_expr(skolems, then),
                else_: box self.generalize_expr(skolems, else_),
            },
            tc::Expr::Tuple(elements) => tc::Expr::Tuple(
                elements
                    .iter()
                    .map(|element| self.generalize_expr(skolems, element.clone()))
                    .collect(),
            ),
            tc::Expr::Proj(box tuple, index) => {
                tc::Expr::Proj(box self.generalize_expr(skolems, tuple), index)
            }
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
            Type::Tuple(element_types) => element_types
                .iter()
                .all(|element_type| self.occurs_check(tvar_key, element_type)),
        }
    }

    fn find_aux(&mut self, type_: Type<'src>) -> Type<'src> {
        match type_ {
            Type::TVar(tvar_key) => match &self.bindings[tvar_key] {
                TVar::Bound(parent) => {
                    let repr = self.find_aux(parent.clone());
                    self.bindings[tvar_key] = TVar::Bound(repr.clone());
                    repr
                }
                _ => type_,
            },
            type_ => type_,
        }
    }

    fn find(&mut self, type_: &mut Type<'src>) {
        let repr = self.find_aux(type_.clone());
        *type_ = repr;
    }

    fn unify(&mut self, lhs: &mut Type<'src>, rhs: &mut Type<'src>) -> Result<(), TypeError<'src>> {
        self.find(lhs);
        self.find(rhs);
        match (lhs, rhs) {
            (t, u) if t == u => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::QVar(a), Type::QVar(b)) if a == b => Ok(()),
            (Type::TVar(tvar_key), type_) | (type_, Type::TVar(tvar_key)) => {
                let tvar = self.bindings[*tvar_key].clone();
                match tvar {
                    TVar::Unbound { index, .. } => {
                        if self.occurs_check(*tvar_key, type_) {
                            self.bindings[*tvar_key] = TVar::Bound(type_.clone());
                            Ok(())
                        } else {
                            Err(TypeError::OccursCheckFailure(index, type_.clone()))
                        }
                    }
                    TVar::Bound(mut bound_type) => self.unify(&mut bound_type, type_),
                }
            }
            (Type::Func(params1, box ret1), Type::Func(params2, box ret2)) => {
                let mut arg_index = 0;
                let (arity1, arity2) = (params1.len(), params2.len());
                while arg_index < arity1 && arg_index < arity2 {
                    self.unify(&mut params1[arg_index], &mut params2[arg_index])?;
                    arg_index += 1;
                }
                match arity1.cmp(&arity2) {
                    Ordering::Equal => self.unify(ret1, ret2),
                    Ordering::Less => self.unify(
                        ret1,
                        &mut Type::Func(
                            params2.iter().skip(arg_index).cloned().collect(),
                            box ret2.clone(),
                        ),
                    ),
                    Ordering::Greater => self.unify(
                        &mut Type::Func(
                            params1.iter().skip(arg_index).cloned().collect(),
                            box ret1.clone(),
                        ),
                        ret2,
                    ),
                }
            }
            (lhs, rhs) => Err(TypeError::UnificationFailure(lhs.clone(), rhs.clone())),
        }
    }

    fn surface_type_to_tc_type(
        &mut self,
        type_params: &Vec<&'src str>,
        subst: &mut Instantiation<'src>,
        type_: surface::Type<'src>,
    ) -> Result<Type<'src>, TypeError<'src>> {
        match type_ {
            surface::Type::Name(name) => match subst.get_mut(&name) {
                Some(Some(tvar_key)) => Ok(Type::TVar(*tvar_key)),
                Some(tvar_ref) => {
                    let tvar_key2 = self.fresh_tvar();
                    *tvar_ref = Some(tvar_key2);
                    Ok(Type::TVar(tvar_key2))
                }
                None => {
                    if type_params.contains(&name) {
                        Ok(Type::QVar(name.to_owned()))
                    } else {
                        Ok(Type::Name { name })
                    }
                }
            },
            surface::Type::Func(params, box ret) => {
                let tc_param_types = params
                    .iter()
                    .cloned()
                    .map(|param_type| self.surface_type_to_tc_type(type_params, subst, param_type))
                    .collect::<Result<Vec<tc::Type<'src>>, TypeError<'src>>>()?;
                let tc_return_type = self.surface_type_to_tc_type(type_params, subst, ret)?;
                Ok(Type::Func(tc_param_types, box tc_return_type))
            }
            surface::Type::Tuple(elements) => {
                let tc_elements = elements
                    .iter()
                    .cloned()
                    .map(|element| self.surface_type_to_tc_type(type_params, subst, element))
                    .collect::<Result<Vec<tc::Type<'src>>, TypeError<'src>>>()?;
                Ok(Type::Tuple(tc_elements))
            }
            surface::Type::Int => Ok(Type::Int),
            surface::Type::Bool => Ok(Type::Bool),
        }
    }

    fn elab_param(
        &mut self,
        type_params: &Vec<&'src str>,
        subst: &mut Instantiation<'src>,
        param: surface::Binding<'src>,
    ) -> Result<tc::Binding<'src>, TypeError<'src>> {
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
                Ok(tc::Binding {
                    name,
                    type_: Type::TVar(param_type),
                })
            }
            surface::Binding::Typed(name, type_) => {
                let param_type = self.surface_type_to_tc_type(type_params, subst, type_)?;
                self.env.insert(
                    name,
                    Scheme {
                        variables: Vec::new(),
                        type_: param_type.clone(),
                    },
                );
                Ok(tc::Binding {
                    name,
                    type_: param_type,
                })
            }
        }
    }

    fn elab_params(
        &mut self,
        type_params: &Vec<&'src str>,
        subst: &mut Instantiation<'src>,
        params: Vec<surface::Binding<'src>>,
    ) -> Result<Vec<tc::Binding<'src>>, TypeError<'src>> {
        params
            .iter()
            .map(|param| self.elab_param(type_params, subst, param.clone()))
            .collect()
    }

    fn new_scope_with_params(&mut self, params: Vec<tc::Binding<'src>>) {
        self.env.new_scope();
        for tc::Binding { name, type_ } in params {
            self.env.insert(
                name,
                Scheme {
                    variables: Vec::new(),
                    type_: type_.clone(),
                },
            );
        }
    }

    fn infer_literal(literal: &surface::Literal) -> Type<'src> {
        match literal {
            surface::Literal::Int(_) => tc::Type::Int,
            surface::Literal::Bool(_) => tc::Type::Bool,
        }
    }

    fn infer(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(tc::Expr<'src>, Type<'src>), TypeError<'src>> {
        match expr {
            surface::Expr::Lit(literal) => Ok((
                tc::Expr::Lit(literal.clone()),
                Self::infer_literal(&literal),
            )),
            surface::Expr::Var(name) => {
                if let Some(scheme) = self.env.lookup(name) {
                    let (instantiated, type_args) = self.instantiate(scheme.clone());
                    Ok((tc::Expr::Var { name, type_args, type_: instantiated.clone() }, instantiated))
                } else {
                    Err(TypeError::ScopeError(name))
                }
            }
            surface::Expr::Add(box lhs, box rhs) => {
                let lhs_elab = self.check(lhs, &mut Type::Int)?;
                let rhs_elab = self.check(rhs, &mut Type::Int)?;
                Ok((tc::Expr::Add(box lhs_elab, box rhs_elab), Type::Int))
            }
            surface::Expr::App(box callee, args) => {
                let (callee_elab, mut callee_type) = self.infer(callee)?;
                let mut param_vars: Vec<TVarKey> = Vec::new();
                for _ in 0..args.len() {
                    param_vars.push(self.fresh_tvar());
                }
                let param_types = param_vars.iter().map(|var| Type::TVar(*var)).collect();
                let return_var = self.fresh_tvar();
                self.unify(
                    &mut callee_type,
                    &mut Type::Func(param_types, box Type::TVar(return_var)),
                )?;
                let mut args_elab = Vec::new();
                for (arg, param_type) in args.iter().zip(param_vars) {
                    let arg_elab = self.check(arg.clone(), &mut Type::TVar(param_type))?;
                    args_elab.push(arg_elab);
                }
                Ok((
                    tc::Expr::App {
                        callee: box callee_elab,
                        args: args_elab,
                    },
                    Type::TVar(return_var),
                ))
            }
            surface::Expr::Abs(params, box body) => {
                let mut subst = HashMap::new();
                let params_elab = self.elab_params(&Vec::new(), &mut subst, params.clone())?;
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
                    tc::Expr::Abs {
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
                let (type_params_gen, params_elab, body_type, body_elab) = {
                    self.level += 1;
                    let mut subst = HashMap::from_iter(
                        type_params.iter().map(|type_param| (*type_param, None)),
                    );
                    let mut params_elab =
                        self.elab_params(&type_params, &mut subst, params.clone())?;
                    self.new_scope_with_params(params_elab.clone());
                    let mut body_elab;
                    let mut body_type;
                    if let Some(return_type) = return_type {
                        body_type = self.surface_type_to_tc_type(
                            &type_params,
                            &mut subst,
                            return_type.clone(),
                        )?;
                        body_elab = self.check(body, &mut body_type)?;
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
                        .cloned()
                        .map(|param| self.generalize_binding(&mut skolems, param))
                        .collect();
                    body_elab = self.generalize_expr(&mut skolems, body_elab);

                    let mut type_params = Vec::new();
                    for (tvar_key, type_param) in skolems {
                        self.bindings.remove(tvar_key);
                        type_params.push(type_param);
                    }

                    if params_elab.is_empty() {
                        match body_type.clone() {
                            tc::Type::Func(_, _) => {}
                            _ if type_params.is_empty() => {}
                            body_type => {
                                Err(TypeError::AmbiguousType(type_params.clone(), body_type))?
                            }
                        }
                    }
                    Ok((type_params, params_elab, body_type, body_elab))
                }?;
                self.env.new_scope();
                let scheme = Scheme {
                    variables: type_params_gen.clone(),
                    type_: if params.is_empty() {
                        body_type.clone()
                    } else {
                        Type::Func(
                            params_elab
                                .iter()
                                .map(|param| param.type_.clone())
                                .collect(),
                            box body_type.clone(),
                        )
                    },
                };
                self.env.insert(name, scheme);
                let (cont_elab, cont_type) = self.infer(cont)?;
                self.env.pop_scope();
                Ok((
                    tc::Expr::Let {
                        name,
                        type_params: type_params_gen,
                        params: params_elab,
                        return_type: body_type,
                        body: box body_elab,
                        cont: box cont_elab,
                    },
                    cont_type,
                ))
            }
            surface::Expr::If {
                box cond,
                box then,
                box else_,
            } => {
                let cond_elab = self.check(cond, &mut Type::Bool)?;
                let (then_elab, mut then_type) = self.infer(then)?;
                let (else_elab, mut else_type) = self.infer(else_)?;
                let mut return_type = Type::TVar(self.fresh_tvar());
                self.unify(&mut then_type, &mut return_type)?;
                self.unify(&mut else_type, &mut return_type)?;
                Ok((
                    tc::Expr::If {
                        cond: box cond_elab,
                        return_type: return_type.clone(),
                        then: box then_elab,
                        else_: box else_elab,
                    },
                    return_type,
                ))
            }
            surface::Expr::Tuple(elements) => {
                let (elements_elab, elements_type) = elements
                    .iter()
                    .map(|element| self.infer(element.clone()))
                    .try_collect()
                    .map(|elements_elab_type: Vec<(tc::Expr<'src>, Type<'src>)>| {
                        elements_elab_type.into_iter().unzip()
                    })?;
                Ok((
                    tc::Expr::Tuple(elements_elab),
                    tc::Type::Tuple(elements_type),
                ))
            }
            surface::Expr::Proj(box tuple, index) => {
                let (tuple_elab, mut tuple_type) = self.infer(tuple)?;
                self.find(&mut tuple_type);
                match tuple_type {
                    Type::Tuple(element_types) => match element_types.get(index as usize) {
                        Some(element_type) => {
                            Ok((tc::Expr::Proj(box tuple_elab, index), element_type.clone()))
                        }
                        _ => Err(TypeError::ProjectionOutOfBounds(element_types, index)),
                    },
                    Type::TVar(_) => Err(TypeError::AmbiguousTuple),
                    _ => Err(TypeError::NotATuple(tuple_type)),
                }
            }
            surface::Expr::Ann(box expr, type_) => {
                let mut expr_type =
                    self.surface_type_to_tc_type(&Vec::new(), &mut HashMap::new(), type_.clone())?;
                let expr_elab = self.check(expr, &mut expr_type)?;
                Ok((expr_elab, expr_type))
            }
        }
    }

    fn check(
        &mut self,
        expr: surface::Expr<'src>,
        type_: &mut Type<'src>,
    ) -> Result<tc::Expr<'src>, TypeError<'src>> {
        self.find(type_);
        match (expr, type_) {
            (surface::Expr::Lit(literal @ surface::Literal::Int(_)), Type::Int) => {
                Ok(tc::Expr::Lit(literal))
            }
            (surface::Expr::Lit(literal @ surface::Literal::Bool(_)), Type::Bool) => {
                Ok(tc::Expr::Lit(literal))
            }
            (surface::Expr::Var(name), expected_type) => {
                if let Some(scheme) = self.env.lookup(name) {
                    let (mut var_type, type_args) = self.instantiate(scheme.clone());
                    self.unify(&mut var_type, expected_type)?;
                    Ok(tc::Expr::Var { name, type_args, type_: var_type })
                } else {
                    Err(TypeError::ScopeError(name))
                }
            }
            (surface::Expr::Add(box lhs, box rhs), Type::Int) => {
                let lhs_elab = self.check(lhs, &mut Type::Int)?;
                let rhs_elab = self.check(rhs, &mut Type::Int)?;
                Ok(tc::Expr::Add(box lhs_elab, box rhs_elab))
            }
            (surface::Expr::App(box callee, args), expected_type) => {
                let (callee_elab, mut callee_type) = self.infer(callee)?;
                let param_types: Vec<Type<'src>> =
                    args.iter().map(|_| Type::TVar(self.fresh_tvar())).collect();
                self.unify(
                    &mut callee_type,
                    &mut Type::Func(param_types.to_vec(), box expected_type.clone()),
                )?;
                let mut args_elab = Vec::new();
                for (arg, mut param_type) in args.iter().cloned().zip(param_types.iter().cloned()) {
                    let arg_elab = self.check(arg, &mut param_type)?;
                    args_elab.push(arg_elab);
                }
                Ok(tc::Expr::App {
                    callee: box callee_elab,
                    args: args_elab,
                })
            }
            (
                surface::Expr::Abs(mut params, box body),
                Type::Func(param_types, box return_type),
            ) => {
                let param_count = params.len();
                let param_type_count = param_types.len();
                let split_index = param_count.min(param_type_count);
                let (remaining_params, remaining_param_types) = (
                    params.split_off(split_index),
                    param_types.split_off(split_index),
                );
                let mut params_typed = Vec::new();
                for (param, param_type) in params.iter().zip(param_types) {
                    let binding = match param {
                        surface::Binding::Inferred(name) => Ok(tc::Binding {
                            name,
                            type_: param_type.clone(),
                        }),
                        surface::Binding::Typed(name, expected_param_type) => {
                            let mut expected_param_type = self.surface_type_to_tc_type(
                                &Vec::new(),
                                &mut HashMap::new(),
                                expected_param_type.clone(),
                            )?;
                            self.unify(param_type, &mut expected_param_type)?;
                            Ok(tc::Binding {
                                name,
                                type_: param_type.clone(),
                            })
                        }
                    }?;
                    params_typed.push(binding);
                }

                self.new_scope_with_params(params_typed.clone());
                let (remaining_body, mut remaining_return_type) =
                    match param_count.cmp(&param_type_count) {
                        Ordering::Less => (
                            body,
                            Type::Func(remaining_param_types, box return_type.clone()),
                        ),
                        Ordering::Equal => (body, return_type.clone()),
                        Ordering::Greater => (
                            surface::Expr::Abs(remaining_params, box body),
                            return_type.clone(),
                        ),
                    };
                let body_elab = self.check(remaining_body, &mut remaining_return_type)?;
                self.env.pop_scope();
                Ok(tc::Expr::Abs {
                    params: params_typed,
                    return_type: remaining_return_type,
                    body: box body_elab,
                })
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
                let (type_params_gen, params_elab, body_type, body_elab) = {
                    self.level += 1;
                    let mut subst = HashMap::from_iter(
                        type_params.iter().map(|type_param| (*type_param, None)),
                    );
                    let mut params_elab =
                        self.elab_params(&type_params, &mut subst, params.clone())?;
                    self.new_scope_with_params(params_elab.clone());
                    let mut body_elab;
                    let mut body_type;
                    if let Some(return_type) = return_type {
                        body_type = self.surface_type_to_tc_type(
                            &type_params,
                            &mut subst,
                            return_type.clone(),
                        )?;
                        body_elab = self.check(body, &mut body_type)?;
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
                        .cloned()
                        .map(|param| self.generalize_binding(&mut skolems, param))
                        .collect();
                    body_elab = self.generalize_expr(&mut skolems, body_elab);

                    let mut type_params = Vec::new();
                    for (tvar_key, type_param) in skolems {
                        self.bindings.remove(tvar_key);
                        type_params.push(type_param);
                    }

                    if params_elab.is_empty() {
                        match body_type.clone() {
                            tc::Type::Func(_, _) => {}
                            _ if type_params.is_empty() => {}
                            body_type => {
                                Err(TypeError::AmbiguousType(type_params.clone(), body_type))?
                            }
                        }
                    }
                    Ok((type_params, params_elab, body_type, body_elab))
                }?;
                self.env.new_scope();
                let scheme = Scheme {
                    variables: type_params_gen.clone(),
                    type_: if params.is_empty() {
                        body_type.clone()
                    } else {
                        Type::Func(
                            params_elab
                                .iter()
                                .map(|param| param.type_.clone())
                                .collect(),
                            box body_type.clone(),
                        )
                    },
                };
                self.env.insert(name, scheme);
                let cont_elab = self.check(cont, expected_type)?;
                self.env.pop_scope();
                Ok(tc::Expr::Let {
                    name,
                    type_params: type_params_gen,
                    params: params_elab,
                    return_type: body_type,
                    body: box body_elab,
                    cont: box cont_elab,
                })
            }
            (
                surface::Expr::If {
                    box cond,
                    box then,
                    box else_,
                },
                expected_type,
            ) => {
                let cond_elab = self.check(cond, &mut Type::Bool)?;
                let then_elab = self.check(then, expected_type)?;
                let else_elab = self.check(else_, expected_type)?;
                Ok(tc::Expr::If {
                    cond: box cond_elab,
                    return_type: expected_type.clone(),
                    then: box then_elab,
                    else_: box else_elab,
                })
            }
            (surface::Expr::Ann(box expr, ann_type), expected_type) => {
                let mut ann_type =
                    self.surface_type_to_tc_type(&Vec::new(), &mut HashMap::new(), ann_type)?;
                self.unify(&mut ann_type, expected_type)?;
                let expr_elab = self.check(expr, expected_type)?;
                Ok(expr_elab)
            }
            (expr, expected_type) => {
                let (expr_elab, mut expr_type) = self.infer(expr)?;
                self.unify(&mut expr_type, expected_type)?;
                Ok(expr_elab)
            }
        }
    }

    fn check_module(
        &mut self,
        module: surface::Module<'src>,
    ) -> Result<tc::Module<'src>, TypeError<'src>> {
        let mut functions = HashMap::new();
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
                        let mut subst = HashMap::new();
                        let mut params_elab = self.elab_params(&type_params, &mut subst, params)?;
                        self.new_scope_with_params(params_elab.clone());
                        let (mut body_elab, mut return_type) =
                            if let Some(return_type) = return_type {
                                let mut return_type = self.surface_type_to_tc_type(
                                    &type_params,
                                    &mut subst,
                                    return_type,
                                )?;
                                let body_elab = self.check(body, &mut return_type)?;
                                Ok((body_elab, return_type))
                            } else {
                                self.infer(body)
                            }?;
                        self.env.pop_scope();
                        self.level -= 1;

                        let mut skolems = SecondaryMap::new();
                        return_type = self.generalize(&mut skolems, return_type);
                        params_elab = params_elab
                            .iter()
                            .map(|tc::Binding { name, type_ }| tc::Binding {
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
                            match return_type.clone() {
                                tc::Type::Func(_, _) => {}
                                _ if type_params.is_empty() => {}
                                return_type => {
                                    Err(TypeError::AmbiguousType(type_params.clone(), return_type))?
                                }
                            }
                        }
                        Ok(tc::FunDecl {
                            name,
                            type_params,
                            params: params_elab,
                            return_type,
                            body: body_elab,
                        })
                    }?;

                    if let hash_map::Entry::Vacant(e) = functions.entry(name) {
                        e.insert(decl_elab.clone());
                        self.env.insert(
                            name,
                            Scheme {
                                variables: decl_elab.type_params,
                                type_: Type::Func(
                                    decl_elab
                                        .params
                                        .iter()
                                        .map(|param| param.type_.clone())
                                        .collect(),
                                    box decl_elab.return_type,
                                ),
                            },
                        );
                    } else {
                        Err(TypeError::AlreadyDefined(name))?
                    }
                }
            }
            self.reset();
        }
        Ok(tc::Module { functions })
    }

    fn type_tc_to_core(&self, type_: Type<'src>) -> core::Type<'src> {
        match type_ {
            Type::Name { name } => core::Type::Name { name },
            Type::QVar(name) => core::Type::QVar(name),
            Type::TVar(tvar_key) => match &self.bindings[tvar_key] {
                TVar::Unbound { .. } => unreachable!("internal error"),
                TVar::Bound(type_) => self.type_tc_to_core(type_.clone()),
            },
            Type::Func(param_types, box return_type) => core::Type::Func(
                param_types
                    .iter()
                    .map(|param_type| self.type_tc_to_core(param_type.clone()))
                    .collect(),
                box self.type_tc_to_core(return_type),
            ),
            Type::Tuple(element_types) => core::Type::Tuple(
                element_types
                    .iter()
                    .map(|element_type| self.type_tc_to_core(element_type.clone()))
                    .collect(),
            ),
            Type::Int => core::Type::Int,
            Type::Bool => core::Type::Bool,
        }
    }

    fn binding_tc_to_core(&self, binding: tc::Binding<'src>) -> core::Binding<'src> {
        core::Binding {
            name: binding.name.to_owned(),
            type_: self.type_tc_to_core(binding.type_),
        }
    }

    fn expr_tc_to_core(&self, expr: tc::Expr<'src>) -> core::Expr<'src> {
        match expr {
            tc::Expr::Lit(literal) => core::Expr::Lit(literal),
            tc::Expr::Var { name, type_args, type_ } => core::Expr::Var {
                name,
                type_args: type_args
                    .iter()
                    .map(|type_arg| self.type_tc_to_core(type_arg.clone()))
                    .collect(),
                type_: self.type_tc_to_core(type_.clone())
            },
            tc::Expr::Abs {
                params,
                return_type,
                box body,
            } => core::Expr::Abs {
                params: params
                    .iter()
                    .map(|param| self.binding_tc_to_core(param.clone()))
                    .collect(),
                return_type: self.type_tc_to_core(return_type),
                body: box self.expr_tc_to_core(body),
            },
            tc::Expr::Add(box lhs, box rhs) => {
                core::Expr::Add(box self.expr_tc_to_core(lhs), box self.expr_tc_to_core(rhs))
            }
            tc::Expr::Let {
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
                    .map(|param| self.binding_tc_to_core(param))
                    .collect(),
                return_type: self.type_tc_to_core(return_type),
                body: box self.expr_tc_to_core(body),
                cont: box self.expr_tc_to_core(cont),
            },
            tc::Expr::If {
                box cond,
                return_type,
                box then,
                box else_,
            } => core::Expr::If {
                cond: box self.expr_tc_to_core(cond),
                return_type: self.type_tc_to_core(return_type),
                then: box self.expr_tc_to_core(then),
                else_: box self.expr_tc_to_core(else_),
            },
            tc::Expr::App { box callee, args } => core::Expr::App {
                callee: box self.expr_tc_to_core(callee),
                args: args
                    .iter()
                    .map(|arg| self.expr_tc_to_core(arg.clone()))
                    .collect(),
            },
            tc::Expr::Tuple(elements) => core::Expr::Tuple(
                elements
                    .iter()
                    .map(|element| self.expr_tc_to_core(element.clone()))
                    .collect(),
            ),
            tc::Expr::Proj(box tuple, index) => {
                core::Expr::Proj(box self.expr_tc_to_core(tuple), index)
            }
        }
    }

    fn decl_tc_to_core(&self, decl: tc::FunDecl<'src>) -> core::FunDecl<'src> {
        core::FunDecl {
            name: decl.name,
            type_params: decl.type_params,
            params: decl
                .params
                .iter()
                .map(|param| self.binding_tc_to_core(param.clone()))
                .collect(),
            return_type: self.type_tc_to_core(decl.return_type),
            body: self.expr_tc_to_core(decl.body),
        }
    }

    fn module_tc_to_core(&self, module: tc::Module<'src>) -> core::Module<'src> {
        core::Module {
            functions: HashMap::from_iter(
                module
                    .functions
                    .iter()
                    .map(|(name, decl)| (*name, self.decl_tc_to_core(decl.clone()))),
            ),
        }
    }

    pub fn run(
        &mut self,
        surface_module: surface::Module<'src>,
    ) -> Result<core::Module<'src>, TypeError<'src>> {
        let module = self.check_module(surface_module)?;
        Ok(self.module_tc_to_core(module))
    }
}
