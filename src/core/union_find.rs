#![allow(dead_code, unused_variables)]

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::core;
use crate::core::syntax::{Scheme, TVar, Type, UVar};
use crate::surface;

type Level = u8;

#[derive(Debug)]
pub enum TypeError<'src> {
    ScopeError(&'src str),
    OccursCheckFailure(core::TVar<'src>, Type<'src>),
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

pub struct Typechecker<'src> {
    pub definitions: HashMap<&'src str, Scheme<'src>>,
    pub environment: Environment<&'src str, Scheme<'src>>,
    pub level: Level,
    pub uvar_supply: UVar,
}

impl<'src> Typechecker<'src> {
    pub fn new() -> Typechecker<'src> {
        Typechecker {
            definitions: HashMap::new(),
            environment: Environment::new(),
            level: 0,
            uvar_supply: 0,
        }
    }

    pub fn reset(&mut self) {
        self.environment.clear();
        self.level = 0;
        self.uvar_supply = 0;
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

    fn instantiate_type(&mut self, variables: &HashSet<String>, type_: Type<'src>) -> Type<'src> {
        match type_ {
            Type::Name { name } => Type::Name { name },
            Type::QVar(name) => {
                if variables.contains(&name) {
                    Type::TVar(box self.fresh_tvar())
                } else {
                    Type::QVar(name)
                }
            }
            Type::TVar(var) => Type::TVar(var),
            Type::Func(parameter_types, box return_type) => {
                let parameter_types = parameter_types
                    .iter()
                    .map(|parameter_type| self.instantiate_type(variables, parameter_type.clone()))
                    .collect();
                let return_type = self.instantiate_type(variables, return_type);
                Type::Func(parameter_types, box return_type)
            }
            Type::Int => Type::Int,
        }
    }

    pub fn instantiate(&mut self, scheme: Scheme<'src>) -> Type<'src> {
        let Scheme { variables, type_ } = scheme;
        let variable_set = HashSet::from_iter(variables.into_iter());
        self.instantiate_type(&variable_set, type_)
    }

    pub fn occurs_check(&self, var: TVar<'src>, type_: Type<'src>) -> Result<(), TypeError<'src>> {
        unimplemented!()
    }

    pub fn unify(&self, lhs: Type<'src>, rhs: Type<'src>) -> Result<(), TypeError<'src>> {
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::QVar(a), Type::QVar(b)) if a == b => Ok(()),
            (Type::TVar(mut var @ box TVar::Unbound { .. }), type_)
            | (type_, Type::TVar(mut var @ box TVar::Unbound { .. })) => {
                self.occurs_check(*var, type_.clone())?;
                *var = TVar::Bound(type_);
                Ok(())
            }
            (Type::TVar(box TVar::Bound(lhs)), rhs) => self.unify(lhs, rhs),
            (lhs, Type::TVar(box TVar::Bound(rhs))) => self.unify(lhs, rhs),
            (Type::Func(params1, ret1), Type::Func(params2, ret2)) => {
                let mut arg_index = 0;
                let (arity1, arity2) = (params1.len(), params2.len());
                while arg_index < arity1 && arg_index < arity2 {
                    self.unify(params1[arg_index].clone(), params2[arg_index].clone())?;
                    arg_index += 1;
                }
                match arity1.cmp(&arity2) {
                    Ordering::Equal => self.unify(*ret1, *ret2),
                    Ordering::Less => self.unify(
                        *ret1,
                        Type::Func(params2.iter().cloned().skip(arg_index).collect(), ret2),
                    ),
                    Ordering::Greater => self.unify(
                        Type::Func(params1.iter().cloned().skip(arg_index).collect(), ret1),
                        *ret2,
                    ),
                }
            }
            (lhs, rhs) => Err(TypeError::UnificationFailure(lhs, rhs)),
        }
    }

    pub fn surface_type_to_core_type(&self, type_: surface::Type<'src>) -> Type<'src> {
        match type_ {
            surface::Type::Name(name) => Type::Name {name},
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
        }
    }

    pub fn infer(
        &mut self,
        expr: surface::Expr<'src>,
    ) -> Result<(core::Expr<'src>, Type<'src>), TypeError<'src>> {
        match expr {
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
                panic!()
            }
            surface::Expr::Abs(params, body) => {
                self.environment.new_scope();
                for binding in params {
                    match binding {
                        surface::Binding::Inferred(name) => {
                            let scheme = Scheme {variables: Vec::new(), type_: Type::TVar(box self.fresh_tvar())};
                            self.environment.insert(name, scheme);
                        },
                        surface::Binding::Typed(name, type_) => {
                            let scheme = Scheme {variables: Vec::new(), type_: self.surface_type_to_core_type(type_)};
                            self.environment.insert(name, scheme);
                        },
                    }
                }
                panic!()
            }
            surface::Expr::Let(_, _, _, _, _) => panic!(),
        }
    }

    pub fn check(
        &mut self,
        expr: surface::Expr<'src>,
        type_: Type<'src>,
    ) -> Result<core::Expr<'src>, TypeError<'src>> {
        match (expr, type_) {
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
                panic!()
            }
            (surface::Expr::Abs(params, _), expected_type) => panic!(),
            (surface::Expr::Let(_, _, _, _, _), expected_type) => panic!(),
            (expr, expected_type) => {
                let (expr_elab, expr_type) = self.infer(expr)?;
                self.unify(expr_type, expected_type)?;
                Ok(expr_elab)
            }
        }
    }
}

// #[derive(PartialEq, Eq)]
// struct Ranked<'src> {
//     rank: u8,
//     value: Option<Type<'src>>,
// }

// impl<'src> PartialOrd for Ranked<'src> {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.rank.cmp(&other.rank))
//     }
// }

// impl<'src> Ord for Ranked<'src> {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.rank.cmp(&other.rank)
//     }
// }

// pub struct Unifier<'src> {
//     uvar_supply: UVar,
//     bindings: HashMap<UVar, Ranked<'src>>,
// }

// impl<'src> Unifier<'src> {
//     fn lookup_var(&self, var: UVar) -> Option<&Type<'src, UVar>> {
//         self.bindings.get(&var).and_then(|ranked| ranked.value.as_ref())
//     }

//     fn fresh_var(&mut self) -> UVar {
//         let var = self.uvar_supply;
//         self.uvar_supply += 1;
//         var
//     }

//     fn new_var(&mut self, value: Type<'src, UVar>) -> UVar {
//         let var = self.fresh_var();
//         self.bindings.insert(var, Ranked { rank: 0, value: Some(value) });
//         var
//     }

//     fn bind_var(&mut self, var: UVar, value: Type<'src, UVar>) {
//         let r: Option<&Ranked<'src>> = self.bindings.get(&var);
//         match r {
//             Some(Ranked {rank, ..}) => {
//                 let new_rank = *rank;
//                 self.bindings.insert(var, Ranked { rank: new_rank, value: Some(value) });
//             }
//             _ => {
//                 self.new_var(value);
//             },
//         }
//     }

//     fn unify_var(&mut self, v1: UVar, v2: UVar) -> Result<Type<'src, UVar>, UnificationError<'src>> {
//         let (r1, r2) = (self.bindings[&v1], self.bindings[&v2]);
//         panic!();
//     }

//     fn unify_types(
//         &mut self,
//         lhs: Type<'src, UVar>,
//         rhs: Type<'src, UVar>,
//     ) -> Result<Type<'src, UVar>, UnificationError<'src>> {
//         match (lhs, rhs) {
//             (Var(x), Var(y)) if x == y => Ok(Var(x)),
//             (UVar(i), UVar(j)) => self.unify_var(i, j),
//             (lhs, rhs) => Err(UnificationError::UnificationFailure(lhs, rhs)),
//         }
//     }
// }
