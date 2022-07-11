use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::anf::syntax::*;

pub struct ClosureConverter {
    var_supply: Rc<Cell<usize>>,
}

type Env<'src> = im::hashmap::HashMap<String, Scheme<'src>>;

impl ClosureConverter {
    pub fn new() -> ClosureConverter {
        ClosureConverter {
            var_supply: Rc::new(Cell::new(0)),
        }
    }

    fn fresh(&self, prefix: &str) -> String {
        let mut name = "@".to_owned();
        name.push_str(prefix);
        name.push_str((*self.var_supply).get().to_string().as_ref());
        self.var_supply.set((*self.var_supply).get() + 1);
        name
    }

    fn substitute_var<'src, 'a>(
        &self,
        env_name: &str,
        subst: &HashMap<String, (u8, Type<'src>)>,
        var: String,
        cont: Box<dyn FnOnce(String) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        if let Some((index, type_)) = subst.get(&var) {
            let c = self.fresh("c");
            Expr::LetProj {
                name: c.clone(),
                type_: type_.clone(),
                tuple: Value::Var {
                    name: env_name.to_string(),
                    type_args: Vec::new(),
                },
                index: *index,
                cont: box cont(c),
            }
        } else {
            cont(var)
        }
    }

    fn substitute_value<'src, 'a>(
        &self,
        env_name: &str,
        subst: &HashMap<String, (u8, Type<'src>)>,
        value: Value<'src>,
        cont: Box<dyn FnOnce(Value<'src>) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        if let Value::Var { name, .. } = value.clone() {
            self.substitute_var(env_name, subst, name, box move |var| {
                cont(Value::Var {
                    name: var,
                    type_args: Vec::new(),
                })
            })
        } else {
            cont(value)
        }
    }

    fn substitute_values_aux<'src, 'a>(
        &self,
        env_name: &str,
        subst: &HashMap<String, (u8, Type<'src>)>,
        values: &Vec<Value<'src>>,
        index: usize,
        accum: Vec<Value<'src>>,
        cont: Box<dyn FnOnce(Vec<Value<'src>>) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        if index >= values.len() {
            cont(accum)
        } else {
            self.substitute_value(env_name, subst, values[index].clone(), box move |value| {
                let mut next_accum = accum.clone();
                next_accum.push(value);
                self.substitute_values_aux(env_name, subst, values, index + 1, next_accum, cont)
            })
        }
    }

    fn substitute_values<'src, 'a>(
        &self,
        env_name: &str,
        subst: &HashMap<String, (u8, Type<'src>)>,
        values: &Vec<Value<'src>>,
        cont: Box<dyn FnOnce(Vec<Value<'src>>) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        let accum = Vec::new();
        self.substitute_values_aux(env_name, subst, values, 0, accum, cont)
    }

    fn substitute_free_variables<'src>(
        &self,
        env_name: &str,
        subst: &HashMap<String, (u8, Type<'src>)>,
        expr: &mut Expr<'src>,
    ) {
        match expr {
            Expr::Halt { value } => {
                *expr = self.substitute_value(env_name, subst, value.clone(), box move |value| {
                    Expr::Halt { value }
                });
            }
            Expr::Jump { name, args } => {
                *expr = self.substitute_values(env_name, subst, args, box move |args| Expr::Jump {
                    name: name.clone(),
                    args,
                })
            }
            Expr::LetJoin {
                name,
                params,
                return_type,
                body,
                cont,
            } => {
                self.substitute_free_variables(env_name, subst, body);
                self.substitute_free_variables(env_name, subst, cont);
            }
            Expr::LetFun {
                name,
                type_params,
                params,
                return_type,
                body,
                cont,
            } => {
                self.substitute_free_variables(env_name, subst, body);
                self.substitute_free_variables(env_name, subst, cont);
            }
            Expr::LetAdd {
                name,
                lhs,
                rhs,
                cont,
            } => {
                *expr = self.substitute_value(env_name, subst, lhs.clone(), box move |lhs| {
                    self.substitute_value(env_name, subst, rhs.clone(), box move |rhs| {
                        self.substitute_free_variables(env_name, subst, cont);
                        Expr::LetAdd {
                            name: name.clone(),
                            lhs,
                            rhs,
                            cont: cont.clone(),
                        }
                    })
                });
            }
            Expr::LetVal {
                name,
                type_,
                value,
                cont,
            } => {
                *expr = self.substitute_value(env_name, subst, value.clone(), box move |value| {
                    self.substitute_free_variables(env_name, subst, cont);
                    Expr::LetVal {
                        name: name.clone(),
                        type_: type_.clone(),
                        value,
                        cont: cont.clone(),
                    }
                });
            }
            Expr::LetApp {
                name,
                type_,
                callee,
                type_args,
                args,
                cont,
            } => {
                *expr = self.substitute_var(env_name, subst, callee.clone(), box move |callee| {
                    self.substitute_values(env_name, subst, args, box move |args| {
                        self.substitute_free_variables(env_name, subst, cont);
                        Expr::LetApp {
                            name: name.clone(),
                            type_: type_.clone(),
                            callee,
                            type_args: type_args.clone(),
                            args,
                            cont: cont.clone(),
                        }
                    })
                });
            }
            Expr::LetTuple {
                name,
                types,
                elements,
                cont,
            } => {
                *expr = self.substitute_values(env_name, subst, elements, box move |elements| {
                    self.substitute_free_variables(env_name, subst, cont);
                    Expr::LetTuple {
                        name: name.clone(),
                        types: types.clone(),
                        elements,
                        cont: cont.clone(),
                    }
                });
            }
            Expr::LetProj {
                name,
                type_,
                tuple,
                index,
                cont,
            } => {
                *expr = self.substitute_value(env_name, subst, tuple.clone(), box move |tuple| {
                    self.substitute_free_variables(env_name, subst, cont);
                    Expr::LetProj {
                        name: name.clone(),
                        type_: type_.clone(),
                        tuple,
                        index: *index,
                        cont: cont.clone(),
                    }
                })
            }
            Expr::If {
                cond,
                return_type,
                then,
                else_,
            } => {
                self.substitute_value(env_name, subst, cond.clone(), box move |cond| {
                    self.substitute_free_variables(env_name, subst, then);
                    self.substitute_free_variables(env_name, subst, else_);
                    Expr::If {
                        cond,
                        return_type: return_type.clone(),
                        then: then.clone(),
                        else_: else_.clone(),
                    }
                });
            }
        }
    }

    fn convert_expr<'src>(&self, mut env: Env<'src>, expr: &mut Expr<'src>) {
        match expr {
            Expr::Halt { .. } => {}
            Expr::Jump { name, args } => {}
            Expr::LetJoin {
                name,
                params,
                return_type,
                body,
                cont,
            } => {
                let body_env = {
                    let mut env = env.clone();
                    params.iter().for_each(|param| {
                        env.insert(
                            param.name.clone(),
                            Scheme {
                                variables: Vec::new(),
                                type_: param.type_.clone(),
                            },
                        );
                    });
                    env
                };
                self.convert_expr(body_env, body);
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: Type::Func(
                            params.iter().map(|param| param.type_.clone()).collect(),
                            box return_type.clone(),
                        ),
                    },
                );
                self.convert_expr(env, cont);
            }
            Expr::LetFun {
                name,
                type_params,
                params,
                return_type,
                body,
                cont,
            } => {
                let body_env = {
                    let mut env = env.clone();
                    params.iter().for_each(|param| {
                        env.insert(
                            param.name.clone(),
                            Scheme {
                                variables: Vec::new(),
                                type_: param.type_.clone(),
                            },
                        );
                    });
                    env
                };
                self.convert_expr(body_env.clone(), body);
                let env_name = self.fresh("env");
                let mut fvs = HashMap::new();
                free_variables_expr(body_env, &mut fvs, body);
                for param in params.clone() {
                    fvs.remove(&param.name);
                }
                let mut closure = vec![Value::Var {
                    name: name.clone(),
                    type_args: Vec::new(),
                }];
                let callee_type = Type::Func(
                    params.iter().map(|param| param.type_.clone()).collect(),
                    box return_type.clone(),
                );
                let mut subst = HashMap::new();
                let mut closure_types = vec![callee_type.clone()];
                for (i, (fv, type_)) in fvs.iter().enumerate() {
                    subst.insert(fv.clone(), ((i + 1) as u8, type_.clone()));
                    closure.push(Value::Var {
                        name: fv.clone(),
                        type_args: Vec::new(),
                    });
                    closure_types.push(type_.clone());
                }
                self.substitute_free_variables(&env_name, &subst, body);
                params.push(Binding {
                    name: env_name.clone(),
                    type_: Type::Tuple(closure_types.clone()),
                });
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: type_params.clone(),
                        type_: callee_type,
                    },
                );
                self.convert_expr(env, cont);
                *cont = box Expr::LetTuple {
                    name: name.clone(),
                    types: closure_types,
                    elements: closure,
                    cont: cont.clone(),
                };
            }
            Expr::LetAdd {
                name,
                lhs,
                rhs,
                cont,
            } => {
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: Type::Int,
                    },
                );
                self.convert_expr(env, cont);
            }
            Expr::LetVal {
                name,
                type_,
                value,
                cont,
            } => {
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: type_.clone(),
                    },
                );
                self.convert_expr(env, cont);
            }
            Expr::LetApp {
                name,
                type_,
                callee,
                type_args,
                args,
                cont,
            } => {
                let f = self.fresh("f");
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: type_.clone(),
                    },
                );
                self.convert_expr(env.clone(), cont);
                let closure = Value::Var {
                    name: callee.clone(),
                    type_args: type_args.clone(),
                };
                let callee_type = infer_value(
                    &env,
                    &Value::Var {
                        name: callee.clone(),
                        type_args: type_args.clone(),
                    },
                );
                *expr = Expr::LetProj {
                    name: f.clone(),
                    type_: callee_type,
                    tuple: closure,
                    index: 0,
                    cont: box Expr::LetApp {
                        name: name.clone(),
                        type_: type_.clone(),
                        type_args: Vec::new(),
                        callee: f,
                        args: args.clone(),
                        cont: cont.clone(),
                    },
                };
            }
            Expr::LetTuple {
                name,
                types,
                elements,
                cont,
            } => {
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: Type::Tuple(types.clone()),
                    },
                );
                self.convert_expr(env, cont);
            }
            Expr::LetProj {
                name,
                type_,
                tuple,
                index,
                cont,
            } => {
                env.insert(
                    name.clone(),
                    Scheme {
                        variables: Vec::new(),
                        type_: type_.clone(),
                    },
                );
                self.convert_expr(env, cont);
            }
            Expr::If {
                cond,
                return_type,
                then,
                else_,
            } => {
                self.convert_expr(env.clone(), then);
                self.convert_expr(env, else_);
            }
        }
    }

    fn convert_decl<'src>(&self, mut env: Env<'src>, decl: &mut FunDecl<'src>) {
        decl.params.iter().for_each(|param| {
            env.insert(
                param.name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: param.type_.clone(),
                },
            );
        });
        self.convert_expr(env, &mut decl.body);
    }

    pub fn convert_module<'src>(&self, module: &mut Module<'src>) {
        let env = im::hashmap::HashMap::from_iter(module.functions.values().map(|decl| {
            let type_ = if decl.params.is_empty() {
                decl.return_type.clone()
            } else {
                Type::Func(
                    decl.params
                        .iter()
                        .map(|param| param.type_.clone())
                        .collect(),
                    box decl.return_type.clone(),
                )
            };
            (
                decl.name.to_owned(),
                Scheme {
                    variables: decl.type_params.clone(),
                    type_,
                },
            )
        }));
        module
            .functions
            .values_mut()
            .for_each(|decl| self.convert_decl(env.clone(), decl));
    }
}

fn specialize_type<'src>(subst: &HashMap<String, Type<'src>>, type_: &Type<'src>) -> Type<'src> {
    use Type::*;
    match type_ {
        Name { name } => Name { name },
        QVar(name) => match subst.get(name) {
            Some(type_) => type_.clone(),
            None => QVar(name.clone()),
        },
        Func(parameter_types, box return_type) => {
            let parameter_types = parameter_types
                .iter()
                .map(|parameter_type| specialize_type(subst, parameter_type))
                .collect();
            let return_type = specialize_type(subst, return_type);
            Func(parameter_types, box return_type)
        }
        Tuple(element_types) => Tuple(
            element_types
                .iter()
                .map(|element_type| specialize_type(subst, element_type))
                .collect(),
        ),
        Int => Int,
        Bool => Bool,
    }
}

fn specialize<'src>(scheme: &Scheme<'src>, type_args: &[Type<'src>]) -> Type<'src> {
    let Scheme { variables, type_ } = scheme;
    let subst = variables
        .iter()
        .cloned()
        .zip(type_args.iter().cloned())
        .collect();
    specialize_type(&subst, type_)
}

fn infer_value<'src>(env: &Env<'src>, value: &Value<'src>) -> Type<'src> {
    match value {
        Value::Var { name, type_args } => {
            let scheme = env
                .get(name)
                .unwrap_or_else(|| panic!("internal error: undefined variable {name}"));
            specialize(scheme, type_args)
        }
        Value::Lit(Literal::Int(_)) => Type::Int,
        Value::Lit(Literal::Bool(_)) => Type::Bool,
    }
}

fn free_variables_value<'src>(
    env: Env<'src>,
    fvs: &mut HashMap<String, Type<'src>>,
    value: &Value<'src>,
) {
    match value {
        Value::Var { name, type_args } => {
            if let Some(scheme) = env.get(name) {
                fvs.insert(name.clone(), specialize(scheme, type_args.as_ref()));
            } else {
                panic!("internal error: undefined variable {name}");
            }
        }
        Value::Lit(_) => {}
    }
}

// TODO: use immutable hashmap for fvs or use unique names
fn free_variables_expr<'src>(
    mut env: Env<'src>,
    fvs: &mut HashMap<String, Type<'src>>,
    expr: &Expr<'src>,
) {
    match expr {
        Expr::Halt { value } => free_variables_value(env, fvs, value),
        Expr::Jump { name, args } => {
            args.iter()
                .for_each(|arg| free_variables_value(env.clone(), fvs, arg));
            if let Some(Scheme {
                type_: Type::Func(_, box return_type),
                ..
            }) = env.get(name)
            {
                fvs.insert(name.clone(), return_type.clone());
            } else {
                panic!("internal error: invalid join point");
            }
        }
        Expr::LetJoin {
            name,
            params,
            return_type,
            body,
            cont,
        } => {
            let body_env = {
                let mut env = env.clone();
                params.iter().for_each(|param| {
                    env.insert(
                        param.name.clone(),
                        Scheme {
                            variables: Vec::new(),
                            type_: param.type_.clone(),
                        },
                    );
                });
                env
            };
            free_variables_expr(body_env, fvs, body);
            let join_type = if params.is_empty() {
                return_type.clone()
            } else {
                Type::Func(
                    params.iter().map(|param| param.type_.clone()).collect(),
                    box return_type.clone(),
                )
            };
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: join_type,
                },
            );
            free_variables_expr(env, fvs, cont);
            params.iter().for_each(|Binding { name, .. }| {
                fvs.remove(name);
            });
            fvs.remove(name);
        }
        Expr::LetFun {
            name,
            type_params,
            params,
            return_type,
            body,
            cont,
        } => {
            let body_env = {
                let mut env = env.clone();
                params.iter().for_each(|param| {
                    env.insert(
                        param.name.clone(),
                        Scheme {
                            variables: Vec::new(),
                            type_: param.type_.clone(),
                        },
                    );
                });
                env
            };
            free_variables_expr(body_env, fvs, body);
            params.iter().for_each(|param| {
                fvs.remove(&param.name);
            });
            env.insert(
                name.clone(),
                Scheme {
                    variables: type_params.clone(),
                    type_: Type::Func(
                        params.iter().map(|param| param.type_.clone()).collect(),
                        box return_type.clone(),
                    ),
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::LetAdd {
            name,
            lhs,
            rhs,
            cont,
        } => {
            free_variables_value(env.clone(), fvs, lhs);
            free_variables_value(env.clone(), fvs, rhs);
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: Type::Int,
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::LetVal {
            name,
            type_,
            value,
            cont,
        } => {
            free_variables_value(env.clone(), fvs, value);
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: type_.clone(),
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::LetApp {
            name,
            type_,
            callee,
            type_args,
            args,
            cont,
        } => {
            fvs.insert(callee.clone(), type_.clone());
            args.iter()
                .for_each(|arg| free_variables_value(env.clone(), fvs, arg));
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: type_.clone(),
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::LetTuple {
            name,
            types,
            elements,
            cont,
        } => {
            elements
                .iter()
                .for_each(|element| free_variables_value(env.clone(), fvs, element));
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: Type::Tuple(types.clone()),
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::LetProj {
            name,
            type_,
            tuple,
            index,
            cont,
        } => {
            free_variables_value(env.clone(), fvs, tuple);
            env.insert(
                name.clone(),
                Scheme {
                    variables: Vec::new(),
                    type_: type_.clone(),
                },
            );
            free_variables_expr(env, fvs, cont);
            fvs.remove(name);
        }
        Expr::If {
            cond,
            return_type,
            then,
            else_,
        } => {
            free_variables_value(env.clone(), fvs, cond);
            free_variables_expr(env.clone(), fvs, then);
            free_variables_expr(env, fvs, else_);
        }
    }
}
