use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::anf;
use crate::core;

type Env<'src> = im::hashmap::HashMap<String, anf::Scheme<'src>>;

pub struct ANFConverter {
    var_supply: Rc<Cell<usize>>,
}

impl ANFConverter {
    pub fn new() -> ANFConverter {
        ANFConverter {
            var_supply: Rc::new(Cell::new(0)),
        }
    }

    fn fresh(&self, prefix: &'static str) -> String {
        let mut name = "$".to_owned();
        name.push_str(prefix);
        name.push_str((*self.var_supply).get().to_string().as_ref());
        self.var_supply.set((*self.var_supply).get() + 1);
        name
    }

    fn convert_many_aux<'src, 'a>(
        &self,
        env: Env<'src>,
        index: usize,
        exprs: &Vec<core::Expr<'src>>,
        expr_cont: Box<dyn FnOnce(Env<'src>, Vec<anf::Value<'src>>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        if index >= exprs.len() {
            expr_cont(env, Vec::new())
        } else {
            let elem = exprs[index].clone();
            self.convert_expr(env.clone(), elem, box move |env2, head| {
                self.convert_many_aux(env2, index + 1, exprs, box move |env3, tail| {
                    let mut values = vec![head];
                    values.extend(tail);
                    expr_cont(env3, values)
                })
            })
        }
    }

    fn convert_many<'src, 'a>(
        &self,
        env: Env<'src>,
        exprs: &Vec<core::Expr<'src>>,
        expr_cont: Box<dyn FnOnce(Env<'src>, Vec<anf::Value<'src>>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        self.convert_many_aux(env, 0, exprs, expr_cont)
    }

    fn convert_app<'src, 'a>(
        &self,
        env: Env<'src>,
        callee: core::Expr<'src>,
        args: Vec<core::Expr<'src>>,
        expr_cont: Box<dyn FnOnce(Env<'src>, anf::Value<'src>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        self.convert_expr(
            env.clone(),
            callee,
            box move |env, callee_val| match callee_val {
                anf::Value::Var {
                    name: callee_name,
                    type_args,
                } => self.convert_many(
                    env.clone(),
                    &args.clone(),
                    box move |env, args_val| {
                        self.convert_app_aux(
                            env,
                            callee_name,
                            type_args.as_ref(),
                            args_val,
                            expr_cont,
                        )
                    },
                ),
                _ => panic!("internal error: callee should be a variable"),
            },
        )
    }

    fn convert_app_aux<'src, 'a>(
        &self,
        mut env: Env<'src>,
        callee_name: String,
        type_args: &[anf::Type<'src>],
        mut args_val: Vec<anf::Value<'src>>,
        expr_cont: Box<dyn FnOnce(Env<'src>, anf::Value<'src>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        let r = self.fresh("r");
        let callee_scheme = env
            .get(&callee_name)
            .expect("internal error: undefined variable");
        if let anf::Type::Func(mut param_types, box return_type) =
            ANFConverter::specialize(callee_scheme, type_args)
        {
            match args_val.len().cmp(&param_types.len()) {
                std::cmp::Ordering::Greater => {
                    let r2 = self.fresh("r");
                    let remaining_args = args_val.split_off(param_types.len());
                    env.insert(
                        r2.clone(),
                        anf::Scheme {
                            variables: Vec::new(),
                            type_: return_type.clone(),
                        },
                    );
                    anf::Expr::LetApp {
                        name: r2.clone(),
                        type_: return_type,
                        callee: callee_name,
                        type_args: type_args.to_vec(),
                        args: args_val,
                        cont: box self.convert_app_aux(
                            env,
                            r2,
                            &[],
                            remaining_args,
                            expr_cont,
                        ),
                    }
                }
                std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
                    let remaining_params = param_types.split_off(args_val.len());
                    env.insert(
                        r.clone(),
                        anf::Scheme {
                            variables: Vec::new(),
                            type_: if remaining_params.is_empty() {
                                return_type.clone()
                            } else {
                                anf::Type::Func(remaining_params, box return_type.clone())
                            },
                        },
                    );
                    anf::Expr::LetApp {
                        name: r.clone(),
                        type_: return_type,
                        callee: callee_name,
                        type_args: type_args.to_vec(),
                        args: args_val,
                        cont: box expr_cont(
                            env,
                            anf::Value::Var {
                                name: r,
                                type_args: Vec::new(),
                            },
                        ),
                    }
                }
            }
        } else {
            panic!("internal error: callee is not a function");
        }
    }

    fn convert_expr<'src, 'a>(
        &self,
        mut env: Env<'src>,
        expr: core::Expr<'src>,
        expr_cont: Box<dyn FnOnce(Env<'src>, anf::Value<'src>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        match expr {
            core::Expr::Lit(literal) => expr_cont(env, anf::Value::Lit(literal)),
            core::Expr::Var { name, type_args } => expr_cont(
                env,
                anf::Value::Var {
                    name: (*name).to_owned(),
                    type_args,
                },
            ),
            core::Expr::Abs {
                params,
                return_type,
                box body,
            } => {
                let name = self.fresh("f");
                let abs_type = anf::Type::Func(
                    params.iter().map(|param| param.type_.clone()).collect(),
                    box return_type.clone(),
                );
                let body_anf = self.convert_expr(env.clone(), body, box |_, ret| anf::Expr::Halt {
                    value: ret,
                });
                env.insert(
                    name.clone(),
                    anf::Scheme {
                        variables: Vec::new(),
                        type_: abs_type,
                    },
                );
                anf::Expr::LetFun {
                    name: name.clone(),
                    type_params: Vec::new(),
                    params,
                    return_type,
                    body: box body_anf,
                    cont: box expr_cont(
                        env,
                        anf::Value::Var {
                            name,
                            type_args: Vec::new(),
                        },
                    ),
                }
            }
            core::Expr::Add(box lhs, box rhs) => {
                self.convert_expr(env.clone(), lhs, box move |env, lhs_val| {
                    self.convert_expr(env, rhs, box move |mut env, rhs_val| {
                        let name = self.fresh("a");
                        env.insert(
                            name.clone(),
                            anf::Scheme {
                                variables: Vec::new(),
                                type_: anf::Type::Int,
                            },
                        );
                        anf::Expr::LetAdd {
                            name: name.clone(),
                            lhs: lhs_val.clone(),
                            rhs: rhs_val.clone(),
                            cont: box expr_cont(
                                env,
                                anf::Value::Var {
                                    name,
                                    type_args: Vec::new(),
                                },
                            ),
                        }
                    })
                })
            }
            core::Expr::Let {
                name,
                type_params,
                params,
                return_type,
                box body,
                box cont,
            } => {
                if params.is_empty() {
                    match body {
                        core::Expr::Lit(literal) => {
                            env.insert(
                                name.to_owned(),
                                anf::Scheme {
                                    variables: Vec::new(),
                                    type_: ANFConverter::infer_value(
                                        &env,
                                        &anf::Value::Lit(literal.clone()),
                                    ),
                                },
                            );
                            anf::Expr::LetVal {
                                name: name.to_owned(),
                                type_: return_type,
                                value: anf::Value::Lit(literal),
                                cont: box self.convert_expr(env, cont, expr_cont),
                            }
                        }
                        core::Expr::Tuple(elements) => self.convert_many(
                            env.clone(),
                            &elements,
                            box move |mut env, elements_val| {
                                let types = elements_val
                                    .iter()
                                    .map(|element_val| ANFConverter::infer_value(&env, element_val))
                                    .collect::<Vec<anf::Type>>();
                                env.insert(
                                    name.to_owned(),
                                    anf::Scheme {
                                        variables: Vec::new(),
                                        type_: anf::Type::Tuple(types.clone()),
                                    },
                                );
                                anf::Expr::LetTuple {
                                    name: name.to_owned(),
                                    types,
                                    elements: elements_val,
                                    cont: box self.convert_expr(env, cont, expr_cont),
                                }
                            },
                        ),
                        core::Expr::App { box callee, args } => {
                            self.convert_app(env, callee, args, expr_cont)
                        }
                        body => {
                            self.convert_expr(env.clone(), body, box move |mut env, body_value| {
                                env.insert(
                                    name.to_owned(),
                                    anf::Scheme {
                                        variables: Vec::new(),
                                        type_: ANFConverter::infer_value(&env, &body_value.clone()),
                                    },
                                );
                                anf::Expr::LetVal {
                                    name: name.to_owned(),
                                    type_: return_type,
                                    value: body_value,
                                    cont: box self.convert_expr(env, cont, expr_cont),
                                }
                            })
                        }
                    }
                } else {
                    let body_env = {
                        let mut env = env.clone();
                        params.iter().for_each(|param| {
                            env.insert(
                                param.name.clone(),
                                anf::Scheme {
                                    variables: Vec::new(),
                                    type_: param.type_.clone(),
                                },
                            );
                        });
                        env
                    };
                    let cont_env = {
                        let mut env = env.clone();
                        env.insert(
                            name.to_owned(),
                            anf::Scheme {
                                variables: type_params.clone(),
                                type_: anf::Type::Func(
                                    params.iter().map(|param| param.type_.clone()).collect(),
                                    box return_type.clone(),
                                ),
                            },
                        );
                        env
                    };
                    anf::Expr::LetFun {
                        name: name.to_owned(),
                        type_params,
                        params,
                        return_type,
                        body: box self.convert_expr(body_env, body, box move |_, return_value| {
                            anf::Expr::Halt {
                                value: return_value,
                            }
                        }),
                        cont: box self.convert_expr(cont_env, cont, expr_cont),
                    }
                }
            }
            core::Expr::If {
                box cond,
                return_type,
                box then,
                box else_,
            } => self.convert_expr(env.clone(), cond, box move |env, cond_value| {
                let join_point_name = self.fresh("j");
                let join_point_name2 = join_point_name.clone();
                let join_cont = box move |_, value| anf::Expr::Jump {
                    name: join_point_name2,
                    args: vec![value],
                };
                let join_param = self.fresh("r");
                let body_env = {
                    let mut env = env.clone();
                    env.insert(
                        join_param.clone(),
                        anf::Scheme {
                            variables: Vec::new(),
                            type_: return_type.clone(),
                        },
                    );
                    env
                };
                let cont_env = {
                    let mut env = env.clone();
                    let join_type =
                        anf::Type::Func(vec![return_type.clone()], box return_type.clone());
                    env.insert(
                        join_point_name.clone(),
                        anf::Scheme {
                            variables: Vec::new(),
                            type_: join_type,
                        },
                    );
                    env
                };
                anf::Expr::LetJoin {
                    name: join_point_name,
                    params: vec![anf::Binding {
                        name: join_param.clone(),
                        type_: return_type.clone(),
                    }],
                    return_type: return_type.clone(),
                    body: box expr_cont(
                        body_env,
                        anf::Value::Var {
                            name: join_param,
                            type_args: Vec::new(),
                        },
                    ),
                    cont: box anf::Expr::If {
                        cond: cond_value,
                        return_type,
                        then: box self.convert_expr(cont_env.clone(), then, join_cont.clone()),
                        else_: box self.convert_expr(cont_env, else_, join_cont),
                    },
                }
            }),
            core::Expr::App { box callee, args } => self.convert_app(env, callee, args, expr_cont),
            core::Expr::Tuple(elements) => {
                self.convert_many(env.clone(), &elements, box move |mut env, elements_val| {
                    let r = self.fresh("t");
                    let types = elements_val
                        .iter()
                        .map(|element_val| ANFConverter::infer_value(&env, element_val))
                        .collect::<Vec<anf::Type>>();
                    env.insert(
                        r.clone(),
                        anf::Scheme {
                            variables: Vec::new(),
                            type_: anf::Type::Tuple(types.clone()),
                        },
                    );
                    anf::Expr::LetTuple {
                        name: r.clone(),
                        types,
                        elements: elements_val,
                        cont: box expr_cont(
                            env,
                            anf::Value::Var {
                                name: r,
                                type_args: Vec::new(),
                            },
                        ),
                    }
                })
            }
            core::Expr::Proj(box tuple, index) => {
                self.convert_expr(env.clone(), tuple, box move |mut env, tuple_val| {
                    let r = self.fresh("t");
                    if let anf::Type::Tuple(element_types) =
                        ANFConverter::infer_value(&env, &tuple_val)
                    {
                        let element_type = element_types[index as usize].clone();
                        env.insert(
                            r.clone(),
                            anf::Scheme {
                                variables: Vec::new(),
                                type_: element_type.clone(),
                            },
                        );
                        anf::Expr::LetProj {
                            name: r.clone(),
                            type_: element_type,
                            tuple: tuple_val,
                            index,
                            cont: box expr_cont(
                                env,
                                anf::Value::Var {
                                    name: r,
                                    type_args: Vec::new(),
                                },
                            ),
                        }
                    } else {
                        panic!("internal error: cannot take project of non-tuple type");
                    }
                })
            }
        }
    }

    fn specialize_type<'src>(
        subst: &HashMap<String, anf::Type<'src>>,
        type_: &anf::Type<'src>,
    ) -> anf::Type<'src> {
        use anf::Type::*;
        match type_ {
            Name { name } => Name { name },
            QVar(name) => match subst.get(name) {
                Some(type_) => type_.clone(),
                None => QVar(name.clone()),
            },
            Func(parameter_types, box return_type) => {
                let parameter_types = parameter_types
                    .iter()
                    .map(|parameter_type| ANFConverter::specialize_type(subst, parameter_type))
                    .collect();
                let return_type = ANFConverter::specialize_type(subst, return_type);
                Func(parameter_types, box return_type)
            }
            Tuple(element_types) => Tuple(
                element_types
                    .iter()
                    .map(|element_type| ANFConverter::specialize_type(subst, element_type))
                    .collect(),
            ),
            Int => Int,
            Bool => Bool,
        }
    }

    fn specialize<'src>(
        scheme: &anf::Scheme<'src>,
        type_args: &[anf::Type<'src>],
    ) -> anf::Type<'src> {
        let anf::Scheme { variables, type_ } = scheme;
        let subst = variables
            .iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();
        ANFConverter::specialize_type(&subst, type_)
    }

    fn infer_value<'src>(env: &Env<'src>, value: &anf::Value<'src>) -> anf::Type<'src> {
        match value {
            anf::Value::Var { name, type_args } => {
                let scheme = env.get(name).expect("internal error: undefined variable");
                ANFConverter::specialize(scheme, type_args)
            }
            anf::Value::Lit(anf::Literal::Int(_)) => anf::Type::Int,
            anf::Value::Lit(anf::Literal::Bool(_)) => anf::Type::Bool,
        }
    }

    fn convert_decl<'src>(
        &self,
        mut env: Env<'src>,
        decl: core::FunDecl<'src>,
    ) -> anf::FunDecl<'src> {
        decl.params.iter().for_each(|param| {
            env.insert(
                param.name.clone(),
                anf::Scheme {
                    variables: Vec::new(),
                    type_: param.type_.clone(),
                },
            );
        });
        anf::FunDecl {
            name: decl.name,
            type_params: decl.type_params,
            params: decl.params,
            return_type: decl.return_type,
            body: self.convert_expr(env, decl.body, box move |_, return_value| anf::Expr::Halt {
                value: return_value,
            }),
        }
    }

    pub fn convert_module<'src>(&self, module: core::Module<'src>) -> anf::Module<'src> {
        let env = im::hashmap::HashMap::from_iter(module.functions.values().map(|decl| {
            let type_ = if decl.params.is_empty() {
                decl.return_type.clone()
            } else {
                anf::Type::Func(
                    decl.params
                        .iter()
                        .map(|param| param.type_.clone())
                        .collect(),
                    box decl.return_type.clone(),
                )
            };
            (
                decl.name.to_owned(),
                anf::Scheme {
                    variables: decl.type_params.clone(),
                    type_,
                },
            )
        }));
        anf::Module {
            functions: HashMap::from_iter(
                module
                    .functions
                    .iter()
                    .map(|(name, decl)| (*name, self.convert_decl(env.clone(), decl.clone()))),
            ),
        }
    }
}
