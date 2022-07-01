use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::anf;
use crate::core;

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
        index: usize,
        exprs: &Vec<core::Expr<'src>>,
        expr_cont: Box<dyn FnOnce(Vec<anf::Value<'src>>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        if index >= exprs.len() {
            expr_cont(Vec::new())
        } else {
            let elem = exprs[index].clone();
            self.convert_expr(elem, box move |head| {
                self.convert_many_aux(index + 1, exprs, box move |tail| {
                    let mut values = vec![head];
                    values.extend(tail);
                    expr_cont(values)
                })
            })
        }
    }

    fn convert_many<'src, 'a>(
        &self,
        exprs: &Vec<core::Expr<'src>>,
        expr_cont: Box<dyn FnOnce(Vec<anf::Value<'src>>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        self.convert_many_aux(0, exprs, expr_cont)
    }

    fn convert_expr<'src, 'a>(
        &self,
        expr: core::Expr<'src>,
        expr_cont: Box<dyn FnOnce(anf::Value<'src>) -> anf::Expr<'src> + 'a>,
    ) -> anf::Expr<'src> {
        match expr {
            core::Expr::Lit(literal) => expr_cont(anf::Value::Lit(literal)),
            core::Expr::Var { name, type_args } => expr_cont(anf::Value::Var {
                name: (*name).to_owned(),
                type_args,
            }),
            core::Expr::Abs {
                params,
                return_type,
                box body,
            } => {
                let name = self.fresh("f");
                let body_anf = self.convert_expr(body, box |ret| anf::Expr::Halt { value: ret });
                anf::Expr::LetFun {
                    name: name.clone(),
                    type_params: Vec::new(),
                    params,
                    return_type,
                    body: box body_anf,
                    cont: box expr_cont(anf::Value::Var {
                        name,
                        type_args: Vec::new(),
                    }),
                }
            }
            core::Expr::Add(box lhs, box rhs) => self.convert_expr(lhs, box move |lhs_val| {
                self.convert_expr(rhs, box move |rhs_val| {
                    let name = self.fresh("a");
                    anf::Expr::LetAdd {
                        name: name.clone(),
                        lhs: lhs_val.clone(),
                        rhs: rhs_val.clone(),
                        cont: box expr_cont(anf::Value::Var {
                            name,
                            type_args: Vec::new(),
                        }),
                    }
                })
            }),
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
                        core::Expr::Lit(literal) => anf::Expr::LetVal {
                            name: name.to_owned(),
                            type_: return_type,
                            value: anf::Value::Lit(literal),
                            cont: box self.convert_expr(cont, expr_cont),
                        },
                        core::Expr::Tuple(elements) => {
                            self.convert_many(&elements, box move |elements_val| {
                                anf::Expr::LetTuple {
                                    name: name.to_owned(),
                                    elements: elements_val,
                                    cont: box self.convert_expr(cont, expr_cont),
                                }
                            })
                        }
                        _ => self.convert_expr(body, box move |body_value| anf::Expr::LetVal {
                            name: name.to_owned(),
                            type_: return_type,
                            value: body_value,
                            cont: box self.convert_expr(cont, expr_cont),
                        }),
                    }
                } else {
                    anf::Expr::LetFun {
                        name: name.to_owned(),
                        type_params,
                        params,
                        return_type,
                        body: box self.convert_expr(body, box move |return_value| {
                            anf::Expr::Halt {
                                value: return_value,
                            }
                        }),
                        cont: box self.convert_expr(cont, expr_cont),
                    }
                }
            }
            core::Expr::If {
                box cond,
                return_type,
                box then,
                box else_,
            } => self.convert_expr(cond, box move |cond_value| {
                let join_point_name = self.fresh("j");
                let join_point_name2 = join_point_name.clone();
                let join_cont = box move |value| anf::Expr::Jump {
                    name: join_point_name2,
                    arg: value,
                };
                let join_param = self.fresh("r");
                anf::Expr::LetJoin {
                    name: join_point_name,
                    param: anf::Binding {
                        name: join_param.clone(),
                        type_: return_type.clone(),
                    },
                    body: box expr_cont(anf::Value::Var {
                        name: join_param,
                        type_args: Vec::new(),
                    }),
                    cont: box anf::Expr::If {
                        cond: cond_value,
                        return_type,
                        then: box self.convert_expr(then, join_cont.clone()),
                        else_: box self.convert_expr(else_, join_cont),
                    },
                }
            }),
            core::Expr::App { box callee, args } => {
                self.convert_expr(callee, box move |callee_val| match callee_val {
                    anf::Value::Var {
                        name: callee_name,
                        type_args,
                    } => self.convert_many(&args, box move |args_val| {
                        let r = self.fresh("r");
                        anf::Expr::LetApp {
                            name: r.clone(),
                            callee: callee_name,
                            type_args,
                            args: args_val,
                            cont: box expr_cont(anf::Value::Var {
                                name: r,
                                type_args: Vec::new(),
                            }),
                        }
                    }),
                    _ => panic!("anf::convert_expr: callee should be a variable"),
                })
            }
            core::Expr::Tuple(elements) => self.convert_many(&elements, box move |elements_val| {
                let r = self.fresh("t");
                anf::Expr::LetTuple {
                    name: r.clone(),
                    elements: elements_val,
                    cont: box expr_cont(anf::Value::Var {
                        name: r,
                        type_args: Vec::new(),
                    }),
                }
            }),
            core::Expr::Proj(box tuple, index) => self.convert_expr(tuple, box move |tuple_val| {
                let r = self.fresh("t");
                anf::Expr::LetProj {
                    name: r.clone(),
                    tuple: tuple_val,
                    index,
                    cont: box expr_cont(anf::Value::Var {
                        name: r,
                        type_args: Vec::new(),
                    }),
                }
            }),
        }
    }

    fn convert_decl<'src>(&self, decl: core::FunDecl<'src>) -> anf::FunDecl<'src> {
        anf::FunDecl {
            name: decl.name,
            type_params: decl.type_params,
            params: decl.params,
            return_type: decl.return_type,
            body: self.convert_expr(decl.body, box move |return_value| anf::Expr::Halt {
                value: return_value,
            }),
        }
    }

    pub fn convert_module<'src>(&self, module: core::Module<'src>) -> anf::Module<'src> {
        anf::Module {
            functions: HashMap::from_iter(
                module
                    .functions
                    .iter()
                    .map(|(name, decl)| (*name, self.convert_decl(decl.clone()))),
            ),
        }
    }
}
