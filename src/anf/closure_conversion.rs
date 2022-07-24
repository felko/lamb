use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::anf::syntax::*;

pub struct ClosureConverter {
    var_supply: Rc<Cell<usize>>,
}

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
                    type_: type_.clone(),
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
        if let Value::Var { name, type_, .. } = value.clone() {
            self.substitute_var(env_name, subst, name, box move |var| {
                cont(Value::Var {
                    name: var,
                    type_args: Vec::new(),
                    type_,
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
            Expr::Jump {
                name,
                args,
                return_type,
            } => {
                *expr = self.substitute_values(env_name, subst, args, box move |args| Expr::Jump {
                    name: name.clone(),
                    args,
                    return_type: return_type.clone(),
                })
            }
            Expr::LetJoin {
                name: _,
                params: _,
                return_type: _,
                body,
                cont,
            } => {
                self.substitute_free_variables(env_name, subst, body);
                self.substitute_free_variables(env_name, subst, cont);
            }
            Expr::LetFun {
                name: _,
                type_params: _,
                params: _,
                return_type: _,
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
                args,
                cont,
            } => {
                *expr = self.substitute_value(env_name, subst, callee.clone(), box move |callee| {
                    self.substitute_values(env_name, subst, args, box move |args| {
                        self.substitute_free_variables(env_name, subst, cont);
                        Expr::LetApp {
                            name: name.clone(),
                            type_: type_.clone(),
                            callee,
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

    fn convert_expr<'src>(&self, expr: &mut Expr<'src>) {
        match expr {
            Expr::Halt { .. } => {}
            Expr::Jump { .. } => {}
            Expr::LetJoin {
                name: _,
                params: _,
                return_type: _,
                body,
                cont,
            } => {
                self.convert_expr(body);
                self.convert_expr(cont);
            }
            Expr::LetFun {
                name: _,
                type_params: _,
                params,
                return_type: _,
                body,
                cont,
            } => {
                self.convert_expr(body);
                let env_name = self.fresh("env");
                let mut fvs = HashMap::new();
                free_variables_expr(&mut fvs, body);
                for param in params.clone() {
                    fvs.remove(&param.name);
                }
                let mut env = Vec::new();
                let mut subst = HashMap::new();
                let mut env_types = Vec::new();
                for (i, (fv, type_)) in fvs.iter().enumerate() {
                    subst.insert(fv.clone(), (i as u8, type_.clone()));
                    env.push(Value::Var {
                        name: fv.clone(),
                        type_args: Vec::new(),
                        type_: type_.clone(),
                    });
                    env_types.push(type_.clone());
                }
                self.substitute_free_variables(&env_name, &subst, body);
                params.push(Binding {
                    name: env_name.clone(),
                    type_: Type::Tuple(env_types.clone()),
                });
                self.convert_expr(cont);
                *cont = box Expr::LetTuple {
                    name: env_name,
                    types: env_types,
                    elements: env,
                    cont: cont.clone(),
                };
            }
            Expr::LetAdd {
                name: _,
                lhs: _,
                rhs: _,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::LetVal {
                name: _,
                type_: _,
                value: _,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::LetApp {
                name,
                type_,
                callee,
                args,
                cont,
            } => {
                let f = self.fresh("f");
                self.convert_expr(cont);
                *expr = Expr::LetProj {
                    name: f.clone(),
                    type_: infer_value(callee),
                    tuple: callee.clone(),
                    index: 0,
                    cont: box Expr::LetApp {
                        name: name.clone(),
                        type_: type_.clone(),
                        callee: Value::Var {
                            name: f,
                            type_args: Vec::new(),
                            type_: infer_value(callee),
                        },
                        args: args.clone(),
                        cont: cont.clone(),
                    },
                };
            }
            Expr::LetTuple {
                name: _,
                types: _,
                elements: _,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::LetProj {
                name: _,
                type_: _,
                tuple: _,
                index: _,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::If {
                cond: _,
                return_type: _,
                then,
                else_,
            } => {
                self.convert_expr(then);
                self.convert_expr(else_);
            }
        }
    }

    fn convert_decl<'src>(&self, decl: &mut FunDecl<'src>) {
        self.convert_expr(&mut decl.body);
    }

    pub fn convert_module<'src>(&self, module: &mut Module<'src>) {
        module
            .functions
            .values_mut()
            .for_each(|decl| self.convert_decl(decl));
    }
}

fn infer_value<'src>(value: &Value<'src>) -> Type<'src> {
    match value {
        Value::Var { type_, .. } => type_.clone(),
        Value::Lit(Literal::Int(_)) => Type::Int,
        Value::Lit(Literal::Bool(_)) => Type::Bool,
    }
}

fn free_variables_value<'src>(fvs: &mut HashMap<String, Type<'src>>, value: &Value<'src>) {
    match value {
        Value::Var { name, type_, .. } => {
            fvs.insert(name.clone(), type_.clone());
        }
        Value::Lit(_) => {}
    }
}

fn free_variables_expr<'src>(fvs: &mut HashMap<String, Type<'src>>, expr: &Expr<'src>) {
    match expr {
        Expr::Halt { value } => free_variables_value(fvs, value),
        Expr::Jump {
            name,
            args,
            return_type,
        } => {
            args.iter().for_each(|arg| free_variables_value(fvs, arg));
            fvs.insert(name.clone(), return_type.clone());
        }
        Expr::LetJoin {
            name,
            params,
            return_type: _,
            body,
            cont,
        } => {
            free_variables_expr(fvs, body);
            free_variables_expr(fvs, cont);
            params.iter().for_each(|Binding { name, .. }| {
                fvs.remove(name);
            });
            fvs.remove(name);
        }
        Expr::LetFun {
            name,
            type_params: _,
            params,
            return_type: _,
            body,
            cont,
        } => {
            free_variables_expr(fvs, body);
            params.iter().for_each(|param| {
                fvs.remove(&param.name);
            });
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetAdd {
            name,
            lhs,
            rhs,
            cont,
        } => {
            free_variables_value(fvs, lhs);
            free_variables_value(fvs, rhs);
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetVal {
            name,
            type_: _,
            value,
            cont,
        } => {
            free_variables_value(fvs, value);
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetApp {
            name,
            type_: _,
            callee,
            args,
            cont,
        } => {
            free_variables_value(fvs, callee);
            args.iter().for_each(|arg| free_variables_value(fvs, arg));
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetTuple {
            name,
            types: _,
            elements,
            cont,
        } => {
            elements
                .iter()
                .for_each(|element| free_variables_value(fvs, element));
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetProj {
            name,
            type_: _,
            tuple,
            index: _,
            cont,
        } => {
            free_variables_value(fvs, tuple);
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::If {
            cond,
            return_type: _,
            then,
            else_,
        } => {
            free_variables_value(fvs, cond);
            free_variables_expr(fvs, then);
            free_variables_expr(fvs, else_);
        }
    }
}
