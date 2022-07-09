#![allow(unused_variables, unreachable_code)]

use std::cell::Cell;
use std::collections::{HashMap, HashSet};
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
        subst: &HashMap<String, u8>,
        var: String,
        cont: Box<dyn FnOnce(String) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        if let Some(index) = subst.get(&var) {
            let c = self.fresh("c");
            Expr::LetProj {
                name: c.clone(),
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
        subst: &HashMap<String, u8>,
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
        subst: &HashMap<String, u8>,
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
        subst: &HashMap<String, u8>,
        values: &Vec<Value<'src>>,
        cont: Box<dyn FnOnce(Vec<Value<'src>>) -> Expr<'src> + 'a>,
    ) -> Expr<'src> {
        let accum = Vec::new();
        self.substitute_values_aux(env_name, subst, values, 0, accum, cont)
    }

    fn substitute_free_variables(
        &self,
        env_name: &str,
        subst: &HashMap<String, u8>,
        expr: &mut Expr,
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
                elements,
                cont,
            } => {
                *expr = self.substitute_values(env_name, subst, elements, box move |elements| {
                    self.substitute_free_variables(env_name, subst, cont);
                    Expr::LetTuple {
                        name: name.clone(),
                        elements,
                        cont: cont.clone(),
                    }
                });
            }
            Expr::LetProj {
                name,
                tuple,
                index,
                cont,
            } => {
                *expr = self.substitute_value(env_name, subst, tuple.clone(), box move |tuple| {
                    self.substitute_free_variables(env_name, subst, cont);
                    Expr::LetProj {
                        name: name.clone(),
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
            Expr::Jump { name, args } => {}
            Expr::LetJoin {
                name,
                params,
                body,
                cont,
            } => {
                self.convert_expr(body);
                self.convert_expr(cont);
            }
            Expr::LetFun {
                name,
                type_params,
                params,
                return_type,
                body,
                cont,
            } => {
                self.convert_expr(body);
                let env_name = self.fresh("env");
                let mut fvs = HashSet::new();
                free_variables_expr(&mut fvs, body);
                for param in params.clone() {
                    fvs.remove(&param.name);
                }
                let mut closure = vec![Value::Var {
                    name: name.clone(),
                    type_args: Vec::new(),
                }];
                let mut subst = HashMap::new();
                for (i, fv) in fvs.iter().enumerate() {
                    subst.insert(fv.clone(), (i + 1) as u8);
                    closure.push(Value::Var {
                        name: fv.clone(),
                        type_args: Vec::new(),
                    });
                }
                self.substitute_free_variables(&env_name, &subst, body);
                params.push(Binding {
                    name: env_name.clone(),
                    type_: Type::QVar(env_name.clone()),
                });
                self.convert_expr(cont);
                *cont = box Expr::LetTuple {
                    name: name.clone(),
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
                self.convert_expr(cont);
            }
            Expr::LetVal {
                name,
                type_,
                value,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::LetApp {
                name,
                callee,
                type_args,
                args,
                cont,
            } => {
                let f = self.fresh("f");
                self.convert_expr(cont);
                let closure = Value::Var {
                    name: callee.clone(),
                    type_args: type_args.clone(),
                };
                *expr = Expr::LetProj {
                    name: f.clone(),
                    tuple: closure,
                    index: 0,
                    cont: box Expr::LetApp {
                        name: name.clone(),
                        type_args: Vec::new(),
                        callee: f,
                        args: args.clone(),
                        cont: cont.clone(),
                    },
                };
            }
            Expr::LetTuple {
                name,
                elements,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::LetProj {
                name,
                tuple,
                index,
                cont,
            } => {
                self.convert_expr(cont);
            }
            Expr::If {
                cond,
                return_type,
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

fn free_variables_value(fvs: &mut HashSet<String>, value: &Value) {
    match value {
        Value::Var { name, .. } => {
            fvs.insert(name.clone());
        }
        Value::Lit(_) => {}
    }
}

fn free_variables_expr(fvs: &mut HashSet<String>, expr: &Expr) {
    match expr {
        Expr::Halt { value } => free_variables_value(fvs, value),
        Expr::Jump { name, args } => {
            args.iter().for_each(|arg| free_variables_value(fvs, arg));
            fvs.insert(name.clone());
        }
        Expr::LetJoin {
            name,
            params,
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
            type_params,
            params,
            return_type,
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
            type_,
            value,
            cont,
        } => {
            free_variables_value(fvs, value);
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetApp {
            name,
            callee,
            type_args,
            args,
            cont,
        } => {
            fvs.insert(callee.clone());
            args.iter().for_each(|arg| free_variables_value(fvs, arg));
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::LetTuple {
            name,
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
            tuple,
            index,
            cont,
        } => {
            free_variables_value(fvs, tuple);
            free_variables_expr(fvs, cont);
            fvs.remove(name);
        }
        Expr::If {
            cond,
            return_type,
            then,
            else_,
        } => {
            free_variables_value(fvs, cond);
            free_variables_expr(fvs, then);
            free_variables_expr(fvs, else_);
        }
    }
}
