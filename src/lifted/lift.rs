#![allow(unused_variables, unreachable_code)]

use std::collections::HashMap;

use crate::anf::syntax as anf;
use crate::lifted::syntax as lifted;

pub struct Lifter<'src> {
    name_supply: usize,
    join_points: Vec<lifted::JoinPoint<'src>>,
    functions: Vec<lifted::FunDecl<'src>>,
}

impl<'src> Lifter<'src> {
    pub fn new() -> Lifter<'src> {
        Lifter {
            name_supply: 0,
            join_points: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn fresh(&mut self, prefix: &str) -> String {
        let mut name = "%".to_owned();
        name.push_str(prefix);
        name.push_str(self.name_supply.to_string().as_ref());
        self.name_supply += 1;
        name
    }

    fn lift_expr(&mut self, expr: anf::Expr<'src>) -> lifted::Expr<'src> {
        match expr {
            anf::Expr::Halt { value } => lifted::Expr::Halt { value },
            anf::Expr::Jump { name, args, return_type } => lifted::Expr::Jump { name, args, return_type },
            anf::Expr::LetJoin {
                name,
                params,
                return_type,
                box body,
                box cont,
            } => {
                let lifted_body = self.lift_expr(body);
                self.join_points.push(lifted::JoinPoint {
                    name,
                    params,
                    return_type,
                    body: lifted_body,
                });
                self.lift_expr(cont)
            }
            anf::Expr::LetFun {
                name,
                type_params,
                params,
                return_type,
                box body,
                box cont,
            } => {
                let fresh_name = self.fresh(name.as_ref());
                self.lift_decl(fresh_name, type_params, params, return_type, body);
                self.lift_expr(cont)
            }
            anf::Expr::LetAdd {
                name,
                lhs,
                rhs,
                box cont,
            } => lifted::Expr::LetAdd {
                name,
                lhs,
                rhs,
                cont: box self.lift_expr(cont),
            },
            anf::Expr::LetVal {
                name,
                type_,
                value,
                box cont,
            } => lifted::Expr::LetVal {
                name,
                type_,
                value,
                cont: box self.lift_expr(cont),
            },
            anf::Expr::LetApp {
                name,
                type_,
                callee,
                args,
                box cont,
            } => lifted::Expr::LetApp {
                name,
                type_,
                callee,
                args,
                cont: box self.lift_expr(cont),
            },
            anf::Expr::LetTuple {
                name,
                types,
                elements,
                box cont,
            } => lifted::Expr::LetTuple {
                name,
                types,
                elements,
                cont: box self.lift_expr(cont),
            },
            anf::Expr::LetProj {
                name,
                type_,
                tuple,
                index,
                box cont,
            } => lifted::Expr::LetProj {
                name,
                type_,
                tuple,
                index,
                cont: box self.lift_expr(cont),
            },
            anf::Expr::If {
                cond,
                return_type,
                box then,
                box else_,
            } => lifted::Expr::If {
                cond,
                return_type,
                then: box self.lift_expr(then),
                else_: box self.lift_expr(else_),
            },
        }
    }

    fn lift_decl(
        &mut self,
        name: String,
        type_params: Vec<String>,
        params: Vec<anf::Binding<'src>>,
        return_type: anf::Type<'src>,
        body: anf::Expr<'src>,
    ) {
        let lifted_body = self.lift_expr(body);
        self.functions.push(lifted::FunDecl {
            name,
            type_params,
            params,
            return_type,
            body: lifted_body,
            join_points: self.join_points.clone(),
        });
        self.join_points.clear();
    }

    pub fn lift_module(&mut self, module: anf::Module<'src>) -> lifted::Module<'src> {
        for anf::FunDecl {
            name,
            type_params,
            params,
            return_type,
            body,
        } in module.functions.values()
        {
            self.lift_decl(
                name.to_string(),
                type_params.clone(),
                params.clone(),
                return_type.clone(),
                body.clone(),
            );
        }
        lifted::Module {
            functions: HashMap::from_iter(
                self.functions
                    .iter()
                    .map(|decl| (decl.name.clone(), decl.clone())),
            ),
        }
    }
}
