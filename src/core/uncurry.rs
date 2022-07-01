use crate::core::syntax::*;

fn uncurry_type(type_: &mut Type<'_>) {
    use Type::*;
    if let Func(param_types, return_type) = type_ {
        param_types.iter_mut().for_each(uncurry_type);
        uncurry_type(return_type);
        if let Func(return_param_types, return_return_type) = *return_type.clone() {
            param_types.extend(return_param_types);
            *return_type = return_return_type;
        }
    }
}

fn uncurry_binding(binding: &mut Binding<'_>) {
    uncurry_type(&mut binding.type_);
}

fn uncurry_expr(expr: &mut Expr<'_>) {
    use Expr::*;
    match expr {
        Abs {
            params,
            return_type,
            body,
        } => {
            params.iter_mut().for_each(uncurry_binding);
            uncurry_type(return_type);
            uncurry_expr(body);
            if let Abs {
                params: body_params,
                return_type: body_return_type,
                body: body_body,
            } = *body.clone()
            {
                params.extend(body_params);
                *return_type = body_return_type;
                *body = body_body;
            }
        }
        Add(lhs, rhs) => {
            uncurry_expr(lhs);
            uncurry_expr(rhs);
        }
        Let {
            name,
            type_params,
            params,
            return_type,
            body,
            cont,
        } => {
            params.iter_mut().for_each(uncurry_binding);
            uncurry_type(return_type);
            uncurry_expr(body);
            uncurry_expr(cont);
            if let Expr::Abs {
                params: body_params,
                return_type: body_return_type,
                body: body_body,
            } = *body.clone()
            {
                params.extend(body_params);
                *expr = Expr::Let {
                    name,
                    type_params: type_params.to_vec(),
                    params: params.to_vec(),
                    return_type: body_return_type,
                    body: body_body,
                    cont: cont.clone(),
                };
            }
        }
        If {
            cond,
            return_type,
            then,
            else_,
        } => {
            uncurry_expr(cond);
            uncurry_type(return_type);
            uncurry_expr(then);
            uncurry_expr(else_);
        }
        App { callee, args } => {
            uncurry_expr(callee);
            args.iter_mut().for_each(uncurry_expr);
            if let App {
                callee: callee_callee,
                args: callee_args,
            } = *callee.clone()
            {
                *callee = callee_callee;
                let original_args = args.clone();
                *args = callee_args;
                args.extend(original_args);
            }
        }
        Tuple(elements) => elements.iter_mut().for_each(uncurry_expr),
        Proj(tuple, _) => uncurry_expr(tuple),
        Lit(_) => {}
        Var { .. } => {}
    }
}

fn uncurry_decl(decl: &mut FunDecl<'_>) {
    decl.params.iter_mut().for_each(uncurry_binding);
    uncurry_type(&mut decl.return_type);
    uncurry_expr(&mut decl.body);
}

pub fn uncurry_module(module: &mut Module) {
    module.functions.values_mut().for_each(uncurry_decl);
}
