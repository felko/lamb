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
            type_, value, cont, ..
        } => {
            uncurry_type(type_);
            uncurry_expr(value);
            uncurry_expr(cont);
        }
        If { cond, then, else_ } => {
            uncurry_expr(cond);
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
        Lit(_) => {}
        Var { .. } => {}
    }
}

fn uncurry_decl(decl: &mut ValueDecl<'_>) {
    uncurry_type(&mut decl.type_);
    uncurry_expr(&mut decl.value);
}

pub fn uncurry_module(module: &mut Module) {
    module.values.values_mut().for_each(uncurry_decl);
}
