#[derive(PartialEq, Eq, Debug)]
pub enum Type<'src> {
    Var(&'src str),
    UVar(u32),
    Int,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Binding<'src> {
    Typed(&'src str, Type<'src>),
    Inferred(&'src str),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr<'src> {
    Lit(i32),
    Var(&'src str),
    Abs(Vec<Binding<'src>>, Box<Expr<'src>>),
    Add(Box<Expr<'src>>, Box<Expr<'src>>),
    Let(
        &'src str,
        Vec<Binding<'src>>,
        Option<Type<'src>>,
        Box<Expr<'src>>,
        Box<Expr<'src>>,
    ),
    App(Box<Expr<'src>>, Vec<Expr<'src>>),
}
