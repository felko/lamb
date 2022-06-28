use crate::pretty::*;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

impl<'a> PrettyPrec<'a> for Literal {
    fn pretty_prec(self, _: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a> {
        match self {
            Literal::Int(x) => allocator.text(x.to_string()).annotate(
                ColorSpec::new()
                    .set_fg(Some(termcolor::Color::Yellow))
                    .clone(),
            ),
            Literal::Bool(b) => allocator.text(b.to_string()).annotate(
                ColorSpec::new()
                    .set_fg(Some(termcolor::Color::Magenta))
                    .clone(),
            ),
        }
    }
}


