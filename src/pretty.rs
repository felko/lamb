pub use pretty::DocAllocator as PrettyDocAllocator;

pub use termcolor::ColorSpec;

pub type Prec = u8;

pub type DocAllocator<'a> = pretty::Arena<'a, ColorSpec>;

pub type DocBuilder<'a> = pretty::DocBuilder<'a, DocAllocator<'a>, ColorSpec>;

pub const PRETTY_INDENT_SIZE: usize = 2;

pub trait PrettyPrec<'a> {
    fn pretty_prec(self, prec: Prec, allocator: &'a DocAllocator<'a>) -> DocBuilder<'a>;
}

impl<'a> PrettyPrec<'a> for ! {
    fn pretty_prec(
        self,
        _: crate::pretty::Prec,
        _: &'a crate::pretty::DocAllocator<'a>,
    ) -> crate::pretty::DocBuilder<'a> {
        unreachable!()
    }
}
