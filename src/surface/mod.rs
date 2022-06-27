use std::ops::Range;

pub mod lexer;
lalrpop_mod!(#[allow(clippy::all)] pub parser, "/surface/parser.rs");
pub mod syntax;

pub type Span = Range<usize>;

use lexer::{Error as LexerError, Lexer, Token};
use parser::ModuleParser;
pub use syntax::*;

pub use lalrpop_util::ParseError;

use crate::pretty::*;

impl<'src, 'a> PrettyPrec<'a> for ParseError<usize, Token<'src>, LexerError> {
    fn pretty_prec(
        self,
        _: crate::pretty::Prec,
        allocator: &'a crate::pretty::DocAllocator<'a>,
    ) -> crate::pretty::DocBuilder<'a> {
        allocator.text(format!("{self}"))
    }
}

pub fn parse(source: &str) -> Result<Module, ParseError<usize, Token, LexerError>> {
    let mut lexer = Lexer::new(source);
    let mut errors = Vec::new();
    ModuleParser::new().parse(&mut errors, &mut lexer)
}
