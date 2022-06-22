use std::fs::File;
use std::io::Read;
use std::ops::Range;

pub mod lexer;
lalrpop_mod!(pub parser, "/surface/parser.rs");
pub mod syntax;

pub type Span = Range<usize>;

use lexer::{Error as LexerError, Lexer, Token};
use parser::ModuleParser;
pub use syntax::*;

use bumpalo::Bump;

#[derive(Debug)]
pub enum Error<'src> {
    IOError(std::io::Error),
    ParseError(lalrpop_util::ParseError<usize, Token<'src>, LexerError>),
}

impl<'src> From<std::io::Error> for Error<'src> {
    fn from(error: std::io::Error) -> Self {
        Error::IOError(error)
    }
}

pub fn parse_from_str<'src>(source: &'src str) -> Result<Module<'src>, Error<'src>> {
    let mut lexer = Lexer::new(source);
    let mut errors = Vec::new();
    match ModuleParser::new().parse(&mut errors, &mut lexer) {
        Ok(expr) => Ok(expr),
        Err(error) => Err(Error::ParseError(error)),
    }
}

pub fn parse_from_file<'src>(
    path: &str,
    source_storage: &'src Bump,
) -> Result<Module<'src>, Error<'src>> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let source = source_storage.alloc(contents);
    let mut lexer = Lexer::new(source.as_mut_str());
    let mut errors = Vec::new();
    ModuleParser::new()
        .parse(&mut errors, &mut lexer)
        .map_err(Error::ParseError)
}

#[cfg(test)]
mod test {
    use crate::surface::{parse_from_str, syntax::Expr::*};

    #[test]
    fn test_parser() {
        if let Ok(expr) = parse_from_str("1+2+3+4+100") {
            assert_eq!(
                expr,
                Add(
                    Box::new(Add(
                        Box::new(Add(
                            Box::new(Add(Box::new(Lit(1)), Box::new(Lit(2)))),
                            Box::new(Lit(3))
                        )),
                        Box::new(Lit(4))
                    )),
                    Box::new(Lit(100))
                )
            )
        } else {
            assert!(false);
        }
    }
}
