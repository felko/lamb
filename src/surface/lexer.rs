use logos::Lexer as LogosLexer;
use logos::Logos;
use std::fmt::Display;
use std::num::ParseIntError;

use crate::pretty::*;
use crate::surface::Span;

#[derive(Logos, Debug, PartialEq, Eq)]
pub enum TokenType {
    #[error]
    // skip whitespace
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    // skip comments
    #[regex(r"(#[^\n]*)|(#[^(\r\n)]*)", logos::skip)]
    Error,

    // Delimiters and syntactic operators
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("->")]
    Arrow,
    #[token("=>")]
    DoubleArrow,

    // Operators
    #[token("+")]
    Plus,

    // Keywords
    #[token("def")]
    Def,
    #[token("fun")]
    Fun,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,

    // Primitive types
    #[token("Int")]
    Int,
    #[token("Bool")]
    Bool,

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,

    // Literals
    #[regex(r"-?([0-9]+)")]
    Number,
    #[token("true")]
    True,
    #[token("false")]
    False,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'src> {
    LParen,
    RParen,
    LAngle,
    RAngle,
    Comma,
    Dot,
    Colon,
    Equal,
    Arrow,
    DoubleArrow,
    Plus,
    Def,
    Fun,
    Let,
    In,
    If,
    Then,
    Else,
    Int,
    Bool,
    Identifier(&'src str),
    Number(i32),
    True,
    False,
}

#[derive(Debug)]
pub enum Error {
    InvalidToken(Span),
    InvalidInt(Span, ParseIntError),
}

pub struct Lexer<'src> {
    logos: LogosLexer<'src, TokenType>,
    pub errors: Vec<Error>,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Equal => write!(f, "="),
            Token::Arrow => write!(f, "->"),
            Token::DoubleArrow => write!(f, "=>"),
            Token::Plus => write!(f, "+"),
            Token::Def => write!(f, "def"),
            Token::Fun => write!(f, "fun"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Int => write!(f, "Int"),
            Token::Bool => write!(f, "Bool"),
            Token::Identifier(ident) => write!(f, "{ident}"),
            Token::Number(num) => write!(f, "{num}"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidToken(span) => write!(f, "Invalid token at {span:?}"),
            Error::InvalidInt(span, err) => write!(
                f,
                "Couldn't parse integer literal at position {span:?}: {err}"
            ),
        }
    }
}

impl<'a> PrettyPrec<'a> for Error {
    fn pretty_prec(
        self,
        _: crate::pretty::Prec,
        allocator: &'a crate::pretty::DocAllocator<'a>,
    ) -> crate::pretty::DocBuilder<'a> {
        allocator.text(format!("{self}"))
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (usize, Token<'src>, usize);
    fn next(&mut self) -> Option<Self::Item> {
        let token = loop {
            let token = self.logos.next()?;
            match self.convert(token) {
                Ok(token) => break token,
                Err(err) => self.errors.push(err),
            }
        };
        let span = self.logos.span();
        Some((span.start, token, span.end))
    }
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            errors: Vec::new(),
            logos: LogosLexer::new(input),
        }
    }

    fn convert(&mut self, token: TokenType) -> Result<Token<'src>, Error> {
        match token {
            TokenType::Error => Err(Error::InvalidToken(self.logos.span())),
            TokenType::LParen => Ok(Token::LParen),
            TokenType::RParen => Ok(Token::RParen),
            TokenType::LAngle => Ok(Token::LAngle),
            TokenType::RAngle => Ok(Token::RAngle),
            TokenType::Comma => Ok(Token::Comma),
            TokenType::Dot => Ok(Token::Dot),
            TokenType::Colon => Ok(Token::Colon),
            TokenType::Equal => Ok(Token::Equal),
            TokenType::Arrow => Ok(Token::Arrow),
            TokenType::DoubleArrow => Ok(Token::DoubleArrow),
            TokenType::Plus => Ok(Token::Plus),
            TokenType::Def => Ok(Token::Def),
            TokenType::Fun => Ok(Token::Fun),
            TokenType::Let => Ok(Token::Let),
            TokenType::In => Ok(Token::In),
            TokenType::If => Ok(Token::If),
            TokenType::Then => Ok(Token::Then),
            TokenType::Else => Ok(Token::Else),
            TokenType::Int => Ok(Token::Int),
            TokenType::Bool => Ok(Token::Bool),
            TokenType::Identifier => Ok(Token::Identifier(self.logos.slice())),
            TokenType::Number => match self.logos.slice().parse() {
                Ok(v) => Ok(Token::Number(v)),
                Err(e) => Err(Error::InvalidInt(self.logos.span(), e)),
            },
            TokenType::True => Ok(Token::True),
            TokenType::False => Ok(Token::False),
        }
    }
}
