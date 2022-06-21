use logos::Lexer as LogosLexer;
use logos::Logos;
use std::num::ParseIntError;

use crate::surface::Span;

#[derive(Logos, Debug, PartialEq)]
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
    #[token("fun")]
    Fun,
    #[token("let")]
    Let,
    #[token("in")]
    In,

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
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    LParen,
    RParen,
    Colon,
    Equal,
    Arrow,
    DoubleArrow,
    Plus,
    Fun,
    Let,
    In,
    Int,
    Bool,
    Identifier(&'src str),
    Number(i32),
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
        return Some((span.start, token, span.end));
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
            TokenType::Colon => Ok(Token::Colon),
            TokenType::Equal => Ok(Token::Equal),
            TokenType::Arrow => Ok(Token::Arrow),
            TokenType::DoubleArrow => Ok(Token::DoubleArrow),
            TokenType::Plus => Ok(Token::Plus),
            TokenType::Fun => Ok(Token::Fun),
            TokenType::Let => Ok(Token::Let),
            TokenType::In => Ok(Token::In),
            TokenType::Int => Ok(Token::Int),
            TokenType::Bool => Ok(Token::Bool),
            TokenType::Identifier => Ok(Token::Identifier(self.logos.slice())),
            TokenType::Number => match self.logos.slice().parse() {
                Ok(v) => Ok(Token::Number(v)),
                Err(e) => Err(Error::InvalidInt(self.logos.span(), e)),
            },
        }
    }
}
