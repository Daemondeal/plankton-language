use std::mem::Discriminant;

use crate::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn discriminant(&self) -> Discriminant<TokenKind> {
        std::mem::discriminant(&self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Comma,
    Dot,
    DotDot,
    Minus,
    Plus,
    Slash,
    Star,
    Colon,
    Arrow,
    Ampersand,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Equal,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    True,
    False,
    Not,
    And,
    Or,

    Let,

    If,
    Else,
    While,
    For,
    In,
    Proc,
    Return,

    Identifier(String),
    String(String),
    Integer32(i32),
    Float32(f32),
    // TypeAnnotation(TypeId), TODO
    LineEnd,
    Eof,
}

impl TokenKind {
    pub fn is_same_variant(&self, other: &TokenKind) -> bool {
        match (self, other) {
            (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
            (TokenKind::String(_), TokenKind::String(_)) => true,
            (TokenKind::Integer32(_), TokenKind::Integer32(_)) => true,
            (TokenKind::Float32(_), TokenKind::Float32(_)) => true,

            _ => self == other,
        }
    }
}
