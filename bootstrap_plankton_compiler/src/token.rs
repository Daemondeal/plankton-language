use crate::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub token_type: TokenType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen, RightParen, Comma, Dot, DotDot, 
    Minus, Plus, Slash, Star, Colon, Arrow, Ampersand,
    LeftBrace, RightBrace,
    LeftBracket, RightBracket,

    Equal, BangEqual, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    True, False,
    Not, And, Or,
    
    Let,

    If, Else, While, For,
    In,
    Proc, Return,

    Identifier(String), String(String), Integer32(i32), Float32(f32),
    // TypeAnnotation(TypeId), TODO

    LineEnd, Eof
}
