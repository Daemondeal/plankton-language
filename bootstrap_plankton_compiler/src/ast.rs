use crate::{Span, debug::ast_viewer::ast_to_sexpr};
use std::fmt::Debug;

pub struct Ast {
    pub statements: Vec<Stmt>,
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ast_to_sexpr(self))
    }
}

pub enum LiteralKind {
    String (String),
    Integer32 (i32),
    Float32 (f32),
    Boolean (bool),
}

#[derive(Debug)]
pub enum Operator {
    Sum, Subtract, Divide, Multiply,
    LogicAnd, LogicOr, LogicNot,
    Negate,
    Equals, NotEquals,
    LessThan, LessThanEqual,
    GreaterThan, GreaterThanEqual,
    Assign, Get,
    GetAddress, Dereference, IndexAccess,
    ProcedureCall
}

impl Operator {
    pub fn get_name(&self) -> &str {
        match self {
            Operator::Equals => "equals",
            Operator::Assign => "assign",
            Operator::Sum => "+",
            Operator::Subtract => "-",
            Operator::Divide => "/",
            Operator::Multiply => "*",
            Operator::LogicAnd => "and",
            Operator::LogicOr => "or",
            Operator::LogicNot => "not",
            Operator::Negate => "-",
            Operator::Dereference => "deref",
            Operator::GetAddress => "getaddr",
            Operator::ProcedureCall => "call",
            Operator::IndexAccess => "access",
            Operator::NotEquals => "notequals",
            Operator::LessThan => "<",
            Operator::LessThanEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEqual => ">=",
            Operator::Get => "get",
        }
    }
}


pub enum ExprKind {
    Operation (Operator, Vec<Expr>),
    Grouping (Box<Expr>),
    Variable (String),
    Literal (LiteralKind),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>)
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub enum TypeExprKind {
    Builtin(String), // TODO: Change this
}

pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

pub enum StmtKind {
    Declaration(String, Option<TypeExpr>, Option<Expr>),
    Expression(Expr),
    Return(Expr),
    Block(Vec<Stmt>)
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
