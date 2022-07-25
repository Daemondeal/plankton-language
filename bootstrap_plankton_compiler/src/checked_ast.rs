use crate::{
    ast::{LiteralKind, Operator},
    Span,
};

// TODO
pub struct CheckedAst {
    pub statements: Vec<CheckedStmt>,
    pub type_context: Vec<Type>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TypeId(pub usize);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Builtin(usize),
    Procedure(Vec<TypeId>, TypeId),
}

impl Type {
    pub fn get_default(&self, span: Span) -> CheckedExpr {
        match self {
            Self::Builtin(id) => {
                // Kinda ugly
                if id == &TYPEID_I32.0 {
                    CheckedExpr {
                        typ: TYPEID_I32,
                        span,
                        kind: CheckedExprKind::Literal(LiteralKind::Integer32(0)),
                    }
                } else if id == &TYPEID_F32.0 {
                    CheckedExpr {
                        typ: TYPEID_F32,
                        span,
                        kind: CheckedExprKind::Literal(LiteralKind::Float32(0.)),
                    }
                } else if id == &TYPEID_STRING.0 {
                    CheckedExpr {
                        typ: TYPEID_STRING,
                        span,
                        kind: CheckedExprKind::Literal(LiteralKind::String("".to_string())),
                    }
                } else if id == &TYPEID_BOOL.0 {
                    CheckedExpr {
                        typ: TYPEID_BOOL,
                        span,
                        kind: CheckedExprKind::Literal(LiteralKind::Boolean(false)),
                    }
                } else {
                    unreachable!("Tried to get default initializer for invalid type. Something has gone wrong while parsing.")
                }
            }

            Self::Procedure(_, _) => unreachable!("Tried to get default initializer for invalid type. Something has gone wrong while parsing.")
        }
    }
}

pub const TYPEID_VOID: TypeId = TypeId(0);
pub const TYPEID_I32: TypeId = TypeId(1);
pub const TYPEID_F32: TypeId = TypeId(2);
pub const TYPEID_BOOL: TypeId = TypeId(3);
pub const TYPEID_STRING: TypeId = TypeId(4);

pub const BUILTIN_TYPES: &[Type] = &[
    Type::Builtin(TYPEID_VOID.0),
    Type::Builtin(TYPEID_I32.0),
    Type::Builtin(TYPEID_F32.0),
    Type::Builtin(TYPEID_BOOL.0),
    Type::Builtin(TYPEID_STRING.0),
];

pub struct CheckedExpr {
    pub typ: TypeId,
    pub span: Span,
    pub kind: CheckedExprKind,
}

pub enum CheckedIntrinsic {
    Println(CheckedExpr),
}

pub enum CheckedExprKind {
    Operation(Operator, Vec<CheckedExpr>),
    Grouping(Box<CheckedExpr>),
    Variable(String),
    Literal(LiteralKind),
    If(Box<CheckedExpr>, Box<CheckedStmt>, Option<Box<CheckedStmt>>),
    While(Box<CheckedExpr>, Box<CheckedStmt>),
    Procedure(Vec<(String, TypeId)>, TypeId, Box<CheckedStmt>),
    Intrinsic(Box<CheckedIntrinsic>),
}

pub enum CheckedStmtKind {
    Declaration(String, TypeId, CheckedExpr),
    Expression(CheckedExpr),
    Return(CheckedExpr),
    Block(Vec<CheckedStmt>),
}

pub struct CheckedStmt {
    pub kind: CheckedStmtKind,
    pub span: Span,
}
