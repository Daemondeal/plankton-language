use crate::{token::Token, compiler::FileId, ast::{Ast, Stmt, StmtKind, Expr, ExprKind}, PlanktonError, Span};

struct Parser {
    tokens: Vec<Token>,
    current: usize,
    file: FileId
}

pub fn parse_tokens(
    source: Vec<Token>, 
    file: FileId
) -> Result<Ast, Vec<PlanktonError>> {
    let mut parser = Parser::new(source, file);
    parser.parse()
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: FileId) -> Self {
        Self { tokens , current: 0, file }
    }

    fn parse(&mut self) -> Result<Ast, Vec<PlanktonError>> {


        Ok(
            Ast {
                statements: vec![
                    Stmt {
                        kind: StmtKind::Declaration("a".to_string(), None, Some(Expr {
                            kind: ExprKind::Variable("b".to_string()),
                            span: Span::new(0, 0, 0)
                        })),
                        span: Span::new(0, 0, 0)
                    }
                ]
            }
        )
    }
}