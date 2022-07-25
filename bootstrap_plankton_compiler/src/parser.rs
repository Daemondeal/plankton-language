use crate::{
    ast::{Ast, Expr, ExprKind, LiteralKind, Operator, Stmt, StmtKind, TypeExpr, TypeExprKind},
    compiler::FileId,
    token::{Token, TokenKind},
    PlanktonError, Span,
};

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

pub fn parse_tokens(source: Vec<Token>, _file: FileId) -> Result<Ast, Vec<PlanktonError>> {
    let mut parser = Parser::new(source);
    parser.parse()
}

fn prefix_binding_power(op: &Operator) -> Option<((), u8)> {
    match op {
        Operator::Negate => Some(((), 110)),
        Operator::LogicNot => Some(((), 110)),
        Operator::Dereference => Some(((), 210)),
        Operator::GetAddress => Some(((), 210)),
        _ => None,
    }
}

fn infix_binding_power(op: &Operator) -> Option<(u8, u8)> {
    let res = match op {
        Operator::Get => (220, 230),
        Operator::Sum | Operator::Subtract => (40, 50),
        Operator::Multiply | Operator::Divide => (60, 70),
        Operator::GreaterThan
        | Operator::GreaterThanEqual
        | Operator::LessThan
        | Operator::LessThanEqual => (6, 7),
        Operator::LogicAnd => (20, 30),
        Operator::LogicOr => (10, 20),
        Operator::Equals | Operator::NotEquals => (4, 5),
        Operator::Assign => (2, 1),
        _ => return None,
    };

    Some(res)
}

fn postfix_binding_power(op: &Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::ProcedureCall => (210, ()),
        Operator::IndexAccess => (210, ()),
        _ => return None,
    };

    Some(res)
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // Utils

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind.is_same_variant(kind)
        }
    }

    fn check_either(&self, kinds: &[TokenKind]) -> bool {
        if self.is_at_end() {
            false
        } else {
            kinds.contains(&self.peek().kind)
        }
    }

    // TODO: Maybe we should handle calling this at start
    fn previous_token_span(&self) -> Span {
        self.tokens[self.current - 1].span
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if !self.check(kind) {
            false
        } else {
            self.advance();
            true
        }
    }

    #[allow(dead_code)]
    fn match_tokens(&mut self, token_kinds: &[TokenKind]) -> bool {
        for token in token_kinds {
            if self.match_token(token) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(
        &mut self,
        kind: &TokenKind,
        error: PlanktonError,
    ) -> Result<&Token, Vec<PlanktonError>> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(vec![error])
        }
    }

    fn consume_or_unexpected(&mut self, kind: &TokenKind) -> Result<&Token, Vec<PlanktonError>> {
        if self.check(kind) {
            Ok(self.advance())
        } else if self.is_at_end() {
            Err(vec![PlanktonError::ParserErrrorWithoutSpan(
                "End of file reached prematurely".to_string(),
            )])
        } else {
            Err(vec![PlanktonError::ParserError {
                message: format!("Unexpected token: Expected a {:?}", &kind),
                span: self.peek().span,
            }])
        }
    }

    // Parsing

    fn parse(&mut self) -> Result<Ast, Vec<PlanktonError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while self.peek().kind != TokenKind::Eof {
            // Ignore newlines
            if self.match_token(&TokenKind::LineEnd) {
                continue;
            }

            match self.statement() {
                Err(mut errs) => {
                    errors.append(&mut errs);

                    // Ignore all tokens until endline
                    while !self.check_either(&[TokenKind::LineEnd, TokenKind::Eof]) {
                        self.advance();
                    }
                }
                Ok(stmt) => statements.push(stmt),
            }
        }

        if errors.is_empty() {
            Ok(Ast { statements })
        } else {
            Err(errors)
        }
    }

    fn statement(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        // Ignore line ends
        while self.check(&TokenKind::LineEnd) {
            self.advance();
        }

        if self.match_token(&TokenKind::LeftBrace) {
            self.statement_block()
        } else if self.match_token(&TokenKind::Let) {
            self.statement_let_declaration()
        } else if self.match_token(&TokenKind::Return) {
            self.statement_return()
        } else if self.match_token(&TokenKind::Proc) {
            self.statement_procedure()
        } else {
            self.statement_expression()
        }
    }

    fn get_last_span(&self) -> Span {
        self.tokens[self.current - 1].span
    }

    fn consume_identifier(&mut self) -> Result<String, Vec<PlanktonError>> {
        let identifier = self.consume_or_unexpected(&TokenKind::Identifier("".to_string()))?;

        // FIXME: this is horrible, find a better way
        match &identifier.kind {
            TokenKind::Identifier(val) => Ok(val.clone()),
            _ => unreachable!(),
        }
    }

    fn statement_procedure(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        let proc_name = self.consume_identifier()?;
        let span = self.get_last_span();

        self.consume_or_unexpected(&TokenKind::LeftParen)?;

        let mut args = vec![];

        while !self.match_token(&TokenKind::RightParen) {
            let identifier = self.consume_identifier()?;
            self.consume_or_unexpected(&TokenKind::Colon)?;
            let typ = self.type_descriptor()?;

            args.push((identifier, typ));

            if self.peek().kind != TokenKind::RightParen {
                self.consume_or_unexpected(&TokenKind::Comma)?;
            }
        }

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.type_descriptor()?
        } else {
            TypeExpr::void(span)
        };

        let body = self.statement()?;

        // let proc_type = TypeExpr {
        //     kind: TypeExprKind::Procedure {
        //         return_type: Box::new(return_type.clone()),
        //         arguments: args.iter().cloned().map(|(_, typ)| typ).collect()
        //     },
        //     span,
        // };

        Ok(Stmt::new(
            StmtKind::Declaration(
                proc_name,
                None, // Some(proc_type),
                Some(Expr::new(
                    ExprKind::Procedure(args, return_type, Box::new(body)),
                    span,
                )),
            ),
            span,
        ))
    }

    fn statement_block(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        let span = self.tokens[self.current - 1].span;

        let mut errors = vec![];
        let mut body = vec![];

        while self.peek().kind != TokenKind::RightBrace {
            if self.peek().kind == TokenKind::Eof {
                return Err(vec![PlanktonError::ParserError {
                    message: "Unmatched brace".to_string(),
                    span,
                }]);
            } else if self.match_token(&TokenKind::LineEnd) {
                continue;
            }

            match self.statement() {
                Err(mut errs) => {
                    errors.append(&mut errs);

                    // Ignore all tokens until endline
                    while !self.check_either(&[TokenKind::LineEnd, TokenKind::RightBrace]) {
                        self.advance();
                    }
                }
                Ok(stmt) => body.push(stmt),
            }
        }

        if errors.is_empty() {
            self.advance();
            Ok(Stmt::new(StmtKind::Block(body), span))
        } else {
            Err(errors)
        }
    }

    fn statement_let_declaration(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        // let identifier = self.consume_or_unexpected(&TokenKind::Identifier("".to_string()))?;

        let var_name = self.consume_identifier()?;
        let span = self.tokens[self.current - 1].span;

        let typ = if self.match_token(&TokenKind::Colon) {
            Some(self.type_descriptor()?)
        } else {
            None
        };

        let initializer = if self.match_token(&TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        if typ.is_none() && initializer.is_none() {
            return Err(vec![PlanktonError::ParserError {
                message: "Not enough information to determine variable type. Consider adding an initializer or a type hint".to_string(), 
                span
            }]);
        }

        self.consume_or_unexpected(&TokenKind::LineEnd)?;

        Ok(Stmt::new(
            StmtKind::Declaration(var_name, typ, initializer),
            span,
        ))
    }

    fn statement_return(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        let expr = self.expression()?;
        let span = expr.span;

        self.consume_or_unexpected(&TokenKind::LineEnd)?;
        Ok(Stmt::new(StmtKind::Return(expr), span))
    }

    fn statement_expression(&mut self) -> Result<Stmt, Vec<PlanktonError>> {
        let expr = self.expression()?;
        let span = expr.span;

        self.match_token(&TokenKind::LineEnd);
        Ok(Stmt::new(StmtKind::Expression(expr), span))
    }

    fn expression(&mut self) -> Result<Expr, Vec<PlanktonError>> {
        self.expression_bp(0)
    }

    // Pratt parser implementation. See https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn expression_bp(&mut self, min_bp: u8) -> Result<Expr, Vec<PlanktonError>> {
        let token = self.peek();
        let span = token.span;

        let mut lhs = match token.kind {
            TokenKind::Minus | TokenKind::Not | TokenKind::Star | TokenKind::Ampersand => {
                let op = match token.kind {
                    TokenKind::Minus => Operator::Negate,
                    TokenKind::Not => Operator::LogicNot,
                    TokenKind::Star => Operator::Dereference,
                    TokenKind::Ampersand => Operator::GetAddress,
                    _ => unreachable!(),
                };

                let ((), r_bp) = prefix_binding_power(&op).unwrap();

                self.advance();

                let rhs = self.expression_bp(r_bp)?;
                Ok(Expr::new(ExprKind::Operation(op, vec![rhs]), span))
            }
            _ => self.primary(),
        }?;

        loop {
            let operator_token = self.peek();
            let op_span = operator_token.span;

            let operator = match operator_token.kind {
                TokenKind::Plus => Operator::Sum,
                TokenKind::Minus => Operator::Subtract,
                TokenKind::Star => Operator::Multiply,
                TokenKind::Slash => Operator::Divide,
                TokenKind::And => Operator::LogicAnd,
                TokenKind::Or => Operator::LogicOr,
                TokenKind::LeftParen => Operator::ProcedureCall,
                TokenKind::LeftBracket => Operator::IndexAccess,
                TokenKind::Less => Operator::LessThan,
                TokenKind::LessEqual => Operator::LessThanEqual,
                TokenKind::Greater => Operator::GreaterThan,
                TokenKind::GreaterEqual => Operator::GreaterThanEqual,
                TokenKind::EqualEqual => Operator::Equals,
                TokenKind::BangEqual => Operator::NotEquals,
                TokenKind::Equal => Operator::Assign,
                TokenKind::Dot => Operator::Get,
                _ => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(&operator) {
                if l_bp < min_bp {
                    break;
                }

                // Consume the operator
                self.advance();

                lhs = match operator {
                    Operator::IndexAccess => {
                        let rhs = self.expression_bp(0)?;

                        self.consume(
                            &TokenKind::RightBracket,
                            PlanktonError::ParserError {
                                message: "Unmatched Bracket".to_string(),
                                span: op_span,
                            },
                        )?;

                        Expr::new(
                            ExprKind::Operation(Operator::IndexAccess, vec![lhs, rhs]),
                            op_span,
                        )
                    }

                    Operator::ProcedureCall => {
                        if self.match_token(&TokenKind::RightParen) {
                            Expr::new(
                                ExprKind::Operation(Operator::ProcedureCall, vec![lhs]),
                                op_span,
                            )
                        } else {
                            let mut args = vec![lhs];

                            args.push(self.expression()?);

                            while self.match_token(&TokenKind::Comma) {
                                args.push(self.expression()?);
                            }

                            self.consume(
                                &TokenKind::RightParen,
                                PlanktonError::ParserError {
                                    message: "Unmatched Bracket".to_string(),
                                    span: op_span,
                                },
                            )?;

                            Expr::new(ExprKind::Operation(Operator::ProcedureCall, args), op_span)
                        }
                    }
                    _ => unreachable!(),
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(&operator) {
                if l_bp < min_bp {
                    break;
                }

                // Consume the operator
                self.advance();

                let rhs = self.expression_bp(r_bp)?;

                lhs = Expr::new(ExprKind::Operation(operator, vec![lhs, rhs]), op_span);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn primary(&mut self) -> Result<Expr, Vec<PlanktonError>> {
        let token = self.advance();

        match &token.kind {
            TokenKind::True => Ok(Expr::new(
                ExprKind::Literal(LiteralKind::Boolean(true)),
                token.span,
            )),
            TokenKind::False => Ok(Expr::new(
                ExprKind::Literal(LiteralKind::Boolean(false)),
                token.span,
            )),

            TokenKind::Integer32(val) => Ok(Expr::new(
                ExprKind::Literal(LiteralKind::Integer32(*val)),
                token.span,
            )),
            TokenKind::Float32(val) => Ok(Expr::new(
                ExprKind::Literal(LiteralKind::Float32(*val)),
                token.span,
            )),

            TokenKind::String(val) => Ok(Expr::new(
                ExprKind::Literal(LiteralKind::String(val.clone())),
                token.span,
            )),

            TokenKind::Identifier(name) => {
                Ok(Expr::new(ExprKind::Variable(name.clone()), token.span))
            }

            TokenKind::LeftParen => {
                let span = token.span;

                let expr = self.expression()?;

                self.consume(
                    &TokenKind::RightParen,
                    PlanktonError::ParserError {
                        message: "Unmatched parenthesis".to_string(),
                        span,
                    },
                )?;

                Ok(Expr::new(ExprKind::Grouping(Box::new(expr)), span))
            }

            TokenKind::If => self.expression_if(),

            TokenKind::While => self.expression_while(),

            _ => Err(vec![PlanktonError::ParserError {
                message: "Unexpected token".to_string(),
                span: token.span,
            }]),
        }
    }

    fn expression_if(&mut self) -> Result<Expr, Vec<PlanktonError>> {
        let span = self.previous_token_span();
        let condition = Box::new(self.expression()?);

        let then_body = Box::new(self.statement()?);

        let else_body = if self.match_token(&TokenKind::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Expr::new(
            ExprKind::If(condition, then_body, else_body),
            span,
        ))
    }

    fn expression_while(&mut self) -> Result<Expr, Vec<PlanktonError>> {
        let span = self.previous_token_span();
        let condition = Box::new(self.expression()?);

        let body = Box::new(self.statement()?);

        Ok(Expr::new(ExprKind::While(condition, body), span))
    }

    fn type_descriptor(&mut self) -> Result<TypeExpr, Vec<PlanktonError>> {
        let id = self.consume_identifier()?;
        let span = self.get_last_span();

        Ok(TypeExpr {
            span,
            kind: TypeExprKind::Builtin(id),
        })
    }
}
