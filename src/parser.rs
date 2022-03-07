use crate::{lexer::Lexer, token::{TokenKind, Token}, ast::{Expr, BinaryOp, Ident, Stmt, Fun, Ty, Block, Else, If}};

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    token: Token,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Prec {
    Product,
    Sum,
    Bracket
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Parser<'src> {
        let mut lexer = Lexer::new(source);
        let token = lexer.next();
        Parser { lexer, token }
    }
    fn next(&mut self) {
        self.token = self.lexer.next();
    }
    pub fn parse_expr(&mut self, prec: Prec) -> Expr {
        let token = self.token;
        let mut left = match self.token.kind {
            TokenKind::Ident => {
                self.next();
                Expr::Ident(Ident { offset: token.offset })
            }
            TokenKind::Integer => {
                self.next();
                Expr::Integer { offset: token.offset }
            }
            TokenKind::True => {
                self.next();
                Expr::Bool(true)
            }
            TokenKind::False => {
                self.next();
                Expr::Bool(false)
            }
            TokenKind::OpenBrace => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket);
                if self.token.kind != TokenKind::CloseBrace { panic!() }
                self.next();
                expr
            }
            _ => panic!("{:?}", self.token.kind),
        };
        loop {
            left = match self.token.kind {
                TokenKind::Plus if prec >= Prec::Sum => self.parse_infix(left, BinaryOp::Add, Prec::Sum),
                TokenKind::Minus if prec >= Prec::Sum => self.parse_infix(left, BinaryOp::Subtract, Prec::Sum),
                TokenKind::Asterisk if prec >= Prec::Product => self.parse_infix(left, BinaryOp::Multiply, Prec::Product),
                TokenKind::ForwardSlash if prec >= Prec::Product => self.parse_infix(left, BinaryOp::Divide, Prec::Product),
                _ => break
            }
        }
        left
    }
    pub fn parse_infix(&mut self, left: Expr, op: BinaryOp, prec: Prec) -> Expr {
        self.next();
        let right = self.parse_expr(prec);
        Expr::Binary {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }
    fn parse_if(&mut self) -> If {
        let cond = Box::new(self.parse_expr(Prec::Bracket));
        let if_block = self.parse_block();
        let else_block = if self.token.kind == TokenKind::Else {
            self.next();
            if self.token.kind == TokenKind::If {
                Else::If(Box::new(self.parse_if()))
            } else {
                Else::Block(self.parse_block())
            }
        } else {
            Else::None
        };
        If { cond, if_block, else_block }
    }
    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.token.kind {
            TokenKind::If => {
                self.next();
                Some(Stmt::If(self.parse_if()))
            }
            TokenKind::Let => {
                self.next();
                if self.token.kind != TokenKind::Ident { panic!() }
                let ident = Ident { offset: self.token.offset };
                self.next();
                let ty = if self.token.kind == TokenKind::Colon {
                    self.next();
                    Some(self.parse_ty())
                } else {
                    None
                };
                let expr = if self.token.kind == TokenKind::Equals {
                    self.next();
                    Some(self.parse_expr(Prec::Bracket))
                } else {
                    None
                };
                if self.token.kind != TokenKind::Semicolon { panic!() }
                self.next();
                Some(Stmt::Let { ident, expr, ty })
            }
            TokenKind::Return => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket);
                if self.token.kind != TokenKind::CloseCurlyBrace { panic!() }
                Some(Stmt::Return { expr })
            }
            TokenKind::Ident => {
                let ident = Ident { offset: self.token.offset };
                self.next();
                if self.token.kind != TokenKind::Equals { panic!() }
                self.next();
                let expr = self.parse_expr(Prec::Bracket);
                if self.token.kind != TokenKind::Semicolon { panic!() }
                self.next();
                Some(Stmt::Assign { ident, expr })
            }
            _ => None,
        }
    }
    pub fn parse_ty(&mut self) -> Ty {
        if self.token.kind != TokenKind::Ident { panic!() };
        let name = Ident { offset: self.token.offset };
        self.next();
        Ty { name }
    }
    pub fn parse_block(&mut self) -> Block {
        if self.token.kind != TokenKind::OpenCurlyBrace { panic!() }
        self.next();
        let mut stmts = vec![];
        let mut result = None;
        while self.token.kind != TokenKind::CloseCurlyBrace {
            if self.token.kind == TokenKind::End { panic!() }
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                result = Some(Box::new(self.parse_expr(Prec::Bracket)));
                if self.token.kind != TokenKind::CloseCurlyBrace { panic!() }
            }
        }
        self.next();
        Block { stmts, result }
    }
    pub fn parse_fn(&mut self) -> Fun {
        if self.token.kind != TokenKind::Fn { panic!() };
        self.next();
        if self.token.kind != TokenKind::Ident { panic!() };
        let name = Ident { offset: self.token.offset };
        self.next();
        if self.token.kind != TokenKind::OpenBrace { panic!() };
        self.next();
        let params = vec![];
        if self.token.kind != TokenKind::CloseBrace { panic!() };
        self.next();
        let returns = if self.token.kind != TokenKind::OpenCurlyBrace {
            Some(self.parse_ty())
        } else {
            None
        };
        let block = self.parse_block();
        Fun { block, params, returns, name }
    }
}

#[test]
fn test_parse_expr() {
    let mut parser = Parser::new("a * 3 + 2");
    let expr = parser.parse_expr(Prec::Bracket);
    assert_eq!(expr, Expr::Binary {
        left: Box::new(Expr::Binary {
            left: Box::new(Expr::Ident(Ident { offset: 0 })),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Integer { offset: 4 })
        }),
        op: BinaryOp::Add,
        right: Box::new(Expr::Integer { offset: 8 })
    });
}

#[test]
fn test_parse_bracket() {
    let mut parser = Parser::new("a * (3 + 2)");
    let expr = parser.parse_expr(Prec::Bracket);
    assert_eq!(expr, Expr::Binary {
        left: Box::new(Expr::Ident(Ident { offset: 0 })),
        op: BinaryOp::Multiply,
        right: Box::new(Expr::Binary {
            left: Box::new(Expr::Integer { offset: 5 }),
            op: BinaryOp::Add,
            right: Box::new(Expr::Integer { offset: 9 })
        }),
    });
}

#[test]
fn test_parse_assign() {
    let mut parser = Parser::new("x = 2 - 3");
    let stmt = parser.parse_stmt();
    assert_eq!(stmt, Some(Stmt::Assign {
        ident: Ident { offset: 0 },
        expr: Expr::Binary {
            left: Box::new(Expr::Integer { offset: 4 }),
            op: BinaryOp::Subtract,
            right: Box::new(Expr::Integer { offset: 8 }),
        }
    }));
}

#[test]
fn test_parse_fn() {
    let mut parser = Parser::new("fn test() { x = 2 }");
    let fun = parser.parse_fn();
    assert_eq!(Token::new(fun.name.offset, TokenKind::Ident).as_str(parser.lexer.src()), "test");
    assert!(fun.params.is_empty());
    assert!(fun.returns.is_none());
    assert_eq!(fun.block.stmts.len(), 1);
}