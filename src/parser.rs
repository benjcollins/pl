use crate::{token::{Token, TokenKind}, ast::{Expr, BinaryOp, Ident, Stmt, Else, If, Block, Ty, Fun}};

pub fn parse(tokens: &[Token]) -> ParseResult<Fun> {
    let mut parser = Parser {
        index: 0,
        tokens,
    };
    parser.parse_fn()
}

struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
    Product,
    Sum,
    Compare,
    Bracket
}

#[derive(Debug)]
pub struct ParseError {
    token: Token,
}

type ParseResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    fn next(&mut self) {
        self.index += 1;
    }
    fn peek(&self) -> Token {
        self.tokens[self.index]
    }
    fn eat(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.peek().kind == kind {
            self.next();
            Ok(())
        } else {
            Err(self.unexpected_token())
        }
    }
    fn unexpected_token(&self) -> ParseError {
        ParseError { token: self.peek() }
    }
    fn parse_expr(&mut self, prec: Prec) -> ParseResult<Expr> {
        let token = self.peek();
        let mut left = match self.peek().kind {
            TokenKind::Ident => {
                self.next();
                Expr::Ident(Ident { start: token.start, end: token.end })
            }
            TokenKind::Integer => {
                self.next();
                Expr::Integer { start: token.start, end: token.end }
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
                let expr = self.parse_expr(Prec::Bracket)?;
                if self.peek().kind != TokenKind::CloseBrace { panic!() }
                self.next();
                expr
            }
            _ => Err(self.unexpected_token())?,
        };
        loop {
            left = match self.peek().kind {
                TokenKind::Plus if prec >= Prec::Sum => self.parse_infix(left, BinaryOp::Add, Prec::Sum)?,
                TokenKind::Minus if prec >= Prec::Sum => self.parse_infix(left, BinaryOp::Subtract, Prec::Sum)?,
                TokenKind::Asterisk if prec >= Prec::Product => self.parse_infix(left, BinaryOp::Multiply, Prec::Product)?,
                TokenKind::ForwardSlash if prec >= Prec::Product => self.parse_infix(left, BinaryOp::Divide, Prec::Product)?,
                TokenKind::OpenAngleBrace if prec >= Prec::Compare => self.parse_infix(left, BinaryOp::LessThan, Prec::Compare)?,
                TokenKind::CloseAngleBrace if prec >= Prec::Compare => self.parse_infix(left, BinaryOp::GreaterThan, Prec::Compare)?,
                _ => break
            }
        }
        Ok(left)
    }
    fn parse_infix(&mut self, left: Expr, op: BinaryOp, prec: Prec) -> ParseResult<Expr> {
        self.next();
        let right = self.parse_expr(prec)?;
        Ok(Expr::Binary {
            left: Box::new(left),
            right: Box::new(right),
            op,
        })
    }
    fn parse_if(&mut self) -> ParseResult<If> {
        let cond = Box::new(self.parse_expr(Prec::Bracket)?);
        let if_block = self.parse_block()?;
        let else_block = if self.peek().kind == TokenKind::Else {
            self.next();
            if self.peek().kind == TokenKind::If {
                Else::If(Box::new(self.parse_if()?))
            } else {
                Else::Block(self.parse_block()?)
            }
        } else {
            Else::None
        };
        Ok(If { cond, if_block, else_block })
    }
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        Ok(match self.peek().kind {
            TokenKind::If => {
                self.next();
                Stmt::If(self.parse_if()?)
            }
            TokenKind::While => {
                self.next();
                let cond = self.parse_expr(Prec::Bracket)?;
                let body = self.parse_block()?;
                Stmt::While { cond, body }
            }
            TokenKind::Let => {
                self.next();
                let token = self.peek();
                self.eat(TokenKind::Ident)?;
                let ident = Ident { start: token.start, end: token.end };

                let ty = if self.peek().kind == TokenKind::Colon {
                    self.next();
                    Some(self.parse_ty()?)
                } else {
                    None
                };
                let expr = if self.peek().kind == TokenKind::Equals {
                    self.next();
                    Some(self.parse_expr(Prec::Bracket)?)
                } else {
                    None
                };
                Stmt::Let { ident, expr, ty }
            }
            TokenKind::Return => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                Stmt::Return { expr }
            }
            TokenKind::Ident => {
                let ident = Ident { start: self.peek().start, end: self.peek().end };
                self.next();
                self.eat(TokenKind::Equals)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                Stmt::Assign { ident, expr }
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_ty(&mut self) -> ParseResult<Ty> {
        let token = self.peek();
        self.eat(TokenKind::Ident)?;
        let name = Ident { start: token.start, end: token.end };
        Ok(Ty { name })
    }
    fn parse_block(&mut self) -> ParseResult<Block> {
        self.eat(TokenKind::OpenCurlyBrace)?;
        let mut stmts = vec![];
        while self.peek().kind != TokenKind::CloseCurlyBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block { stmts })
    }
    fn parse_fn(&mut self) -> ParseResult<Fun> {
        self.eat(TokenKind::Fn)?;
        let token = self.peek();
        self.eat(TokenKind::Ident)?;
        let name = Ident { start: token.start, end: token.end };

        self.eat(TokenKind::OpenBrace)?;
        let params = vec![];
        self.eat(TokenKind::CloseBrace)?;
        let returns = if self.peek().kind != TokenKind::OpenCurlyBrace {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let block = self.parse_block()?;
        Ok(Fun { block, params, returns, name })
    }
}