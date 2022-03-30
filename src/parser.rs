use std::collections::HashMap;

use crate::{token::{Token, TokenKind}, ast::{Expr, InfixOp, Stmt, Else, If, Block, Ty, Func, PrefixOp, Assign, Param, FnCall, Struct, StructField, Program, StructValue}};

pub fn parse<'a>(tokens: &[Token], src: &'a str) -> ParseResult<Program<'a>> {
    let mut parser = Parser {
        index: 0,
        tokens,
        src,
    };
    parser.parse_file()
}

struct Parser<'a, 'b> {
    tokens: &'b [Token],
    index: usize,
    src: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
    Dot,
    Ref,
    Product,
    Sum,
    Compare,
    Bracket
}

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
}

type ParseResult<T> = Result<T, ParseError>;

impl<'a, 'b> Parser<'a, 'b> {
    fn next(&mut self) -> Token {
        let token = self.tokens[self.index];
        self.index += 1;
        token
    }
    fn peek(&self) -> TokenKind {
        self.tokens[self.index].kind
    }
    fn eat_or_err(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.peek() == kind {
            Ok(self.next())
        } else {
            Err(self.unexpected_token())
        }
    }
    fn end(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn unexpected_token(&self) -> ParseError {
        ParseError { token: self.tokens[self.index] }
    }
    fn parse_list<T>(&mut self, sep: TokenKind, term: TokenKind, f: impl Fn(&mut Parser<'a, 'b>) -> ParseResult<T>) -> ParseResult<Vec<T>> {
        let mut items = vec![];
        if self.peek() != term {
            items.push(f(self)?);
            while self.peek() == sep {
                self.next();
                items.push(f(self)?);
            }
        }
        self.eat_or_err(term)?;
        Ok(items)
    }
    fn parse_expr(&mut self, prec: Prec) -> ParseResult<Expr<'a>> {
        let mut left = match self.peek() {
            TokenKind::Asterisk => self.parse_prefix(PrefixOp::Deref, Prec::Ref)?,
            TokenKind::Ampersand => self.parse_prefix(PrefixOp::Ref, Prec::Ref)?,

            TokenKind::Ident => {
                let name = self.next().as_str(self.src);
                match self.peek() {
                    TokenKind::OpenBrace => {
                        self.next();
                        let args = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| parser.parse_expr(Prec::Bracket))?;
                        Expr::FnCall(FnCall { name, args })
                    }
                    TokenKind::OpenCurlyBrace => {
                        self.next();
                        let values = self.parse_list(TokenKind::Comma, TokenKind::CloseCurlyBrace, |parser| {
                            let name = parser.eat_or_err(TokenKind::Ident)?.as_str(self.src);
                            parser.eat_or_err(TokenKind::Colon)?;
                            let expr = parser.parse_expr(Prec::Bracket)?;
                            Ok(StructValue { name, expr })
                        })?;
                        Expr::InitStruct { name, values }
                    }
                    _ => Expr::Ident(name)
                }
            }
            TokenKind::Integer => {
                Expr::Integer(self.next().as_str(self.src))
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
                self.eat_or_err(TokenKind::CloseBrace)?;
                self.next();
                expr
            }
            _ => Err(self.unexpected_token())?,
        };
        loop {
            left = match self.peek() {
                TokenKind::Plus if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Add, Prec::Sum)?,
                TokenKind::Minus if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Subtract, Prec::Sum)?,
                TokenKind::Asterisk if prec >= Prec::Product => self.parse_infix(left, InfixOp::Multiply, Prec::Product)?,
                TokenKind::ForwardSlash if prec >= Prec::Product => self.parse_infix(left, InfixOp::Divide, Prec::Product)?,
                TokenKind::OpenAngleBrace if prec >= Prec::Compare => self.parse_infix(left, InfixOp::LessThan, Prec::Compare)?,
                TokenKind::CloseAngleBrace if prec >= Prec::Compare => self.parse_infix(left, InfixOp::GreaterThan, Prec::Compare)?,
                TokenKind::Dot if prec >= Prec::Dot => {
                    self.next();
                    let name = self.eat_or_err(TokenKind::Ident)?.as_str(self.src);
                    Expr::Field { expr: Box::new(left), name }
                }
                _ => break
            }
        }
        Ok(left)
    }
    fn parse_prefix(&mut self, op: PrefixOp, prec: Prec) -> ParseResult<Expr<'a>> {
        self.next();
        let expr = Box::new(self.parse_expr(prec)?);
        Ok(Expr::Prefix { op, expr })
    }
    fn parse_infix(&mut self, left: Expr<'a>, op: InfixOp, prec: Prec) -> ParseResult<Expr<'a>> {
        self.next();
        let right = self.parse_expr(prec)?;
        Ok(Expr::Infix {
            left: Box::new(left),
            right: Box::new(right),
            op,
        })
    }
    fn parse_if(&mut self) -> ParseResult<If<'a>> {
        let cond = Box::new(self.parse_expr(Prec::Bracket)?);
        let if_block = self.parse_block()?;
        let else_block = if self.peek() == TokenKind::Else {
            self.next();
            if self.peek() == TokenKind::If {
                Else::If(Box::new(self.parse_if()?))
            } else {
                Else::Block(self.parse_block()?)
            }
        } else {
            Else::None
        };
        Ok(If { cond, if_block, else_block })
    }
    fn parse_assign(&mut self) -> ParseResult<Assign<'a>> {
        Ok(match self.peek() {
            TokenKind::Asterisk => {
                self.next();
                Assign::Deref(Box::new(self.parse_assign()?))
            }
            TokenKind::Ident => {
                Assign::Name(self.next().as_str(self.src))
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        Ok(match self.peek() {
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
            TokenKind::Var => {
                self.next();
                let ident = self.eat_or_err(TokenKind::Ident)?.as_str(self.src);

                let ty = if self.peek() == TokenKind::Colon {
                    self.next();
                    Some(self.parse_ty()?)
                } else {
                    None
                };
                let expr = if self.peek() == TokenKind::Equals {
                    self.next();
                    Some(self.parse_expr(Prec::Bracket)?)
                } else {
                    None
                };
                self.eat_or_err(TokenKind::Semicolon)?;
                Stmt::Let { ident, expr, ty }
            }
            TokenKind::Return => {
                self.next();
                let expr = if self.peek() == TokenKind::Semicolon {
                    None
                } else {
                    Some(self.parse_expr(Prec::Bracket)?)
                };
                self.eat_or_err(TokenKind::Semicolon)?;
                Stmt::Return(expr)
            }
            TokenKind::Ident => {
                let name = self.next().as_str(self.src);
                let stmt = if self.peek() == TokenKind::OpenBrace {
                    self.next();
                    let args = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| parser.parse_expr(Prec::Bracket))?;
                    Stmt::FnCall(FnCall { name, args })
                } else {
                    self.eat_or_err(TokenKind::Equals)?;
                    let expr = self.parse_expr(Prec::Bracket)?;
                    Stmt::Assign { assign: Assign::Name(name), expr }
                };
                self.eat_or_err(TokenKind::Semicolon)?;
                stmt
            }
            TokenKind::Asterisk => {
                let assign = self.parse_assign()?;
                self.eat_or_err(TokenKind::Equals)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.eat_or_err(TokenKind::Semicolon)?;
                Stmt::Assign { assign, expr }
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_ty(&mut self) -> ParseResult<Ty<'a>> {
        Ok(match self.peek() {
            TokenKind::Ampersand => {
                self.next();
                Ty::Ref(Box::new(self.parse_ty()?))
            }
            TokenKind::Ident => {
                Ty::Name(self.next().as_str(self.src))
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_block(&mut self) -> ParseResult<Block<'a>> {
        self.eat_or_err(TokenKind::OpenCurlyBrace)?;
        let mut stmts = vec![];
        while self.peek() != TokenKind::CloseCurlyBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block { stmts })
    }
    fn parse_func(&mut self) -> ParseResult<Func<'a>> {
        self.eat_or_err(TokenKind::OpenBrace)?;
        let params = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| {
            let name = parser.eat_or_err(TokenKind::Ident)?.as_str(self.src);
            parser.eat_or_err(TokenKind::Colon)?;
            let ty = parser.parse_ty()?;
            Ok(Param { name, ty })
        })?;
        let returns = if self.peek() == TokenKind::Colon {
            self.next();
            Some(self.parse_ty()?)
        } else {
            None
        };
        let body = if self.peek() == TokenKind::OpenCurlyBrace {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Func { body, params, returns })
    }
    fn parse_struct(&mut self) -> ParseResult<Struct<'a>> {
        self.eat_or_err(TokenKind::OpenCurlyBrace)?;
        let fields = self.parse_list(TokenKind::Comma, TokenKind::CloseCurlyBrace, |parser| {
            let name = parser.eat_or_err(TokenKind::Ident)?.as_str(self.src);
            parser.eat_or_err(TokenKind::Colon)?;
            let ty = parser.parse_ty()?;
            Ok(StructField { name, ty })
        })?;
        Ok(Struct { fields })
    }
    fn parse_file(&mut self) -> ParseResult<Program<'a>> {
        let mut file = Program {
            funcs: HashMap::new(),
            structs: HashMap::new(),
        };
        while !self.end() {
            match self.peek() {
                TokenKind::Func => {
                    self.next();
                    let name = self.eat_or_err(TokenKind::Ident)?.as_str(self.src);
                    file.funcs.insert(name, self.parse_func()?);
                }
                TokenKind::Struct => {
                    self.next();
                    let name = self.eat_or_err(TokenKind::Ident)?.as_str(self.src);
                    file.structs.insert(name, self.parse_struct()?);
                }
                _ => Err(self.unexpected_token())?,
            }
        }
        Ok(file)
    }
}