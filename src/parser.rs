use std::{collections::HashMap, fmt};

use crate::{token::{Token, TokenKind, Symbol, Keyword}, ast::{Expr, InfixOp, Stmt, Else, If, Block, Ty, Func, PrefixOp, Param, FuncCall, Struct, StructField, Program, StructValue, Int, RefExpr}, symbols::Symbols};

pub fn parse<'a, 'b>(tokens: &'b [Token], src: &'a str) -> ParseResult<'a, (Program, Symbols<'a>)> {
    let mut parser = Parser {
        index: 0,
        tokens,
        source: src,
        symbols: Symbols::new(),
    };
    Ok((parser.parse_file()?, parser.symbols))
}

struct Parser<'a, 'b> {
    tokens: &'b [Token],
    index: usize,
    source: &'a str,
    symbols: Symbols<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
    Dot,
    Ref,
    Product,
    Sum,
    Compare,
    Bracket,
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub token: Token,
    pub source: &'a str,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pos = self.token.pos(self.source);
        let str = self.token.str(self.source);
        write!(f, "syntax error on line {} at token '{}'", pos.line, str)
    }
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl<'a, 'b> Parser<'a, 'b> {
    fn peek(&self) -> TokenKind {
        self.tokens[self.index].kind
    }
    fn next(&mut self) -> Token {
        let token = self.tokens[self.index];
        self.index += 1;
        token
    }
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek() == kind {
            self.next();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, kind: TokenKind) -> ParseResult<'a, Token> {
        if self.peek() == kind {
            Ok(self.next())
        } else {
            Err(self.unexpected_token())
        }
    }
    fn end(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn unexpected_token(&self) -> ParseError<'a> {
        ParseError { token: self.tokens[self.index], source: self.source }
    }
    fn parse_list<T>(&mut self, sep: TokenKind, term: TokenKind, f: impl Fn(&mut Parser<'a, 'b>) -> ParseResult<'a, T>) -> ParseResult<'a, Vec<T>> {
        let mut items = vec![];
        if self.peek() != term {
            items.push(f(self)?);
            while self.peek() == sep {
                self.next();
                items.push(f(self)?);
            }
        }
        self.expect(term)?;
        Ok(items)
    }
    fn parse_ref_expr(&mut self) -> ParseResult<'a, RefExpr> {
        let value = match self.peek() {
            TokenKind::Symbol(Symbol::Asterisk) => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                RefExpr::Deref(expr)
            }
            TokenKind::Symbol(Symbol::OpenBrace) => {
                self.next();
                let ref_expr = self.parse_ref_expr()?;
                self.expect(TokenKind::Symbol(Symbol::CloseBrace))?;
                ref_expr
            }
            TokenKind::Ident => {
                let ident = self.next().str(self.source);
                RefExpr::Ident(self.symbols.get_symbol(ident))
            }
            _ => Err(self.unexpected_token())?
        };
        self.parse_ref_expr_fields(value)
    }
    fn parse_ref_expr_fields(&mut self, mut left: RefExpr) -> ParseResult<'a, RefExpr> {
        loop {
            match self.peek() {
                TokenKind::Symbol(Symbol::Dot) => {
                    self.next();
                    let ident = self.expect(TokenKind::Ident)?.str(self.source);
                    let name = self.symbols.get_symbol(ident);
                    left = RefExpr::Field { ref_expr: Box::new(left), name };
                }
                _ => break Ok(left)
            }
        }
    }
    fn parse_expr(&mut self, prec: Prec) -> ParseResult<'a, Expr> {
        let mut left = match self.peek() {
            TokenKind::Symbol(Symbol::Asterisk) => self.parse_prefix(PrefixOp::Deref, Prec::Ref)?,
            TokenKind::Symbol(Symbol::Ampersand) => {
                self.next();
                Expr::Ref(Box::new(self.parse_ref_expr()?))
            }

            TokenKind::Ident => {
                let name = self.next().str(self.source);
                let symbol = self.symbols.get_symbol(name);
                match self.peek() {
                    TokenKind::Symbol(Symbol::OpenBrace) => {
                        self.next();
                        let args = self.parse_list(TokenKind::Symbol(Symbol::Comma), TokenKind::Symbol(Symbol::CloseBrace), |parser| parser.parse_expr(Prec::Bracket))?;
                        Expr::FuncCall(FuncCall { name: symbol, args })
                    }
                    TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                        self.next();
                        let values = self.parse_list(TokenKind::Symbol(Symbol::Comma), TokenKind::Symbol(Symbol::CloseCurlyBrace), |parser| {
                            let name = parser.expect(TokenKind::Ident)?.str(self.source);
                            let symbol = parser.symbols.get_symbol(name);
                            parser.expect(TokenKind::Symbol(Symbol::Colon))?;
                            let expr = parser.parse_expr(Prec::Bracket)?;
                            Ok(StructValue { name: symbol, expr })
                        })?;
                        Expr::InitStruct { name: symbol, values }
                    }
                    _ => Expr::Ident(symbol)
                }
            }
            TokenKind::Integer => {
                Expr::Integer(self.next().str(self.source).parse().unwrap())
            }
            TokenKind::Keyword(Keyword::True) => {
                self.next();
                Expr::Bool(true)
            }
            TokenKind::Keyword(Keyword::False) => {
                self.next();
                Expr::Bool(false)
            }
            TokenKind::Symbol(Symbol::OpenBrace) => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect(TokenKind::Symbol(Symbol::CloseBrace))?;
                self.next();
                expr
            }
            _ => Err(self.unexpected_token())?,
        };
        loop {
            left = match self.peek() {
                TokenKind::Symbol(Symbol::Plus) if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Add, Prec::Sum)?,
                TokenKind::Symbol(Symbol::Minus) if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Subtract, Prec::Sum)?,
                TokenKind::Symbol(Symbol::Asterisk) if prec >= Prec::Product => self.parse_infix(left, InfixOp::Multiply, Prec::Product)?,
                TokenKind::Symbol(Symbol::ForwardSlash) if prec >= Prec::Product => self.parse_infix(left, InfixOp::Divide, Prec::Product)?,
                TokenKind::Symbol(Symbol::OpenAngleBrace) if prec >= Prec::Compare => self.parse_infix(left, InfixOp::LessThan, Prec::Compare)?,
                TokenKind::Symbol(Symbol::CloseAngleBrace) if prec >= Prec::Compare => self.parse_infix(left, InfixOp::GreaterThan, Prec::Compare)?,
                TokenKind::Symbol(Symbol::Dot) if prec >= Prec::Dot => {
                    self.next();
                    let name = self.expect(TokenKind::Ident)?.str(self.source);
                    let symbol = self.symbols.get_symbol(name);
                    Expr::Field { expr: Box::new(left), name: symbol }
                }
                _ => break
            }
        }
        Ok(left)
    }
    fn parse_prefix(&mut self, op: PrefixOp, prec: Prec) -> ParseResult<'a, Expr> {
        self.next();
        let expr = Box::new(self.parse_expr(prec)?);
        Ok(Expr::Prefix { op, expr })
    }
    fn parse_infix(&mut self, left: Expr, op: InfixOp, prec: Prec) -> ParseResult<'a, Expr> {
        self.next();
        let right = self.parse_expr(prec)?;
        Ok(Expr::Infix {
            left: Box::new(left),
            right: Box::new(right),
            op,
        })
    }
    fn parse_if(&mut self) -> ParseResult<'a, If> {
        let cond = Box::new(self.parse_expr(Prec::Bracket)?);
        let if_block = self.parse_block()?;
        let else_block = if self.peek() == TokenKind::Keyword(Keyword::Else) {
            self.next();
            if self.peek() == TokenKind::Keyword(Keyword::If) {
                Else::If(Box::new(self.parse_if()?))
            } else {
                Else::Block(self.parse_block()?)
            }
        } else {
            Else::None
        };
        Ok(If { cond, if_block, else_block })
    }
    fn parse_stmt(&mut self) -> ParseResult<'a, Stmt> {
        Ok(match self.peek() {
            TokenKind::Keyword(Keyword::If) => {
                self.next();
                Stmt::If(self.parse_if()?)
            }
            TokenKind::Keyword(Keyword::While) => {
                self.next();
                let cond = self.parse_expr(Prec::Bracket)?;
                let body = self.parse_block()?;
                Stmt::While { cond, body }
            }
            TokenKind::Keyword(Keyword::Var) => {
                self.next();
                let name = self.expect(TokenKind::Ident)?.str(self.source);
                let symbol = self.symbols.get_symbol(name);

                let ty = if self.peek() == TokenKind::Symbol(Symbol::Colon) {
                    self.next();
                    Some(self.parse_ty()?)
                } else {
                    None
                };
                let expr = if self.peek() == TokenKind::Symbol(Symbol::Equals) {
                    self.next();
                    Some(self.parse_expr(Prec::Bracket)?)
                } else {
                    None
                };
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Let { ident: symbol, expr, ty }
            }
            TokenKind::Keyword(Keyword::Return) => {
                self.next();
                let expr = if self.peek() == TokenKind::Symbol(Symbol::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr(Prec::Bracket)?)
                };
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Return(expr)
            }
            TokenKind::Ident => {
                let name = self.next().str(self.source);
                let symbol = self.symbols.get_symbol(name);
                let stmt = if self.peek() == TokenKind::Symbol(Symbol::OpenBrace) {
                    self.next();
                    let args = self.parse_list(TokenKind::Symbol(Symbol::Comma), TokenKind::Symbol(Symbol::CloseBrace), |parser| parser.parse_expr(Prec::Bracket))?;
                    Stmt::FuncCall(FuncCall { name: symbol, args })
                } else {
                    let ref_expr = self.parse_ref_expr_fields(RefExpr::Ident(symbol))?;
                    self.expect(TokenKind::Symbol(Symbol::Equals))?;
                    let expr = self.parse_expr(Prec::Bracket)?;
                    Stmt::Assign { ref_expr, expr }
                };
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                stmt
            }
            TokenKind::Symbol(Symbol::Asterisk) => {
                let ref_expr = self.parse_ref_expr()?;
                self.expect(TokenKind::Symbol(Symbol::Equals))?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Assign { ref_expr, expr }
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_basic_ty(&mut self, ty: Ty) -> Ty {
        self.next();
        ty
    }
    fn parse_ty(&mut self) -> ParseResult<'a, Ty> {
        Ok(match self.peek() {
            TokenKind::Keyword(Keyword::I8) => self.parse_basic_ty(Ty::Int(Int::I8)),
            TokenKind::Keyword(Keyword::I16) => self.parse_basic_ty(Ty::Int(Int::I16)),
            TokenKind::Keyword(Keyword::I32) => self.parse_basic_ty(Ty::Int(Int::I32)),

            TokenKind::Keyword(Keyword::U8) => self.parse_basic_ty(Ty::Int(Int::U8)),
            TokenKind::Keyword(Keyword::U16) => self.parse_basic_ty(Ty::Int(Int::U16)),
            TokenKind::Keyword(Keyword::U32) => self.parse_basic_ty(Ty::Int(Int::U32)),

            TokenKind::Keyword(Keyword::Bool) => self.parse_basic_ty(Ty::Bool),

            TokenKind::Symbol(Symbol::Asterisk) => {
                self.next();
                Ty::Ref(Box::new(self.parse_ty()?))
            }
            TokenKind::Ident => {
                let name = self.next().str(self.source);
                Ty::Struct(self.symbols.get_symbol(name))
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_block(&mut self) -> ParseResult<'a, Block> {
        self.expect(TokenKind::Symbol(Symbol::OpenCurlyBrace))?;
        let mut stmts = vec![];
        while self.peek() != TokenKind::Symbol(Symbol::CloseCurlyBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block { stmts })
    }
    fn parse_func(&mut self) -> ParseResult<'a, Func> {
        self.expect(TokenKind::Symbol(Symbol::OpenBrace))?;
        let params = self.parse_list(TokenKind::Symbol(Symbol::Comma), TokenKind::Symbol(Symbol::CloseBrace), |parser| {
            let name = parser.expect(TokenKind::Ident)?.str(self.source);
            let symbol = parser.symbols.get_symbol(name);
            parser.expect(TokenKind::Symbol(Symbol::Colon))?;
            let ty = parser.parse_ty()?;
            Ok(Param { name: symbol, ty })
        })?;
        let returns = if self.eat(TokenKind::Symbol(Symbol::Colon)) {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let body = if self.peek() == TokenKind::Symbol(Symbol::OpenCurlyBrace) {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Func { body, params, returns })
    }
    fn parse_struct(&mut self) -> ParseResult<'a, Struct> {
        self.expect(TokenKind::Symbol(Symbol::OpenCurlyBrace))?;
        let fields = self.parse_list(TokenKind::Symbol(Symbol::Comma), TokenKind::Symbol(Symbol::CloseCurlyBrace), |parser| {
            let name = parser.expect(TokenKind::Ident)?.str(self.source);
            let symbol = parser.symbols.get_symbol(name);
            parser.expect(TokenKind::Symbol(Symbol::Colon))?;
            let ty = parser.parse_ty()?;
            Ok(StructField { name: symbol, ty })
        })?;
        Ok(Struct { fields })
    }
    fn parse_file(&mut self) -> ParseResult<'a, Program> {
        let mut file = Program {
            funcs: HashMap::new(),
            structs: HashMap::new(),
        };
        while !self.end() {
            match self.peek() {
                TokenKind::Keyword(Keyword::Func) => {
                    self.next();
                    let name = self.expect(TokenKind::Ident)?.str(self.source);
                    let symbol = self.symbols.get_symbol(name);
                    file.funcs.insert(symbol, self.parse_func()?);
                }
                TokenKind::Keyword(Keyword::Struct) => {
                    self.next();
                    let name = self.expect(TokenKind::Ident)?.str(self.source);
                    let symbol = self.symbols.get_symbol(name);
                    file.structs.insert(symbol, self.parse_struct()?);
                }
                _ => Err(self.unexpected_token())?,
            }
        }
        Ok(file)
    }
}