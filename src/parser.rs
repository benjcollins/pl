use std::fmt;

use crate::{
    ast::{
        Block, Decl, Else, Expr, Func, FuncCall, If, InfixOp, Int, Param, PrefixOp, Program,
        RefExpr, Stmt, Struct, StructField, StructValue, Ty,
    },
    symbols::Symbols,
    token::{Keyword, Symbol, Token, TokenKind},
    tokens::{TokenIter, Tokens},
};

pub fn parse<'s, 't>(tokens: &Tokens<'s>) -> (Program, Symbols<'s>, Vec<ParseError<'s>>) {
    let mut token_iter = tokens.iter();
    let token = token_iter.next();
    let mut parser = Parser {
        token,
        token_iter,
        symbols: Symbols::new(),
        handled_errors: vec![],
    };
    (
        parser.parse_program(),
        parser.symbols,
        parser.handled_errors,
    )
}

struct Parser<'s, 't> {
    token_iter: TokenIter<'t, 's>,
    token: Option<Token<'s>>,
    symbols: Symbols<'s>,
    handled_errors: Vec<ParseError<'s>>,
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

#[derive(Debug, Clone, Copy)]
pub struct ParseError<'s> {
    pub token: Option<Token<'s>>,
    pub expected: Expected,
}

#[derive(Debug, Clone, Copy)]
pub enum Expected {
    Token(TokenKind),
    Expr,
    Stmt,
    Type,
    RefExpr,
    Decl,
}

impl<'s> fmt::Display for ParseError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(token) = self.token {
            write!(
                f,
                "syntax error on line {} at token '{}'",
                token.pos().line,
                token.str()
            )?;
        } else {
            write!(f, "syntax error, unexpected end of file")?;
        }
        write!(f, " expected ")?;
        match self.expected {
            Expected::Token(token) => match token {
                TokenKind::Ident => write!(f, "an identifier"),
                TokenKind::Integer => write!(f, "an integer"),
                TokenKind::Keyword(keyword) => write!(f, "the keyword '{}'", keyword.str()),
                TokenKind::Symbol(symbol) => write!(f, "the symbol '{}'", symbol.str()),
            },
            Expected::Expr => write!(f, "an expression"),
            Expected::Stmt => write!(f, "a statement"),
            Expected::Type => write!(f, "a type"),
            Expected::RefExpr => write!(f, "a reference expression"),
            Expected::Decl => write!(f, "a top level declaration"),
        }
    }
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl<'s, 't> Parser<'s, 't> {
    fn peek(&self) -> Option<TokenKind> {
        self.token.map(|token| token.kind)
    }
    fn next(&mut self) -> Token<'s> {
        let last_token = self.token;
        self.token = self.token_iter.next();
        last_token.unwrap()
    }
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek() == Some(kind) {
            self.next();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, kind: TokenKind) -> ParseResult<'s, Token<'s>> {
        if self.peek() == Some(kind) {
            Ok(self.next())
        } else {
            Err(self.unexpected_token(Expected::Token(kind)))
        }
    }
    fn unexpected_token(&self, expected: Expected) -> ParseError<'s> {
        ParseError {
            token: self.token,
            expected,
        }
    }
    fn parse_list<T>(
        &mut self,
        sep: TokenKind,
        term: TokenKind,
        f: impl Fn(&mut Parser<'s, 't>) -> ParseResult<'s, T>,
    ) -> ParseResult<'s, Vec<T>> {
        let mut items = vec![];
        if self.peek() != Some(term) {
            items.push(f(self)?);
            while self.eat(sep) {
                items.push(f(self)?);
            }
        }
        self.expect(term)?;
        Ok(items)
    }
    fn parse_ref_expr(&mut self) -> ParseResult<'s, RefExpr> {
        let value = match self.peek() {
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                RefExpr::Deref(expr)
            }
            Some(TokenKind::Symbol(Symbol::OpenBrace)) => {
                self.next();
                let ref_expr = self.parse_ref_expr()?;
                self.expect(TokenKind::Symbol(Symbol::CloseBrace))?;
                ref_expr
            }
            Some(TokenKind::Ident) => {
                let ident = self.next().str();
                RefExpr::Ident(self.symbols.get_symbol(ident))
            }
            _ => Err(self.unexpected_token(Expected::RefExpr))?,
        };
        self.parse_ref_expr_fields(value)
    }
    fn parse_ref_expr_fields(&mut self, mut left: RefExpr) -> ParseResult<'s, RefExpr> {
        loop {
            match self.peek() {
                Some(TokenKind::Symbol(Symbol::Dot)) => {
                    self.next();
                    let ident = self.expect(TokenKind::Ident)?.str();
                    let name = self.symbols.get_symbol(ident);
                    left = RefExpr::Field {
                        ref_expr: Box::new(left),
                        name,
                    };
                }
                _ => break Ok(left),
            }
        }
    }
    fn parse_expr(&mut self, prec: Prec) -> ParseResult<'s, Expr> {
        let mut left = match self.peek() {
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.parse_prefix(PrefixOp::Deref, Prec::Ref)?
            }
            Some(TokenKind::Symbol(Symbol::Ampersand)) => {
                self.next();
                Expr::Ref(Box::new(self.parse_ref_expr()?))
            }
            Some(TokenKind::Ident) => {
                let name = self.next().str();
                let symbol = self.symbols.get_symbol(name);
                match self.peek() {
                    Some(TokenKind::Symbol(Symbol::OpenBrace)) => {
                        self.next();
                        let args = self.parse_list(
                            TokenKind::Symbol(Symbol::Comma),
                            TokenKind::Symbol(Symbol::CloseBrace),
                            |parser| parser.parse_expr(Prec::Bracket),
                        )?;
                        Expr::FuncCall(FuncCall { name: symbol, args })
                    }
                    Some(TokenKind::Symbol(Symbol::OpenCurlyBrace)) => {
                        self.next();
                        let values = self.parse_list(
                            TokenKind::Symbol(Symbol::Comma),
                            TokenKind::Symbol(Symbol::CloseCurlyBrace),
                            |parser| {
                                let name = parser.expect(TokenKind::Ident)?.str();
                                let symbol = parser.symbols.get_symbol(name);
                                parser.expect(TokenKind::Symbol(Symbol::Colon))?;
                                let expr = parser.parse_expr(Prec::Bracket)?;
                                Ok(StructValue { name: symbol, expr })
                            },
                        )?;
                        Expr::InitStruct {
                            name: symbol,
                            values,
                        }
                    }
                    _ => Expr::Ident(symbol),
                }
            }
            Some(TokenKind::Integer) => Expr::Integer(self.next().str().parse().unwrap()),
            Some(TokenKind::Keyword(Keyword::True)) => {
                self.next();
                Expr::Bool(true)
            }
            Some(TokenKind::Keyword(Keyword::False)) => {
                self.next();
                Expr::Bool(false)
            }
            Some(TokenKind::Symbol(Symbol::OpenBrace)) => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect(TokenKind::Symbol(Symbol::CloseBrace))?;
                self.next();
                expr
            }
            _ => Err(self.unexpected_token(Expected::Expr))?,
        };
        loop {
            left = match self.peek() {
                Some(TokenKind::Symbol(Symbol::Plus)) if prec >= Prec::Sum => {
                    self.parse_infix(left, InfixOp::Add, Prec::Sum)?
                }
                Some(TokenKind::Symbol(Symbol::Minus)) if prec >= Prec::Sum => {
                    self.parse_infix(left, InfixOp::Subtract, Prec::Sum)?
                }
                Some(TokenKind::Symbol(Symbol::Asterisk)) if prec >= Prec::Product => {
                    self.parse_infix(left, InfixOp::Multiply, Prec::Product)?
                }
                Some(TokenKind::Symbol(Symbol::ForwardSlash)) if prec >= Prec::Product => {
                    self.parse_infix(left, InfixOp::Divide, Prec::Product)?
                }
                Some(TokenKind::Symbol(Symbol::OpenAngleBrace)) if prec >= Prec::Compare => {
                    self.parse_infix(left, InfixOp::LessThan, Prec::Compare)?
                }
                Some(TokenKind::Symbol(Symbol::CloseAngleBrace)) if prec >= Prec::Compare => {
                    self.parse_infix(left, InfixOp::GreaterThan, Prec::Compare)?
                }
                Some(TokenKind::Symbol(Symbol::Dot)) if prec >= Prec::Dot => {
                    self.next();
                    let name = self.expect(TokenKind::Ident)?.str();
                    let symbol = self.symbols.get_symbol(name);
                    Expr::Field {
                        expr: Box::new(left),
                        name: symbol,
                    }
                }
                _ => break,
            }
        }
        Ok(left)
    }
    fn parse_prefix(&mut self, op: PrefixOp, prec: Prec) -> ParseResult<'s, Expr> {
        self.next();
        let expr = Box::new(self.parse_expr(prec)?);
        Ok(Expr::Prefix { op, expr })
    }
    fn parse_infix(&mut self, left: Expr, op: InfixOp, prec: Prec) -> ParseResult<'s, Expr> {
        self.next();
        let right = self.parse_expr(prec)?;
        Ok(Expr::Infix {
            left: Box::new(left),
            right: Box::new(right),
            op,
        })
    }
    fn parse_if(&mut self) -> ParseResult<'s, If> {
        let cond = Box::new(self.parse_expr(Prec::Bracket)?);
        let if_block = self.parse_block()?;
        let else_block = if self.eat(TokenKind::Keyword(Keyword::Else)) {
            if self.peek() == Some(TokenKind::Keyword(Keyword::If)) {
                Else::If(Box::new(self.parse_if()?))
            } else {
                Else::Block(self.parse_block()?)
            }
        } else {
            Else::None
        };
        Ok(If {
            cond,
            if_block,
            else_block,
        })
    }
    fn parse_stmt(&mut self) -> ParseResult<'s, Stmt> {
        Ok(match self.peek() {
            Some(TokenKind::Keyword(Keyword::If)) => {
                self.next();
                Stmt::If(self.parse_if()?)
            }
            Some(TokenKind::Keyword(Keyword::While)) => {
                self.next();
                let cond = self.parse_expr(Prec::Bracket)?;
                let body = self.parse_block()?;
                Stmt::While { cond, body }
            }
            Some(TokenKind::Keyword(Keyword::Var)) => {
                self.next();
                let name = self.expect(TokenKind::Ident)?.str();
                let symbol = self.symbols.get_symbol(name);

                let ty = if self.peek() == Some(TokenKind::Symbol(Symbol::Colon)) {
                    self.next();
                    Some(self.parse_ty()?)
                } else {
                    None
                };
                let expr = if self.peek() == Some(TokenKind::Symbol(Symbol::Equals)) {
                    self.next();
                    Some(self.parse_expr(Prec::Bracket)?)
                } else {
                    None
                };
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Let {
                    ident: symbol,
                    expr,
                    ty,
                }
            }
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.next();
                let expr = if self.peek() == Some(TokenKind::Symbol(Symbol::Semicolon)) {
                    None
                } else {
                    Some(self.parse_expr(Prec::Bracket)?)
                };
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Return(expr)
            }
            Some(TokenKind::Ident) => {
                let name = self.next().str();
                let symbol = self.symbols.get_symbol(name);
                let stmt = if self.peek() == Some(TokenKind::Symbol(Symbol::OpenBrace)) {
                    self.next();
                    let args = self.parse_list(
                        TokenKind::Symbol(Symbol::Comma),
                        TokenKind::Symbol(Symbol::CloseBrace),
                        |parser| parser.parse_expr(Prec::Bracket),
                    )?;
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
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                let ref_expr = self.parse_ref_expr()?;
                self.expect(TokenKind::Symbol(Symbol::Equals))?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect(TokenKind::Symbol(Symbol::Semicolon))?;
                Stmt::Assign { ref_expr, expr }
            }
            _ => Err(self.unexpected_token(Expected::Stmt))?,
        })
    }
    fn parse_basic_ty(&mut self, ty: Ty) -> Ty {
        self.next();
        ty
    }
    fn parse_ty(&mut self) -> ParseResult<'s, Ty> {
        Ok(match self.peek() {
            Some(TokenKind::Keyword(Keyword::I8)) => self.parse_basic_ty(Ty::Int(Int::I8)),
            Some(TokenKind::Keyword(Keyword::I16)) => self.parse_basic_ty(Ty::Int(Int::I16)),
            Some(TokenKind::Keyword(Keyword::I32)) => self.parse_basic_ty(Ty::Int(Int::I32)),

            Some(TokenKind::Keyword(Keyword::U8)) => self.parse_basic_ty(Ty::Int(Int::U8)),
            Some(TokenKind::Keyword(Keyword::U16)) => self.parse_basic_ty(Ty::Int(Int::U16)),
            Some(TokenKind::Keyword(Keyword::U32)) => self.parse_basic_ty(Ty::Int(Int::U32)),

            Some(TokenKind::Keyword(Keyword::Bool)) => self.parse_basic_ty(Ty::Bool),

            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                Ty::Ref(Box::new(self.parse_ty()?))
            }
            Some(TokenKind::Ident) => {
                let name = self.next().str();
                Ty::Struct(self.symbols.get_symbol(name))
            }
            _ => Err(self.unexpected_token(Expected::Type))?,
        })
    }
    fn parse_block(&mut self) -> ParseResult<'s, Block> {
        self.expect(TokenKind::Symbol(Symbol::OpenCurlyBrace))?;
        let mut stmts = vec![];
        while self.peek() != Some(TokenKind::Symbol(Symbol::CloseCurlyBrace)) {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.handled_errors.push(err);
                    loop {
                        match self.peek() {
                            Some(TokenKind::Symbol(Symbol::Semicolon)) => {
                                self.next();
                                break;
                            }
                            Some(TokenKind::Symbol(Symbol::CloseCurlyBrace)) => break,
                            Some(TokenKind::Keyword(Keyword::Func | Keyword::Struct)) => {
                                return Ok(Block { stmts })
                            }
                            _ => _ = self.next(),
                        }
                    }
                }
            }
        }
        self.next();
        Ok(Block { stmts })
    }
    fn parse_func(&mut self) -> ParseResult<'s, Func> {
        let name = self.expect(TokenKind::Ident)?.str();
        let symbol = self.symbols.get_symbol(name);
        self.expect(TokenKind::Symbol(Symbol::OpenBrace))?;
        let params = self.parse_list(
            TokenKind::Symbol(Symbol::Comma),
            TokenKind::Symbol(Symbol::CloseBrace),
            |parser| {
                let name = parser.expect(TokenKind::Ident)?.str();
                let symbol = parser.symbols.get_symbol(name);
                parser.expect(TokenKind::Symbol(Symbol::Colon))?;
                let ty = parser.parse_ty()?;
                Ok(Param { name: symbol, ty })
            },
        )?;
        let returns = match self.peek() {
            Some(TokenKind::Symbol(Symbol::OpenCurlyBrace | Symbol::Semicolon)) => None,
            _ => Some(self.parse_ty()?),
        };
        let body = match self.peek() {
            Some(TokenKind::Symbol(Symbol::Semicolon)) => {
                self.next();
                None
            }
            Some(TokenKind::Symbol(Symbol::OpenCurlyBrace)) => Some(self.parse_block()?),
            _ => {
                return Err(self
                    .unexpected_token(Expected::Token(TokenKind::Symbol(Symbol::OpenCurlyBrace))))
            }
        };
        Ok(Func {
            name: symbol,
            body,
            params,
            returns,
        })
    }
    fn parse_struct(&mut self) -> ParseResult<'s, Struct> {
        let name = self.expect(TokenKind::Ident)?.str();
        let symbol = self.symbols.get_symbol(name);
        self.expect(TokenKind::Symbol(Symbol::OpenCurlyBrace))?;
        let fields = self.parse_list(
            TokenKind::Symbol(Symbol::Comma),
            TokenKind::Symbol(Symbol::CloseCurlyBrace),
            |parser| {
                let name = parser.expect(TokenKind::Ident)?.str();
                let symbol = parser.symbols.get_symbol(name);
                parser.expect(TokenKind::Symbol(Symbol::Colon))?;
                let ty = parser.parse_ty()?;
                Ok(StructField { name: symbol, ty })
            },
        )?;
        Ok(Struct {
            fields,
            name: symbol,
        })
    }
    fn parse_decl(&mut self) -> ParseResult<'s, Decl> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Func)) => {
                self.next();
                Ok(Decl::Func(self.parse_func()?))
            }
            Some(TokenKind::Keyword(Keyword::Struct)) => {
                self.next();
                Ok(Decl::Struct(self.parse_struct()?))
            }
            _ => Err(self.unexpected_token(Expected::Decl))?,
        }
    }
    fn parse_program(&mut self) -> Program {
        let mut decls = vec![];
        while self.peek().is_some() {
            match self.parse_decl() {
                Ok(decl) => decls.push(decl),
                Err(err) => loop {
                    self.handled_errors.push(err);
                    match self.peek() {
                        Some(TokenKind::Keyword(Keyword::Func | Keyword::Struct)) => break,
                        _ => _ = self.next(),
                    }
                },
            }
        }
        Program { decls }
    }
}
