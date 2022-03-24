use crate::{token::{Token, TokenKind}, ast::{Expr, InfixOp, Stmt, Else, If, Block, Ty, Fun, PrefixOp, Assign, Param, FnCall}};

pub fn parse<'a>(tokens: &[Token], src: &'a str) -> ParseResult<Vec<Fun<'a>>> {
    let mut parser = Parser {
        index: 0,
        tokens,
        src,
    };
    let mut fns = vec![];
    while parser.index < parser.tokens.len() {
        fns.push(parser.parse_fn()?)
    }
    Ok(fns)
}

struct Parser<'a, 'b> {
    tokens: &'b [Token],
    index: usize,
    src: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
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
        let token = self.peek();
        self.index += 1;
        token
    }
    fn peek(&self) -> Token {
        self.tokens[self.index]
    }
    fn eat(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.peek().kind == kind {
            let token = self.peek();
            self.next();
            Ok(token)
        } else {
            Err(self.unexpected_token())
        }
    }
    fn unexpected_token(&self) -> ParseError {
        ParseError { token: self.peek() }
    }
    fn parse_list<T>(&mut self, sep: TokenKind, term: TokenKind, f: impl Fn(&mut Parser<'a, 'b>) -> ParseResult<T>) -> ParseResult<Vec<T>> {
        let mut items = vec![];
        if self.peek().kind != term {
            items.push(f(self)?);
            while self.peek().kind == sep {
                self.next();
                items.push(f(self)?);
            }
        }
        self.eat(term)?;
        Ok(items)
    }
    fn parse_expr(&mut self, prec: Prec) -> ParseResult<Expr<'a>> {
        let mut left = match self.peek().kind {
            TokenKind::Asterisk => self.parse_prefix(PrefixOp::Deref, Prec::Ref)?,
            TokenKind::Ampersand => self.parse_prefix(PrefixOp::Ref, Prec::Ref)?,

            TokenKind::Ident => {
                let ident = self.next().as_str(self.src);
                if self.peek().kind == TokenKind::OpenBrace {
                    self.next();
                    let args = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| parser.parse_expr(Prec::Bracket))?;
                    Expr::FnCall(FnCall { name: ident, args })
                } else {
                    Expr::Ident(ident)
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
                if self.peek().kind != TokenKind::CloseBrace { panic!() }
                self.next();
                expr
            }
            _ => Err(self.unexpected_token())?,
        };
        loop {
            left = match self.peek().kind {
                TokenKind::Plus if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Add, Prec::Sum)?,
                TokenKind::Minus if prec >= Prec::Sum => self.parse_infix(left, InfixOp::Subtract, Prec::Sum)?,
                TokenKind::Asterisk if prec >= Prec::Product => self.parse_infix(left, InfixOp::Multiply, Prec::Product)?,
                TokenKind::ForwardSlash if prec >= Prec::Product => self.parse_infix(left, InfixOp::Divide, Prec::Product)?,
                TokenKind::OpenAngleBrace if prec >= Prec::Compare => self.parse_infix(left, InfixOp::LessThan, Prec::Compare)?,
                TokenKind::CloseAngleBrace if prec >= Prec::Compare => self.parse_infix(left, InfixOp::GreaterThan, Prec::Compare)?,
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
    fn parse_assign(&mut self) -> ParseResult<Assign<'a>> {
        Ok(match self.peek().kind {
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
            TokenKind::Var => {
                self.next();
                let ident = self.eat(TokenKind::Ident)?.as_str(self.src);

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
                self.eat(TokenKind::Semicolon)?;
                Stmt::Let { ident, expr, ty }
            }
            TokenKind::Return => {
                self.next();
                let expr = if self.peek().kind == TokenKind::Semicolon {
                    None
                } else {
                    Some(self.parse_expr(Prec::Bracket)?)
                };
                self.eat(TokenKind::Semicolon)?;
                Stmt::Return(expr)
            }
            TokenKind::Ident => {
                let name = self.next().as_str(self.src);
                let stmt = if self.peek().kind == TokenKind::OpenBrace {
                    self.next();
                    let args = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| parser.parse_expr(Prec::Bracket))?;
                    Stmt::FnCall(FnCall { name, args })
                } else {
                    self.eat(TokenKind::Equals)?;
                    let expr = self.parse_expr(Prec::Bracket)?;
                    Stmt::Assign { assign: Assign::Name(name), expr }
                };
                self.eat(TokenKind::Semicolon)?;
                stmt
            }
            TokenKind::Asterisk => {
                let assign = self.parse_assign()?;
                self.eat(TokenKind::Equals)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.eat(TokenKind::Semicolon)?;
                Stmt::Assign { assign, expr }
            }
            _ => Err(self.unexpected_token())?,
        })
    }
    fn parse_ty(&mut self) -> ParseResult<Ty<'a>> {
        Ok(match self.peek().kind {
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
        self.eat(TokenKind::OpenCurlyBrace)?;
        let mut stmts = vec![];
        while self.peek().kind != TokenKind::CloseCurlyBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block { stmts })
    }
    fn parse_fn(&mut self) -> ParseResult<Fun<'a>> {
        let is_extern = if self.peek().kind == TokenKind::Extern {
            self.next();
            true
        } else {
            false
        };
        self.eat(TokenKind::Fn)?;
        let name = self.eat(TokenKind::Ident)?.as_str(self.src);

        self.eat(TokenKind::OpenBrace)?;
        let params = self.parse_list(TokenKind::Comma, TokenKind::CloseBrace, |parser| {
            let name = parser.eat(TokenKind::Ident)?.as_str(self.src);
            parser.eat(TokenKind::Colon)?;
            let ty = parser.parse_ty()?;
            Ok(Param { name, ty })
        })?;
        let returns = if self.peek().kind == TokenKind::Colon {
            self.next();
            Some(self.parse_ty()?)
        } else {
            None
        };
        let body = if self.peek().kind == TokenKind::OpenCurlyBrace {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Fun { body, params, returns, name, is_extern })
    }
}