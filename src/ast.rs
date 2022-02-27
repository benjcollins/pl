use crate::token::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub offset: usize,
}

impl Ident {
    pub fn as_str<'src>(&self, src: &'src str) -> &'src str {
        Token::new(self.offset, TokenKind::Ident).as_str(src)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer {
        offset: usize,
    },
    Ident(Ident),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { ident: Ident, expr: Option<Expr>, ty: Option<Ty> },
    Assign { ident: Ident, expr: Expr },
    Return { expr: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub params: Vec<(Ident, Ty)>,
    pub returns: Option<Ty>,
    pub block: Block,
    pub name: Ident,
}