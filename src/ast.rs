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
pub struct If {
    pub cond: Box<Expr>,
    pub if_block: Block,
    pub else_block: Else,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Else {
    Block(Block),
    If(Box<If>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer {
        offset: usize,
    },
    Bool(bool),
    Ident(Ident),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
    // If(If),
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
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub params: Vec<(Ident, Ty)>,
    pub returns: Option<Ty>,
    pub block: Block,
    pub name: Ident,
}