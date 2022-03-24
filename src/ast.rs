use crate::token::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub start: usize,
    pub end: usize,
}

impl Ident {
    pub fn new(token: Token) -> Ident {
        assert_eq!(token.kind, TokenKind::Ident);
        Ident { start: token.start, end: token.end }
    }
    pub fn as_str<'src>(&self, src: &'src str) -> &'src str {
        &src[self.start..self.end]
    }
    pub fn eq(&self, other: Ident, src: &str) -> bool {
        self.as_str(src) == other.as_str(src)
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
        start: usize,
        end: usize,
    },
    Bool(bool),
    Ident(Ident),
    Infix {
        left: Box<Expr>,
        right: Box<Expr>,
        op: InfixOp,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    FnCall(FnCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assign {
    Deref(Box<Assign>),
    Name(Ident),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrefixOp {
    Deref,
    Ref,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InfixOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { ident: Ident, expr: Option<Expr>, ty: Option<Ty> },
    Assign { assign: Assign, expr: Expr },
    Return { expr: Option<Expr> },
    While {
        cond: Expr,
        body: Block,
    },
    If(If),
    FnCall(FnCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Name(Ident),
    Ref(Box<Ty>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub params: Vec<Param>,
    pub returns: Option<Ty>,
    pub body: Option<Block>,
    pub name: Ident,
    pub is_extern: bool,
}