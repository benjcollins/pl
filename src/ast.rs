use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct If<'a> {
    pub cond: Box<Expr<'a>>,
    pub if_block: Block<'a>,
    pub else_block: Else<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Else<'a> {
    Block(Block<'a>),
    If(Box<If<'a>>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Integer(&'a str),
    Bool(bool),
    Ident(&'a str),
    Field {
        expr: Box<Expr<'a>>,
        name: &'a str,
    },
    Infix {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
        op: InfixOp,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<Expr<'a>>,
    },
    FnCall(FnCall<'a>),
    InitStruct {
        name: &'a str,
        values: Vec<StructValue<'a>>
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValue<'a> {
    pub name: &'a str,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assign<'a> {
    Deref(Box<Assign<'a>>),
    Name(&'a str),
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
pub enum Stmt<'a> {
    Let {
        ident: &'a str,
        expr: Option<Expr<'a>>,
        ty: Option<Ty<'a>>
    },
    Assign {
        assign: Assign<'a>,
        expr: Expr<'a>
    },
    While {
        cond: Expr<'a>,
        body: Block<'a>,
    },
    Return(Option<Expr<'a>>),
    If(If<'a>),
    FnCall(FnCall<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty<'a> {
    Name(&'a str),
    Ref(Box<Ty<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'a> {
    pub name: &'a str,
    pub ty: Ty<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func<'a> {
    pub params: Vec<Param<'a>>,
    pub returns: Option<Ty<'a>>,
    pub body: Option<Block<'a>>,
}

pub struct Struct<'a> {
    pub fields: Vec<StructField<'a>>,
}

pub struct StructField<'a> {
    pub name: &'a str,
    pub ty: Ty<'a>,
}

pub struct Program<'a> {
    pub funcs: HashMap<&'a str, Func<'a>>,
    pub structs: HashMap<&'a str, Struct<'a>>,
}