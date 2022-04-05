use std::collections::HashMap;

use crate::symbols::Symbol;

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub if_block: Block,
    pub else_block: Else,
}

#[derive(Debug, Clone)]
pub enum Else {
    Block(Block),
    If(Box<If>),
    None,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i64),
    Bool(bool),
    Ident(Symbol),
    Field {
        expr: Box<Expr>,
        name: Symbol,
    },
    Infix {
        left: Box<Expr>,
        right: Box<Expr>,
        op: InfixOp,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    FnCall(FuncCall),
    InitStruct {
        name: Symbol,
        values: Vec<StructValue>
    },
}

#[derive(Debug, Clone)]
pub struct StructValue {
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Assign {
    Deref(Box<Assign>),
    Name(Symbol),
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Deref,
    Ref,
}

#[derive(Debug, Clone, Copy)]
pub enum InfixOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        ident: Symbol,
        expr: Option<Expr>,
        ty: Option<Ty>
    },
    Assign {
        assign: Assign,
        expr: Expr
    },
    While {
        cond: Expr,
        body: Block,
    },
    Return(Option<Expr>),
    If(If),
    FuncCall(FuncCall),
}

#[derive(Debug, Clone)]
pub enum Ty {
    Struct(Symbol),
    Ref(Box<Ty>),
    Int(Int),
    Bool,
}

#[derive(Debug, Clone)]
pub enum Int {
    I8,
    I16,
    I32,
    U8,
    U16,
    U32,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Symbol,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name: Symbol,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub params: Vec<Param>,
    pub returns: Option<Ty>,
    pub body: Option<Block>,
}

pub struct Struct {
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub name: Symbol,
    pub ty: Ty,
}

pub struct Program {
    pub funcs: HashMap<Symbol, Func>,
    pub structs: HashMap<Symbol, Struct>,
}