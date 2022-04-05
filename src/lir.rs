use crate::{ty::Int, mir, symbols::Symbol};

#[derive(Debug, Clone)]
pub enum Ty {
    Int(Int),
    Bool,
    Ptr,
    Struct(Vec<Ty>),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Symbol,
    pub params: Vec<Ty>,
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone)]
pub enum Branch {
    Return(Option<Expr>),
    Static(mir::BlockId),
    Condition {
        expr: Expr,
        if_true: mir::BlockId,
        if_false: mir::BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum Assign {
    Deref(Box<Assign>),
    Variable(mir::Variable),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(Ty),
    Assign {
        assign: Assign,
        ty: Ty,
        expr: Expr,
    },
    FuncCall(FuncCall),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Int,
        op: mir::BinaryOp,
    },
    Bool(bool),
    Load {
        var: mir::Variable,
        ty: Ty,
    },
    Ref(mir::Variable),
    Deref {
        expr: Box<Expr>,
        ty: Ty,
    },
    FuncCall(FuncCall),
    InitStruct(Vec<StructValue>),
}

#[derive(Debug, Clone)]
pub struct StructValue {
    pub ty: Ty,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name: Symbol,
    pub args: Vec<Expr>,
}