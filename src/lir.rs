use crate::{ty::Int, mir, symbols::Symbol};

#[derive(Debug, Clone)]
pub enum Ty {
    Int(Int),
    Bool,
    Ptr,
    Struct(Vec<Ty>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Variable(u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub enum Branch {
    Return(Option<Expr>),
    Static(BlockId),
    Condition {
        expr: Expr,
        if_true: BlockId,
        if_false: BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum Assign {
    Deref(Box<Assign>),
    Assign {
        var: Variable,
        expr: Expr,
        ty: Ty,
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(Ty),
    Assign(Assign),
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
        var: Variable,
        ty: Ty,
    },
    Ref(Variable),
    Deref {
        expr: Box<Expr>,
        ty: Ty,
    },
    FnCall {
        fn_call: FuncCall,
        result: Ty,
    },
    InitStruct(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name: Symbol,
    pub args: Vec<Expr>,
}