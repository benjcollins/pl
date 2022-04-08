use crate::{ty::Int, typed_ast, symbols::Symbol};

#[derive(Debug, Clone)]
pub enum Ty {
    Int(Int),
    Bool,
    Ptr,
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Symbol,
    pub ty: Ty,
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
    Static(typed_ast::BlockId),
    Condition {
        expr: Expr,
        if_true: typed_ast::BlockId,
        if_false: typed_ast::BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(Ty),
    DerefAssign {
        assign: Expr,
        ty: Ty,
        expr: Expr,
    },
    Assign {
        ref_expr: RefExpr,
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
        op: typed_ast::BinaryOp,
    },
    Bool(bool),
    Load {
        var: typed_ast::Variable,
        ty: Ty,
    },
    Ref(RefExpr),
    Deref {
        expr: Box<Expr>,
        ty: Ty,
    },
    FuncCall(FuncCall),
    InitStruct(Vec<StructValue>),
    Field {
        expr: Box<Expr>,
        fields: Vec<StructField>,
        name: Symbol,
    }
}

#[derive(Debug, Clone)]
pub enum RefExpr {
    Variable(typed_ast::Variable),
    Field {
        ref_expr: Box<RefExpr>,
        fields: Vec<StructField>,
        name: Symbol,
    }
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