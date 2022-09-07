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
    Ref(Box<RefExpr>),
    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    FuncCall(FuncCall),
    InitStruct {
        name: Symbol,
        values: Vec<StructValue>,
    },
}

#[derive(Debug, Clone)]
pub enum RefExpr {
    Ident(Symbol),
    Deref(Expr),
    Field {
        ref_expr: Box<RefExpr>,
        name: Symbol,
    },
}

#[derive(Debug, Clone)]
pub struct StructValue {
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Deref,
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
        ty: Option<Ty>,
    },
    Assign {
        ref_expr: RefExpr,
        expr: Expr,
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
    pub name: Symbol,
    pub params: Vec<Param>,
    pub returns: Option<Ty>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Symbol,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Symbol,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Struct(Struct),
    Func(Func),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

impl Program {
    pub fn func_iter(&self) -> impl Iterator<Item = &Func> {
        self.decls.iter().filter_map(|decl| match decl {
            Decl::Func(func_decl) => Some(func_decl),
            _ => None,
        })
    }
    pub fn struct_iter(&self) -> impl Iterator<Item = &Struct> {
        self.decls.iter().filter_map(|decl| match decl {
            Decl::Struct(struct_decl) => Some(struct_decl),
            _ => None,
        })
    }
}
