use std::fmt;

use crate::{ty::{TyRef, IntTyRef, TyOption, IntTyOption}};

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub branch: Branch<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub enum Branch<'a> {
    Return(Option<Expr<'a>>),
    Static(BlockId),
    Condition {
        expr: Expr<'a>,
        if_true: BlockId,
        if_false: BlockId,
    },
}

#[derive(Debug, Clone)]
pub struct Func<'a> {
    pub name: &'a str,
    pub params: Vec<TyRef<'a>>,
    pub returns: Option<TyRef<'a>>,
    pub blocks: Vec<Block<'a>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Alloc(TyRef<'a>),
    Assign {
        assign: Assign,
        expr: Expr<'a>,
        ty: TyRef<'a>,
    },
    FnCall(FnCall<'a>),
}

#[derive(Debug, Clone)]
pub enum Assign {
    Deref(Box<Assign>),
    Stack(u32),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Int {
        value: u32,
        ty: IntTyRef,
    },
    Binary {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
        ty: IntTyRef,
        op: BinaryOp,
    },
    Bool(bool),
    Load {
        stack_slot: u32,
        ty: TyRef<'a>,
    },
    Ref(u32),
    Deref {
        expr: Box<Expr<'a>>,
        ty: TyRef<'a>,
    },
    FnCall {
        fn_call: FnCall<'a>,
        result: TyRef<'a>,
    },
    InitStruct(Vec<StructValue<'a>>),
}

#[derive(Debug, Clone)]
pub struct StructValue<'a> {
    pub expr: Expr<'a>,
    pub ty: TyRef<'a>,
}

#[derive(Debug, Clone)]
pub struct FnCall<'a> {
    pub name: &'a str,
    pub args: Vec<Arg<'a>>,
}

#[derive(Debug, Clone)]
pub struct Arg<'a> {
    pub expr: Expr<'a>,
    pub ty: TyRef<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

pub struct BlockIdIter {
    index: u32,
    len: u32,
}

impl BlockId {
    pub fn id(&self) -> u32 {
        self.0
    }
}

impl Iterator for BlockIdIter {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.len {
            let block_id = BlockId(self.index);
            self.index += 1;
            Some(block_id)
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Func<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn {}()", self.name)?;
        if let Some(ty) = &self.returns {
            write!(f, " {}", TyOption(ty.concrete()))?;
        }
        writeln!(f, "")?;
        for (id, block) in self.blocks.iter().enumerate() {
            writeln!(f, "b{}:", id)?;
            write!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Block<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.stmts {
            writeln!(f, "  {}", stmt)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Alloc(ty) => write!(f, "alloc({})", TyOption(ty.concrete())),
            Stmt::Assign { assign, expr, ty } => write!(f, "{} = {}", assign, expr),
            Stmt::FnCall { .. } => write!(f, "call"),
        }
    }
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assign::Deref(assign) => write!(f, "*{}", assign),
            Assign::Stack(stack_slot) => write!(f, "${}", stack_slot),
        }
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int { value, ty } => write!(f, "{}{}", value, IntTyOption(ty.concrete())),
            Expr::Binary { left, right, op, .. } => write!(f, "({} {} {})", left, op, right),
            Expr::Bool(value) => write!(f, "{}", if *value { "true" } else { "false" }),
            Expr::Load { stack_slot, ty } => write!(f, "({}) ${}", TyOption(ty.concrete()), stack_slot),
            Expr::Ref(stack_slot) => write!(f, "&${}", stack_slot),
            Expr::Deref { expr, ty } => write!(f, "*({}) {}", TyOption(ty.concrete()), expr),
            Expr::FnCall { .. } => write!(f, "call"),
            Expr::InitStruct { .. } => todo!(),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::LessThan => "<",
            BinaryOp::GreaterThan => ">",
        })
    }
}