use std::fmt;

use crate::{ty::{TyRef, IntTyRef, TyOption, IntTyOption}};

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub branch: Branch<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(u32);

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
pub struct Fun<'a> {
    pub name: &'a str,
    pub is_extern: bool,
    pub params: Vec<TyRef>,
    pub returns: Option<TyRef>,
    blocks: Vec<Block<'a>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Alloc(TyRef),
    Assign {
        assign: Assign,
        expr: Expr<'a>,
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
        op: BinaryOp,
    },
    Bool(bool),
    Load {
        stack_slot: u32,
        ty: TyRef,
    },
    Ref(u32),
    Deref {
        expr: Box<Expr<'a>>,
        ty: TyRef,
    },
    FnCall {
        fn_call: FnCall<'a>,
        result: TyRef,
    }
}

#[derive(Debug, Clone)]
pub struct FnCall<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
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

impl<'a> Fun<'a> {
    pub fn new(params: Vec<TyRef>, is_extern: bool, name: &'a str, returns: Option<TyRef>) -> Fun<'a> {
        Fun { blocks: vec![], params, is_extern, name, returns }
    }
    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block { stmts: vec![], branch: Branch::Return(None) });
        id
    }
    pub fn get_block_mut(&mut self, id: BlockId) -> &mut Block<'a> {
        &mut self.blocks[id.0 as usize]
    }
    pub fn get_block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0 as usize]
    }
    pub fn blocks(&self) -> BlockIdIter {
        BlockIdIter { index: 0, len: self.blocks.len() as u32 }
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

impl<'a> fmt::Display for Fun<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn {}()", self.name)?;
        if let Some(ty) = &self.returns {
            write!(f, " {}", TyOption(ty.concrete()))?;
        }
        writeln!(f, "")?;
        for block in self.blocks() {
            writeln!(f, "b{}:", block.id())?;
            write!(f, "{}", self.get_block(block))?;
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
            Stmt::Assign { assign, expr } => write!(f, "{} = {}", assign, expr),
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
            Expr::Binary { left, right, op } => write!(f, "({} {} {})", left, op, right),
            Expr::Bool(value) => write!(f, "{}", if *value { "true" } else { "false" }),
            Expr::Load { stack_slot, ty } => write!(f, "({}) ${}", TyOption(ty.concrete()), stack_slot),
            Expr::Ref(stack_slot) => write!(f, "&${}", stack_slot),
            Expr::Deref { expr, ty } => write!(f, "*({}) {}", TyOption(ty.concrete()), expr),
            Expr::FnCall { .. } => write!(f, "call"),
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