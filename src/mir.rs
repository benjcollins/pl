use std::fmt;

use crate::ty::{TyRef, IntTyRef};

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(u32);

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
pub struct Fun {
    pub params: Vec<TyRef>,
    blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(TyRef),
    Assign {
        assign: Assign,
        expr: Expr,
    }
}

#[derive(Debug, Clone)]
pub enum Assign {
    Deref(Box<Assign>),
    Stack(u32),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int {
        value: u32,
        ty: IntTyRef,
    },
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
    Bool(bool),
    Load {
        stack_slot: u32,
        ty: TyRef,
    },
    Ref(u32),
    Deref {
        expr: Box<Expr>,
        ty: TyRef,
    },
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

impl Fun {
    pub fn new(params: Vec<TyRef>) -> Fun {
        Fun { blocks: vec![], params }
    }
    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block { stmts: vec![], branch: Branch::Return(None) });
        id
    }
    pub fn get_block_mut(&mut self, id: BlockId) -> &mut Block {
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

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in self.blocks() {
            writeln!(f, "b{}:", block.id())?;
            write!(f, "{}", self.get_block(block))?;
        }
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.stmts {
            writeln!(f, "  {}", stmt)?;
        }
        Ok(())
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Alloc(ty) => write!(f, "alloc {}", ty),
            Stmt::Assign { assign, expr } => write!(f, "{} = {}", assign, expr),
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int { value, ty } => write!(f, "{}{}", value, ty),
            Expr::Binary { left, right, op } => write!(f, "({} {} {})", left, op, right),
            Expr::Bool(value) => write!(f, "{}", if *value { "true" } else { "false" }),
            Expr::Load { stack_slot, ty } => write!(f, "${}:{}", stack_slot, ty),
            Expr::Ref(stack_slot) => write!(f, "&${}", stack_slot),
            Expr::Deref { expr, ty } => write!(f, "*({}) {}", ty, expr),
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