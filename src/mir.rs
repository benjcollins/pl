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
    pub fn new() -> Fun {
        Fun { blocks: vec![] }
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