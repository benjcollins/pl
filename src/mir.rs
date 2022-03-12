#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntTyName(usize);

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct TyName(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int {
    pub size: Size,
    pub signedness: Signedness,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    B8, B16, B32
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Signedness {
    Signed, Unsigned
}

#[derive(Debug, Clone, Copy)]
pub enum Ty {
    Any,
    Equal(TyName),
    Bool,
    Ref(TyName),
    None,
    Int(IntTyName),
}

#[derive(Debug, Clone, Copy)]
pub enum IntTy {
    Any,
    Equal(IntTyName),
    Int(Int),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteTy {
    None,
    Bool,
    Int(Int),
    Ref(Box<ConcreteTy>),
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(u32);

#[derive(Debug, Clone)]
pub enum Branch {
    End,
    Static(BlockId),
    Bool {
        expr: Expr,
        if_true: BlockId,
        if_false: BlockId,
    },
    Compare {
        a: Expr,
        b: Expr,
        cmp: CompareOp,
        if_true: BlockId,
        if_false: BlockId,
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompareOp {
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone)]
pub struct Fun {
    blocks: Vec<Block>,
    tys: Vec<Ty>,
    int_tys: Vec<IntTy>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(TyName),
    Drop,
    Return(Expr),
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
        ty: IntTyName,
    },
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
    Bool(bool),
    Load {
        stack_slot: u32,
        ty: TyName,
    },
    Ref(u32),
    Deref {
        expr: Box<Expr>,
        ty: TyName,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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

impl Ty {
    pub fn concrete(&self, fun: &Fun) -> ConcreteTy {
        match self {
            Ty::Bool => ConcreteTy::Bool,
            Ty::None => ConcreteTy::None,
            Ty::Int(name) => ConcreteTy::Int(fun.get_int_ty(*name).concrete(fun)),
            Ty::Equal(name) => fun.get_ty(*name).concrete(fun),
            Ty::Ref(ty) => ConcreteTy::Ref(Box::new(fun.get_ty(*ty).concrete(fun))),
            Ty::Any => panic!(),
        }
    }
}

impl IntTy {
    pub fn concrete(&self, fun: &Fun) -> Int {
        match self {
            IntTy::Any => Int { size: Size::B32, signedness: Signedness::Signed },
            IntTy::Int(int) => *int,
            IntTy::Equal(name) => fun.get_int_ty(*name).concrete(fun),
        }
    }
}

impl Fun {
    pub fn new() -> Fun {
        Fun { blocks: vec![], tys: vec![], int_tys: vec![] }
    }
    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block { stmts: vec![], branch: Branch::End });
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
    pub fn new_int_ty_name(&mut self, ty: IntTy) -> IntTyName {
        let ty_name = IntTyName(self.int_tys.len());
        self.int_tys.push(ty);
        ty_name
    }
    pub fn new_ty_name(&mut self, ty: Ty) -> TyName {
        let ty_name = TyName(self.tys.len());
        self.tys.push(ty);
        ty_name
    }
    pub fn get_int_ty(&self, name: IntTyName) -> &IntTy {
        &self.int_tys[name.0]
    }
    pub fn assign_int_ty(&mut self, name: IntTyName, ty: IntTy) {
        self.int_tys[name.0] = ty
    }
    pub fn get_ty(&self, name: TyName) -> &Ty {
        &self.tys[name.0]
    }
    pub fn assign_ty(&mut self, name: TyName, ty: Ty) {
        self.tys[name.0] = ty
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