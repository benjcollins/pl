use crate::{ty::{Int, Ty, AtomicTy}, symbol::{Symbol, SymbolMap, SymbolTable}, ast::{self, If}, token::{Token, TokenKind}};

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(u32);

#[derive(Debug, Clone)]
pub enum Branch {
    End,
    Static(BlockId),
    Conditional {
        expr: Expr,
        if_true: BlockId,
        if_false: BlockId,
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(Ty),
    Drop,
    Return {
        ty: Ty,
        expr: Expr,
    },
    Assign {
        ty: Ty,
        stack_slot: usize,
        expr: Expr,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int {
        value: u32,
        ty: Int,
    },
    Bool(bool),
    Load {
        stack_slot: usize,
        ty: Ty,
    },
    Arithmetic {
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Int,
        op: ArithmeticOp,
    }
}

#[derive(Debug, Clone)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct Compiler<'a> {
    symbols: &'a SymbolTable,
    symbol_tys: &'a SymbolMap<Ty>,
    return_ty: &'a Ty,
    src: &'a str,
    stack_slots: Vec<Ty>,
    symbol_locations: SymbolMap<usize>,
    fun: Fun,
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

impl<'a> Compiler<'a> {
    pub fn compile_fun(src: &str, symbols: &SymbolTable, symbol_tys: &SymbolMap<Ty>, return_ty: &Ty, fun: &ast::Fun) -> Fun {
        let mut compiler = Compiler {
            src,
            symbols,
            symbol_tys,
            symbol_locations: symbols.create_symbol_map(),
            stack_slots: vec![],
            fun: Fun::new(),
            return_ty,
        };
        let mut main = compiler.fun.new_block();
        compiler.compile_block(&fun.block, &mut main);
        compiler.fun
    }
    fn compile_block(&mut self, ast_block: &ast::Block, block_id: &mut BlockId) {
        let stack_len = self.stack_slots.len();
        for stmt in &ast_block.stmts {
            self.compile_stmt(&stmt, block_id);
        }
        for _ in 0..self.stack_slots.iter().rev().take(self.stack_slots.len() - stack_len).count() {
            self.fun.get_block_mut(*block_id).stmts.push(Stmt::Drop)
        }
        self.stack_slots.truncate(stack_len);
    }
    fn compile_if(&mut self, if_stmt: &If, block_id: &mut BlockId) {
        let cond = self.compile_expr(&if_stmt.cond, &Ty::Atomic(AtomicTy::Bool));
        let mut if_block_id = self.fun.new_block();
        let mut else_block_id = self.fun.new_block();
        self.compile_block(&if_stmt.if_block, &mut if_block_id);
        self.fun.get_block_mut(*block_id).branch = Branch::Conditional {
            expr: cond,
            if_true: if_block_id,
            if_false: else_block_id,
        };

        match &if_stmt.else_block {
            ast::Else::Block(else_ast_block) => {
                let exit_block_id = self.fun.new_block();
                self.compile_block(else_ast_block, &mut else_block_id);
                self.fun.get_block_mut(else_block_id).branch = Branch::Static(exit_block_id);
                self.fun.get_block_mut(if_block_id).branch = Branch::Static(exit_block_id);
                *block_id = exit_block_id;
            }
            ast::Else::If(if_stmt) => {
                self.compile_if(if_stmt, &mut else_block_id);
                self.fun.get_block_mut(if_block_id).branch = Branch::Static(else_block_id);
                *block_id = else_block_id;
            }
            ast::Else::None => {
                self.fun.get_block_mut(if_block_id).branch = Branch::Static(else_block_id);
                *block_id = else_block_id;
            }
        }
    }
    fn compile_stmt(&mut self, stmt: &ast::Stmt, block_id: &mut BlockId) {
        match stmt {
            ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
            ast::Stmt::Let { ident, expr, .. } => {
                let symbol = self.symbols.ident_to_symbol(*ident);
                let ty = self.symbol_tys.get(symbol);
                self.fun.get_block_mut(*block_id).stmts.push(Stmt::Alloc(ty.clone()));
                self.symbol_locations.update(symbol, self.stack_slots.len());
                self.stack_slots.push(ty.clone());
                if let Some(expr) = expr {
                    self.compile_assign(symbol, expr, block_id);
                }
            }
            ast::Stmt::Assign { ident, expr } => {
                let symbol = self.symbols.ident_to_symbol(*ident);
                self.compile_assign(symbol, expr, block_id)
            }
            ast::Stmt::Return { expr } => {
                let expr = self.compile_expr(expr, self.return_ty);
                let return_ty = self.return_ty.clone();
                self.fun.get_block_mut(*block_id).stmts.push(Stmt::Return { ty: return_ty, expr });
            }
        }
    }
    fn compile_assign(&mut self, symbol: Symbol, expr: &ast::Expr, block_id: &mut BlockId) {
        let ty = self.symbol_tys.get(symbol);
        let expr = self.compile_expr(expr, ty);
        let stack_slot = *self.symbol_locations.get(symbol);
        self.fun.get_block_mut(*block_id).stmts.push(Stmt::Assign { stack_slot, expr, ty: ty.clone() })
    }
    fn compile_expr(&self, expr: &ast::Expr, ty: &Ty) -> Expr {
        match expr {
            ast::Expr::Integer { offset } => {
                let token = Token::new(*offset, TokenKind::Integer);
                let value: u32 = token.as_str(self.src).parse().unwrap();
                let int = match ty {
                    Ty::Atomic(AtomicTy::Int(int)) => int,
                    _ => panic!(),
                };
                Expr::Int { value, ty: *int }
            }
            ast::Expr::Bool(value) => {
                Expr::Bool(*value)
            }
            ast::Expr::Ident(ident) => {
                let symbol = self.symbols.ident_to_symbol(*ident);
                let symbol_ty = self.symbol_tys.get(symbol);
                assert_eq!(symbol_ty, ty);
                let stack_slot = *self.symbol_locations.get(symbol);
                Expr::Load { stack_slot, ty: ty.clone() }
            }
            ast::Expr::Binary { left, right, op } => {
                let int = *match ty {
                    Ty::Atomic(AtomicTy::Int(int)) => int,
                    _ => panic!(),
                };
                let left = Box::new(self.compile_expr(left, ty));
                let right = Box::new(self.compile_expr(right, ty));
                let op = match op {
                    ast::BinaryOp::Add => ArithmeticOp::Add,
                    ast::BinaryOp::Subtract => ArithmeticOp::Subtract,
                    ast::BinaryOp::Multiply => ArithmeticOp::Multiply,
                    ast::BinaryOp::Divide => ArithmeticOp::Divide,
                };
                Expr::Arithmetic { ty: int, left, right, op }
            }
        }
    }
}