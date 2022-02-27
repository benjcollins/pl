use crate::{ty::{Int, Ty, AtomicTy}, symbol::{Symbol, SymbolMap, SymbolTable}, ast, token::{Token, TokenKind}};

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
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
}

impl<'a> Compiler<'a> {
    pub fn compile_fun(src: &str, symbols: &SymbolTable, symbol_tys: &SymbolMap<Ty>, return_ty: &Ty, fun: &ast::Fun) -> Block {
        let mut compiler = Compiler {
            src,
            symbols,
            symbol_tys,
            symbol_locations: symbols.create_symbol_map(),
            stack_slots: vec![],
            return_ty,
        };
        compiler.compile_block(&fun.block)
    }
    fn compile_block(&mut self, ast_block: &ast::Block) -> Block {
        let stack_len = self.stack_slots.len();
        let mut block = Block { stmts: vec![] };
        for stmt in &ast_block.stmts {
            self.compile_stmt(&stmt, &mut block);
        }
        for _ in self.stack_slots.iter().rev().take(self.stack_slots.len() - stack_len) {
            block.stmts.push(Stmt::Drop)
        }
        self.stack_slots.truncate(stack_len);
        block
    }
    fn compile_stmt(&mut self, stmt: &ast::Stmt, block: &mut Block) {
        match stmt {
            ast::Stmt::Let { ident, expr, .. } => {
                let symbol = self.symbols.ident_to_symbol(*ident);
                let ty = self.symbol_tys.get(symbol);
                block.stmts.push(Stmt::Alloc(ty.clone()));
                self.symbol_locations.update(symbol, self.stack_slots.len());
                self.stack_slots.push(ty.clone());
                if let Some(expr) = expr {
                    self.compile_assign(symbol, expr, block);
                }
            }
            ast::Stmt::Assign { ident, expr } => {
                let symbol = self.symbols.ident_to_symbol(*ident);
                self.compile_assign(symbol, expr, block)
            }
            ast::Stmt::Return { expr } => {
                let expr = self.compile_expr(expr, self.return_ty);
                block.stmts.push(Stmt::Return { ty: self.return_ty.clone(), expr });
            }
        }
    }
    fn compile_assign(&self, symbol: Symbol, expr: &ast::Expr, block: &mut Block) {
        let ty = self.symbol_tys.get(symbol);
        let expr = self.compile_expr(expr, ty);
        let stack_slot = *self.symbol_locations.get(symbol);
        block.stmts.push(Stmt::Assign { stack_slot, expr, ty: ty.clone() })
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