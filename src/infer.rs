use crate::{ast::{Fun, Stmt, Expr, BinaryOp, self}, symbol::{SymbolTable, SymbolMap}, ty::{AtomicTy, Int, Signedness, Size, Ty}};

pub struct Context<'sym, 'src> {
    tys: Vec<InferTy>,
    symbol_tys: SymbolMap<TyName>,
    symbols: &'sym SymbolTable,
    src: &'src str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct TyName(usize);

#[derive(Debug, Clone, Copy)]
enum InferTy {
    Any,
    Equal(TyName),
    AnyInt,
    Atomic(AtomicTy),
}

impl Default for TyName {
    fn default() -> Self {
        TyName(0)
    }
}

impl<'sym, 'src> Context<'sym, 'src> {
    pub fn infer(fun: &Fun, symbols: &'sym SymbolTable, src: &'src str) -> (SymbolMap<Ty>, Ty) {
        let mut ctx = Context {
            symbols,
            symbol_tys: symbols.create_symbol_map(),
            tys: vec![InferTy::Any],
            src,
        };
        let return_ty = fun.returns.as_ref().map_or(InferTy::Atomic(AtomicTy::None), |ty| ctx.ast_to_ty(ty));
        let return_ty_name = ctx.new_ty_name(return_ty);
        for stmt in &fun.block.stmts {
            ctx.infer_stmt(stmt, return_ty_name);
        }
        let symbol_tys = ctx.symbol_tys.map(|_, name| ctx.to_concrete(*name));
        (symbol_tys, ctx.to_concrete(return_ty_name))
    }
    fn to_concrete(&self, name: TyName) -> Ty {
        match *self.get_ty(name) {
            InferTy::Atomic(ty) => Ty::Atomic(ty),
            InferTy::Equal(name) => self.to_concrete(name),
            InferTy::AnyInt => Ty::Atomic(AtomicTy::Int(Int { size: Size::B32, signedness: Signedness::Unsigned })),
            InferTy::Any => panic!(),
        }
    }
    fn infer_stmt(&mut self, stmt: &Stmt, return_ty: TyName) {
        match stmt {
            Stmt::Let { ident, expr, ty } => {
                let expr_ty = expr.as_ref().map(|expr| self.ty_of_expr(expr));
                let ast_ty = ty.as_ref().map(|ty| {
                    let ty = self.ast_to_ty(ty);
                    self.new_ty_name(ty)
                });
                let ty = match (expr_ty, ast_ty) {
                    (None, None) => self.new_ty_name(InferTy::Any),
                    (None, Some(ast_ty)) => ast_ty,
                    (Some(expr_ty), None) => expr_ty,
                    (Some(expr_ty), Some(ast_ty)) => {
                        self.unify(expr_ty, ast_ty);
                        expr_ty
                    }
                };
                self.symbol_tys.update(self.symbols.ident_to_symbol(*ident), ty);
            }
            Stmt::Assign { ident, expr } => {
                let symbol_ty = *self.symbol_tys.get(self.symbols.ident_to_symbol(*ident));
                let expr_ty = self.ty_of_expr(&expr);
                self.unify(symbol_ty, expr_ty);
            }
            Stmt::Return { expr } => {
                let expr_ty = self.ty_of_expr(expr);
                self.unify(expr_ty, return_ty);
            }
        }
    }
    fn ty_of_expr(&mut self, expr: &Expr) -> TyName {
        match expr {
            Expr::Bool(_) => self.new_ty_name(InferTy::Atomic(AtomicTy::Bool)),
            Expr::Integer { .. } => self.new_ty_name(InferTy::AnyInt),
            Expr::Ident(ident) => *self.symbol_tys.get(self.symbols.ident_to_symbol(*ident)),
            Expr::Binary { left, right, op } => match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                    let left_ty = self.ty_of_expr(&left);
                    let right_ty = self.ty_of_expr(&right);
                    let any_int_ty = self.new_ty_name(InferTy::AnyInt);
                    self.unify(left_ty, any_int_ty);
                    self.unify(right_ty, any_int_ty);
                    any_int_ty
                }
            }
        }
    }
    fn ast_to_ty(&mut self, ty: &ast::Ty) -> InferTy {
        match ty.name.as_str(self.src) {
            "u8" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B8 })),
            "u16" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B16 })),
            "u32" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B32 })),
            
            "i8" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Signed, size: Size::B8 })),
            "i16" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Signed, size: Size::B16 })),
            "i32" => InferTy::Atomic(AtomicTy::Int(Int { signedness: Signedness::Signed, size: Size::B32 })),

            "bool" => InferTy::Atomic(AtomicTy::Bool),

            _ => panic!(),
        }
    }
    fn new_ty_name(&mut self, ty: InferTy) -> TyName {
        let name = TyName(self.tys.len());
        self.tys.push(ty);
        name
    }
    fn get_ty(&self, name: TyName) -> &InferTy {
        &self.tys[name.0]
    }
    fn get_ty_mut(&mut self, name: TyName) -> &mut InferTy {
        &mut self.tys[name.0]
    }
    fn unify(&mut self, a: TyName, b: TyName) {
        if a == b { return }
        match (*self.get_ty(a), *self.get_ty(b)) {
            (InferTy::Equal(a), _) => self.unify(a, b),
            (_, InferTy::Equal(b)) => self.unify(a, b),

            (InferTy::Any, _) => *self.get_ty_mut(a) = InferTy::Equal(b),
            (_, InferTy::Any) => *self.get_ty_mut(b) = InferTy::Equal(a),

            (InferTy::AnyInt, InferTy::AnyInt) => *self.get_ty_mut(a) = InferTy::Equal(b),

            (InferTy::AnyInt, InferTy::Atomic(AtomicTy::Int(int))) | (InferTy::Atomic(AtomicTy::Int(int)), InferTy::AnyInt) => {
                *self.get_ty_mut(a) = InferTy::Atomic(AtomicTy::Int(int));
                *self.get_ty_mut(b) = InferTy::Atomic(AtomicTy::Int(int));
            }
            (InferTy::AnyInt, InferTy::Atomic(_)) | (InferTy::Atomic(_), InferTy::AnyInt) => { panic!() }
            (InferTy::Atomic(a), InferTy::Atomic(b)) => if a != b { panic!() }
        }
    }
}