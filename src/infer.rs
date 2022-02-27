use crate::{ast::{Fun, Stmt, Expr, BinaryOp, self}, symbol::{SymbolTable, SymbolMap}};

pub struct Context<'sym, 'src> {
    tys: Vec<Ty>,
    symbol_tys: SymbolMap<TyName>,
    symbols: &'sym SymbolTable,
    src: &'src str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct TyName(usize);

#[derive(Debug, Clone, Copy)]
enum Ty {
    Any,
    Equal(TyName),
    AnyInt,
    Int(Int),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int {
    pub signedness: Signedness,
    pub size: Size,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    B8,
    B16,
    B32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteTy {
    Int(Int),
    None,
}

impl Default for TyName {
    fn default() -> Self {
        TyName(0)
    }
}

impl<'sym, 'src> Context<'sym, 'src> {
    pub fn infer(fun: &Fun, symbols: &'sym SymbolTable, src: &'src str) -> (SymbolMap<ConcreteTy>, ConcreteTy) {
        let mut ctx = Context {
            symbols,
            symbol_tys: symbols.create_symbol_map(),
            tys: vec![Ty::Any],
            src,
        };
        let return_ty = fun.returns.as_ref().map_or(Ty::None, |ty| ctx.ast_to_ty(ty));
        let return_ty_name = ctx.new_ty_name(return_ty);
        for stmt in &fun.block.stmts {
            ctx.infer_stmt(stmt, return_ty_name);
        }
        let symbol_tys = ctx.symbol_tys.map(|_, name| ctx.to_concrete(*name));
        (symbol_tys, ctx.to_concrete(return_ty_name))
    }
    fn to_concrete(&self, name: TyName) -> ConcreteTy {
        match *self.get_ty(name) {
            Ty::Int(int) => ConcreteTy::Int(int),
            Ty::Equal(name) => self.to_concrete(name),
            Ty::AnyInt => ConcreteTy::Int(Int { size: Size::B32, signedness: Signedness::Unsigned }),
            Ty::None => ConcreteTy::None,
            Ty::Any => panic!(),
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
                    (None, None) => self.new_ty_name(Ty::Any),
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
            Expr::Integer { .. } => self.new_ty_name(Ty::AnyInt),
            Expr::Ident(ident) => *self.symbol_tys.get(self.symbols.ident_to_symbol(*ident)),
            Expr::Binary { left, right, op } => match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                    let left_ty = self.ty_of_expr(&left);
                    let right_ty = self.ty_of_expr(&right);
                    let any_int_ty = self.new_ty_name(Ty::AnyInt);
                    self.unify(left_ty, any_int_ty);
                    self.unify(right_ty, any_int_ty);
                    any_int_ty
                }
            }
        }
    }
    fn ast_to_ty(&mut self, ty: &ast::Ty) -> Ty {
        let ty = match ty.name.as_str(self.src) {
            "u8" => Int { signedness: Signedness::Unsigned, size: Size::B8 },
            "u16" => Int { signedness: Signedness::Unsigned, size: Size::B16 },
            "u32" => Int { signedness: Signedness::Unsigned, size: Size::B32 },
            "i8" => Int { signedness: Signedness::Signed, size: Size::B8 },
            "i16" => Int { signedness: Signedness::Signed, size: Size::B16 },
            "i32" => Int { signedness: Signedness::Signed, size: Size::B32 },
            _ => panic!(),
        };
        Ty::Int(ty)
    }
    fn new_ty_name(&mut self, ty: Ty) -> TyName {
        let name = TyName(self.tys.len());
        self.tys.push(ty);
        name
    }
    fn get_ty(&self, name: TyName) -> &Ty {
        &self.tys[name.0]
    }
    fn get_ty_mut(&mut self, name: TyName) -> &mut Ty {
        &mut self.tys[name.0]
    }
    fn unify(&mut self, a: TyName, b: TyName) {
        if a == b { return }
        match (*self.get_ty(a), *self.get_ty(b)) {
            (Ty::Equal(a), _) => self.unify(a, b),
            (_, Ty::Equal(b)) => self.unify(a, b),

            (Ty::Any, _) => *self.get_ty_mut(a) = Ty::Equal(b),
            (_, Ty::Any) => *self.get_ty_mut(b) = Ty::Equal(a),

            (Ty::AnyInt, Ty::AnyInt) => *self.get_ty_mut(a) = Ty::Equal(b),
            
            (Ty::AnyInt, Ty::Int(int)) | (Ty::Int(int), Ty::AnyInt) => {
                *self.get_ty_mut(a) = Ty::Int(int);
                *self.get_ty_mut(b) = Ty::Int(int);
            }
            (Ty::Int(a), Ty::Int(b)) => if a != b {
                panic!()
            }
            (Ty::None, Ty::None) => {},

            _ => panic!(),
        }
    }
}