use crate::{ast, mir::{self, Ty, IntTy, TyName, Int, Signedness, Size, IntTyName}, token::{Token, TokenKind}};

struct Compiler<'a> {
    src: &'a str,
    scope: Vec<Variable>,
    fun: mir::Fun,
    return_ty: TyName,
}

#[derive(Debug, Clone, Copy)]
struct Variable {
    name: ast::Ident,
    stack_slot: u32,
    ty: TyName,
}

pub fn compile_fun<'a>(fun: &ast::Fun, src: &'a str) -> mir::Fun {
    let mut compiler = Compiler {
        fun: mir::Fun::new(),
        scope: vec![],
        src,
        return_ty: TyName::default(),
    };
    compiler.return_ty = match &fun.returns {
        Some(ty) => compiler.compile_ty(&ty),
        None => compiler.fun.new_ty_name(Ty::None),
    };
    let mut block_id = compiler.fun.new_block();
    compiler.compile_block(&fun.block, &mut block_id);
    compiler.fun
}

impl<'a> Compiler<'a> {
    fn compile_block(&mut self, block: &ast::Block, block_id: &mut mir::BlockId) {
        for stmt in &block.stmts {
            self.compile_stmt(stmt, block_id);
        }
    }
    fn compile_stmt(&mut self, stmt: &ast::Stmt, block_id: &mut mir::BlockId) {
        match stmt {
            ast::Stmt::Let { ident, expr, ty: ast_ty } => {
                let stack_slot = self.scope.len() as u32;
                let ty = self.fun.new_ty_name(Ty::Any);
                self.scope.push(Variable { name: *ident, stack_slot, ty });
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Alloc(ty));
                
                if let Some(expr) = expr {
                    let (expr, expr_ty) = self.compile_expr(expr);
                    self.unify(ty, expr_ty);
                    self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { stack_slot, expr });
                }
                if let Some(ast_ty) = ast_ty {
                    let ast_ty = self.compile_ty(ast_ty);
                    self.unify(ty, ast_ty);
                }
            }
            ast::Stmt::Assign { ident, expr } => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let var = self.lookup_var(*ident);
                self.unify(var.ty, expr_ty);
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { stack_slot: var.stack_slot, expr });
            }
            ast::Stmt::Return { expr } => {
                let (expr, ty) = self.compile_expr(expr);
                self.unify(ty, self.return_ty);
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Return(expr))
            }
            ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
        }
    }
    fn compile_if(&mut self, if_stmt: &ast::If, block_id: &mut mir::BlockId) {
        let mut if_block_id = self.fun.new_block();
        let mut else_block_id = self.fun.new_block();
        self.compile_block(&if_stmt.if_block, &mut if_block_id);
        self.fun.get_block_mut(*block_id).branch = self.compile_bool_expr(&if_stmt.cond, if_block_id, else_block_id);

        match &if_stmt.else_block {
            ast::Else::Block(else_ast_block) => {
                let exit_block_id = self.fun.new_block();
                self.compile_block(else_ast_block, &mut else_block_id);
                self.fun.get_block_mut(else_block_id).branch = mir::Branch::Static(exit_block_id);
                self.fun.get_block_mut(if_block_id).branch = mir::Branch::Static(exit_block_id);
                *block_id = exit_block_id;
            }
            ast::Else::If(if_stmt) => {
                self.compile_if(if_stmt, &mut else_block_id);
                self.fun.get_block_mut(if_block_id).branch = mir::Branch::Static(else_block_id);
                *block_id = else_block_id;
            }
            ast::Else::None => {
                self.fun.get_block_mut(if_block_id).branch = mir::Branch::Static(else_block_id);
                *block_id = else_block_id;
            }
        }
    }
    fn lookup_var(&self, name: ast::Ident) -> Variable {
        *self.scope.iter().find(|var| var.name.as_str(self.src) == name.as_str(self.src)).unwrap()
    }
    fn new_int_ty(&mut self, int_ty: IntTy) -> TyName {
        let int_ty = self.fun.new_int_ty_name(int_ty);
        self.fun.new_ty_name(Ty::Int(int_ty))
    }
    fn compile_ty(&mut self, ty: &ast::Ty) -> TyName {
        match ty.name.as_str(self.src) {
            "u8" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B8 })),
            "u16" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B16 })),
            "u32" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B32 })),
            
            "i8" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B8 })),
            "i16" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B16 })),
            "i32" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B32 })),

            "bool" => self.fun.new_ty_name(Ty::Bool),

            _ => panic!(),
        }
    }
    fn compile_bool_expr(&mut self, expr: &ast::Expr, if_true: mir::BlockId, if_false: mir::BlockId) -> mir::Branch {
        match expr {
            ast::Expr::Integer { .. } => panic!(),
            ast::Expr::Bool(value) => if *value {
                mir::Branch::Static(if_true)
            } else {
                mir::Branch::Static(if_false)
            }
            ast::Expr::Ident(ident) => {
                let var = self.lookup_var(*ident);
                mir::Branch::Bool {
                    expr: mir::Expr::Load {
                        stack_slot: var.stack_slot,
                        ty: self.fun.new_ty_name(Ty::Bool),
                    },
                    if_true,
                    if_false,
                }
            }
            ast::Expr::Binary { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                let int_ty = self.new_int_ty(IntTy::Any);
                self.unify(int_ty, left_ty);
                self.unify(int_ty, right_ty);
                let cmp = match op {
                    ast::BinaryOp::LessThan => mir::Compare::LessThan,
                    ast::BinaryOp::GreaterThan => mir::Compare::GreaterThan,
                    _ => panic!(),
                };
                mir::Branch::Comparison {
                    a: left_expr,
                    b: right_expr,
                    cmp,
                    if_true,
                    if_false,
                }
            }
        }
    }
    fn compile_expr(&mut self, expr: &ast::Expr) -> (mir::Expr, TyName) {
        match expr {
            ast::Expr::Integer { offset } => {
                let int_ty = self.fun.new_int_ty_name(IntTy::Any);
                let ty = self.fun.new_ty_name(Ty::Int(int_ty));
                let value = Token::new(*offset, TokenKind::Integer).as_str(self.src).parse().unwrap();
                (mir::Expr::Int { value, ty: int_ty }, ty)
            }
            ast::Expr::Bool(value) =>  {
                (mir::Expr::Bool(*value), self.fun.new_ty_name(Ty::Bool))
            }
            ast::Expr::Binary { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                match op {
                    ast::BinaryOp::Add => self.compile_binary_expr(mir::BinaryOp::Add, left_expr, right_expr, left_ty, right_ty),
                    ast::BinaryOp::Subtract => self.compile_binary_expr(mir::BinaryOp::Subtract, left_expr, right_expr, left_ty, right_ty),
                    ast::BinaryOp::Multiply => self.compile_binary_expr(mir::BinaryOp::Multiply, left_expr, right_expr, left_ty, right_ty),
                    ast::BinaryOp::Divide => self.compile_binary_expr(mir::BinaryOp::Divide, left_expr, right_expr, left_ty, right_ty),
                    ast::BinaryOp::LessThan => todo!(),
                    ast::BinaryOp::GreaterThan => todo!(),
                }
            }
            ast::Expr::Ident(ident) => {
                let var = self.lookup_var(*ident);
                (mir::Expr::Load {
                    stack_slot: var.stack_slot,
                    ty: var.ty,
                }, var.ty)
            }
        }
    }
    fn compile_binary_expr(&mut self, op: mir::BinaryOp, left_expr: mir::Expr, right_expr: mir::Expr, left_ty: TyName, right_ty: TyName) -> (mir::Expr, TyName) {
        let int_ty = self.fun.new_int_ty_name(IntTy::Any);
        let ty = self.fun.new_ty_name(Ty::Int(int_ty));
        self.unify(ty, left_ty);
        self.unify(ty, right_ty);
        (mir::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            op,
        }, ty)
    }
    fn unify(&mut self, a: TyName, b: TyName) {
        match (self.fun.get_ty(a), self.fun.get_ty(b)) {
            (&Ty::Equal(a), _) => self.unify(a, b),
            (_, &Ty::Equal(b)) => self.unify(a, b),
            
            (Ty::Any, _) => self.fun.assign_ty(a, Ty::Equal(b)),
            (_, Ty::Any) => self.fun.assign_ty(b, Ty::Equal(a)),

            (Ty::Bool, Ty::Bool) => (),
            (Ty::None, Ty::None) => (),

            (Ty::Int(_), Ty::Bool) | (Ty::Bool, Ty::Int(_)) => panic!(),
            (Ty::Bool, Ty::None) | (Ty::None, Ty::Bool) => panic!(),
            (Ty::None, Ty::Int(_)) | (Ty::Int(_), Ty::None) => panic!(),
            
            (&Ty::Int(a), &Ty::Int(b)) => self.unify_ints(a, b),
        }
    }
    fn unify_ints(&mut self, a: IntTyName, b: IntTyName) {
        match (self.fun.get_int_ty(a), self.fun.get_int_ty(b)) {
            (&IntTy::Equal(a), _) => self.unify_ints(a, b),
            (_, &IntTy::Equal(b)) => self.unify_ints(a, b),
            
            (IntTy::Any, _) => self.fun.assign_int_ty(a, IntTy::Equal(b)),
            (_, IntTy::Any) => self.fun.assign_int_ty(b, IntTy::Equal(a)),

            (IntTy::Int(a), IntTy::Int(b)) => if a != b { panic!() }
        }
    }
}