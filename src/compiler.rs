use crate::{ast, mir::{self, Ty, IntTy, TyName, Int, Signedness, Size, IntTyName}};

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
            ast::Stmt::While { cond, body } => {
                let mut loop_block = self.fun.new_block();
                let cond_block = self.fun.new_block();
                let exit_block = self.fun.new_block();
                self.fun.get_block_mut(*block_id).branch = mir::Branch::Static(cond_block);
                let (cond_expr, cond_ty) = self.compile_expr(cond);
                let bool_ty = self.fun.new_ty_name(Ty::Bool);
                self.unify(bool_ty, cond_ty);
                self.fun.get_block_mut(*block_id).branch = mir::Branch::Conditional {
                    expr: cond_expr,
                    if_true: loop_block,
                    if_false: exit_block,
                };
                self.compile_block(body, &mut loop_block);
                self.fun.get_block_mut(loop_block).branch = mir::Branch::Static(cond_block);
                *block_id = exit_block;
            }
            ast::Stmt::Let { ident, expr, ty: ast_ty } => {
                let stack_slot = self.scope.len() as u32;
                let ty = self.fun.new_ty_name(Ty::Any);
                self.scope.push(Variable { name: *ident, stack_slot, ty });
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Alloc(ty));
                
                if let Some(expr) = expr {
                    let (expr, expr_ty) = self.compile_expr(expr);
                    self.unify(ty, expr_ty);
                    self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { assign: mir::Assign::Stack(stack_slot), expr });
                }
                if let Some(ast_ty) = ast_ty {
                    let ast_ty = self.compile_ty(ast_ty);
                    self.unify(ty, ast_ty);
                }
            }
            ast::Stmt::Assign { assign, expr } => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let (assign, ty) = self.compile_assign(assign);
                self.unify(expr_ty, ty);
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { assign, expr });
            }
            ast::Stmt::Return { expr } => {
                let (expr, ty) = self.compile_expr(expr);
                self.unify(ty, self.return_ty);
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Return(expr))
            }
            ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
        }
    }
    fn compile_assign(&mut self, assign: &ast::Assign) -> (mir::Assign, TyName) {
        match assign {
            ast::Assign::Deref(assign) => {
                let (assign, ty) = self.compile_assign(assign);
                (mir::Assign::Deref(Box::new(assign)), self.deref_ty(ty))
            }
            ast::Assign::Name(name) => {
                let var = self.lookup_var(*name);
                (mir::Assign::Stack(var.stack_slot), var.ty)
            }
        }
    }
    fn compile_if(&mut self, if_stmt: &ast::If, block_id: &mut mir::BlockId) {
        let mut if_block_id = self.fun.new_block();
        let mut else_block_id = self.fun.new_block();
        self.compile_block(&if_stmt.if_block, &mut if_block_id);
        let (cond_expr, cond_ty) = self.compile_expr(&if_stmt.cond);
        let bool_ty = self.fun.new_ty_name(Ty::Bool);
        self.unify(bool_ty, cond_ty);
        self.fun.get_block_mut(*block_id).branch = mir::Branch::Conditional {
            expr: cond_expr,
            if_true: if_block_id,
            if_false: else_block_id,
        };

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
        match ty {
            ast::Ty::Name(name) => match name.as_str(self.src) {
                "u8" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B8 })),
                "u16" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B16 })),
                "u32" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Unsigned, size: Size::B32 })),
                
                "i8" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B8 })),
                "i16" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B16 })),
                "i32" => self.new_int_ty(IntTy::Int(Int { signedness: Signedness::Signed, size: Size::B32 })),
    
                "bool" => self.fun.new_ty_name(Ty::Bool),
    
                _ => panic!(),
            }
            ast::Ty::Pointer(ty) => {
                let ty = self.compile_ty(ty);
                self.fun.new_ty_name(Ty::Ref(ty))
            }
        }
    }
    fn compile_expr(&mut self, expr: &ast::Expr) -> (mir::Expr, TyName) {
        match expr {
            ast::Expr::Integer { start, end } => {
                let int_ty = self.fun.new_int_ty_name(IntTy::Any);
                let ty = self.fun.new_ty_name(Ty::Int(int_ty));
                let value = self.src[*start..*end].parse().unwrap();
                (mir::Expr::Int { value, ty: int_ty }, ty)
            }
            ast::Expr::Bool(value) =>  {
                (mir::Expr::Bool(*value), self.fun.new_ty_name(Ty::Bool))
            }
            ast::Expr::Infix { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                match op {
                    ast::InfixOp::Add => self.compile_binary_expr(mir::BinaryOp::Add, left_expr, right_expr, left_ty, right_ty),
                    ast::InfixOp::Subtract => self.compile_binary_expr(mir::BinaryOp::Subtract, left_expr, right_expr, left_ty, right_ty),
                    ast::InfixOp::Multiply => self.compile_binary_expr(mir::BinaryOp::Multiply, left_expr, right_expr, left_ty, right_ty),
                    ast::InfixOp::Divide => self.compile_binary_expr(mir::BinaryOp::Divide, left_expr, right_expr, left_ty, right_ty),
                    ast::InfixOp::LessThan => todo!(),
                    ast::InfixOp::GreaterThan => todo!(),
                }
            }
            ast::Expr::Ident(ident) => {
                let var = self.lookup_var(*ident);
                (mir::Expr::Load {
                    stack_slot: var.stack_slot,
                    ty: var.ty,
                }, var.ty)
            }
            ast::Expr::Prefix { op, expr } => match op {
                ast::PrefixOp::Deref => {
                    let (expr, ty) = self.compile_expr(expr);
                    let ty = self.deref_ty(ty);
                    (mir::Expr::Deref { expr: Box::new(expr), ty }, ty)
                }
                ast::PrefixOp::Ref => match &**expr {
                    ast::Expr::Ident(name) => {
                        let var = self.lookup_var(*name);
                        let ty = self.fun.new_ty_name(Ty::Ref(var.ty));
                        (mir::Expr::Ref(var.stack_slot), ty)
                    }
                    _ => panic!(),
                }
            }
        }
    }
    fn deref_ty(&mut self, ty: TyName) -> TyName {
        match self.fun.get_ty(ty) {
            Ty::Any => {
                let any_ty = self.fun.new_ty_name(Ty::Any);
                let ref_ty = self.fun.new_ty_name(Ty::Ref(any_ty));
                self.unify(ty, ref_ty);
                any_ty
            }
            &Ty::Equal(ty) => self.deref_ty(ty),
            Ty::Ref(ty) => *ty,
            _ => panic!(),
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
        if a == b { return }
        match (self.fun.get_ty(a), self.fun.get_ty(b)) {
            (&Ty::Equal(a), _) => self.unify(a, b),
            (_, &Ty::Equal(b)) => self.unify(a, b),
            
            (Ty::Any, _) => self.fun.assign_ty(a, Ty::Equal(b)),
            (_, Ty::Any) => self.fun.assign_ty(b, Ty::Equal(a)),

            (Ty::Bool, Ty::Bool) => (),
            (Ty::None, Ty::None) => (),
            (&Ty::Int(a), &Ty::Int(b)) => self.unify_ints(a, b),
            (&Ty::Ref(a), &Ty::Ref(b)) => self.unify(a, b),

            (Ty::Int(_), _) | (_, Ty::Int(_)) => panic!(),
            (Ty::Bool, _) | (_, Ty::Bool) => panic!(),
            (Ty::None, _) | (_, Ty::None) => panic!(),
            (Ty::Ref(_), _) | (_, Ty::Ref(_)) => panic!(),
        }
    }
    fn unify_ints(&mut self, a: IntTyName, b: IntTyName) {
        if a == b { return }
        match (self.fun.get_int_ty(a), self.fun.get_int_ty(b)) {
            (&IntTy::Equal(a), _) => self.unify_ints(a, b),
            (_, &IntTy::Equal(b)) => self.unify_ints(a, b),
            
            (IntTy::Any, _) => self.fun.assign_int_ty(a, IntTy::Equal(b)),
            (_, IntTy::Any) => self.fun.assign_int_ty(b, IntTy::Equal(a)),

            (IntTy::Int(a), IntTy::Int(b)) => if a != b { panic!() }
        }
    }
}