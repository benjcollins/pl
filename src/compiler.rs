use crate::{ast, mir, ty::{TyRef, Ty, IntTyRef, IntTy, Signedness, Size}, infer::{unify, InferTy}};

struct Compiler<'a> {
    src: &'a str,
    scope: Vec<Variable>,
    fun: mir::Fun,
    return_ty: TyRef,
}

#[derive(Debug, Clone)]
struct Variable {
    name: ast::Ident,
    stack_slot: u32,
    ty: TyRef,
}

pub fn compile_fun<'a>(fun: &ast::Fun, src: &'a str) -> mir::Fun {
    let return_ty = match &fun.returns {
        Some(ty) => compile_ty(&ty, src),
        None => TyRef::known(Ty::None),
    };
    let mut compiler = Compiler {
        fun: mir::Fun::new(),
        scope: vec![],
        src,
        return_ty,
    };
    let mut block_id = compiler.fun.new_block();
    compiler.compile_block(&fun.block, &mut block_id);
    compiler.fun
}

fn compile_ty(ty: &ast::Ty, src: &str) -> TyRef {
    match ty {
        ast::Ty::Name(name) => match name.as_str(src) {
            "u8" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B8 }))),
            "u16" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B16 }))),
            "u32" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B32 }))),

            "i8" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B8 }))),
            "i16" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B16 }))),
            "i32" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B32 }))),

            "bool" => TyRef::known(Ty::Bool),

            _ => panic!(),
        }
        ast::Ty::Pointer(ty) => TyRef::known_with_args(Ty::Ref, vec![compile_ty(ty, src)]),
    }
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
                self.fun.get_block_mut(cond_block).branch = self.compile_bool_expr(cond, loop_block, exit_block);
                self.compile_block(body, &mut loop_block);
                self.fun.get_block_mut(loop_block).branch = mir::Branch::Static(cond_block);
                *block_id = exit_block;
            }
            ast::Stmt::Let { ident, expr, ty: ast_ty } => {
                let stack_slot = self.scope.len() as u32;
                let ty = TyRef::any();
                self.scope.push(Variable { name: *ident, stack_slot, ty: ty.clone() });
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Alloc(ty.clone()));
                
                if let Some(expr) = expr {
                    let (expr, expr_ty) = self.compile_expr(expr);
                    unify(&ty, &expr_ty).unwrap();
                    self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { assign: mir::Assign::Stack(stack_slot), expr });
                }
                if let Some(ast_ty) = ast_ty {
                    let ast_ty = compile_ty(ast_ty, self.src);
                    unify(&ty, &ast_ty).unwrap();
                }
            }
            ast::Stmt::Assign { assign, expr } => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let (assign, ty) = self.compile_assign(assign);
                unify(&expr_ty, &ty).unwrap();
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Assign { assign, expr });
            }
            ast::Stmt::Return { expr } => {
                let (expr, ty) = self.compile_expr(expr);
                unify(&ty, &self.return_ty).unwrap();
                self.fun.get_block_mut(*block_id).stmts.push(mir::Stmt::Return(expr))
            }
            ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
        }
    }
    fn compile_assign(&mut self, assign: &ast::Assign) -> (mir::Assign, TyRef) {
        match assign {
            ast::Assign::Deref(assign) => {
                let (assign, ty) = self.compile_assign(assign);
                (mir::Assign::Deref(Box::new(assign)), self.deref_ty(ty))
            }
            ast::Assign::Name(name) => {
                let var = self.lookup_var(*name);
                (mir::Assign::Stack(var.stack_slot), var.ty.clone())
            }
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
    fn lookup_var(&self, name: ast::Ident) -> &Variable {
        self.scope.iter().find(|var| var.name.as_str(self.src) == name.as_str(self.src)).unwrap()
    }
    fn compile_compare_expr(&mut self, left: &ast::Expr, right: &ast::Expr, op: mir::CompareOp, if_true: mir::BlockId, if_false: mir::BlockId) -> mir::Branch {
        let (left_expr, left_ty) = self.compile_expr(left);
        let (right_expr, right_ty) = self.compile_expr(right);
        let int_ty = TyRef::known(Ty::Int(IntTyRef::any()));
        unify(&int_ty, &left_ty).unwrap();
        unify(&int_ty, &right_ty).unwrap();
        mir::Branch::Compare {
            a: left_expr,
            b: right_expr,
            cmp: op,
            if_true,
            if_false,
        }
    }
    fn compile_bool_expr(&mut self, expr: &ast::Expr, if_true: mir::BlockId, if_false: mir::BlockId) -> mir::Branch {
        match expr {
            ast::Expr::Bool(value) => if *value {
                mir::Branch::Static(if_true)
            } else {
                mir::Branch::Static(if_false)
            }
            ast::Expr::Infix { left, right, op: ast::InfixOp::LessThan } => {
                self.compile_compare_expr(left, right, mir::CompareOp::LessThan, if_true, if_false)
            }
            ast::Expr::Infix { left, right, op: ast::InfixOp::GreaterThan } => {
                self.compile_compare_expr(left, right, mir::CompareOp::GreaterThan, if_true, if_false)
            }
            expr => {
                let (expr, ty) = self.compile_expr(expr);
                unify(&ty, &TyRef::known(Ty::Bool)).unwrap();
                mir::Branch::Bool { expr, if_true, if_false }
            }
        }
    }
    fn compile_expr(&mut self, expr: &ast::Expr) -> (mir::Expr, TyRef) {
        match expr {
            ast::Expr::Integer { start, end } => {
                let int_ty = IntTyRef::any();
                let value = self.src[*start..*end].parse().unwrap();
                (mir::Expr::Int { value, ty: int_ty.clone() }, TyRef::known(Ty::Int(int_ty)))
            }
            ast::Expr::Bool(value) =>  {
                (mir::Expr::Bool(*value), TyRef::known(Ty::Bool))
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
                    ty: var.ty.clone(),
                }, var.ty.clone())
            }
            ast::Expr::Prefix { op, expr } => match op {
                ast::PrefixOp::Deref => {
                    let (expr, ty) = self.compile_expr(expr);
                    let ty = self.deref_ty(ty);
                    (mir::Expr::Deref { expr: Box::new(expr), ty: ty.clone() }, ty)
                }
                ast::PrefixOp::Ref => match &**expr {
                    ast::Expr::Ident(name) => {
                        let var = self.lookup_var(*name);
                        (mir::Expr::Ref(var.stack_slot), TyRef::known_with_args(Ty::Ref, vec![var.ty.clone()]))
                    }
                    _ => panic!(),
                }
            }
        }
    }
    fn deref_ty(&mut self, ty: TyRef) -> TyRef {
        match &*ty.infer_ty() {
            InferTy::Any => {
                let any_ty = TyRef::any();
                let ref_ty = TyRef::known_with_args(Ty::Ref, vec![any_ty.clone()]);
                unify(&ty, &ref_ty).unwrap();
                any_ty
            }
            InferTy::Equal(ty) => self.deref_ty(ty.clone()),
            InferTy::Known { ty: Ty::Ref, args } => args[0].clone(),
            _ => panic!(),
        }
    }
    fn compile_binary_expr(&mut self, op: mir::BinaryOp, left_expr: mir::Expr, right_expr: mir::Expr, left_ty: TyRef, right_ty: TyRef) -> (mir::Expr, TyRef) {
        let ty = TyRef::known(Ty::Int(IntTyRef::any()));
        unify(&ty, &left_ty).unwrap();
        unify(&ty, &right_ty).unwrap();
        (mir::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            op,
        }, ty)
    }
}