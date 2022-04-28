use std::collections::{HashSet, HashMap};

use crate::{ast, typed_ast, ty::{TyRef, Ty, IntTyRef, IntTy, Signedness, Size, Int, StructTy, StructTyRef, Field}, infer::unify, symbols::Symbol};

struct Compiler<'a> {
    scope: Vec<Variable>,
    program: &'a ast::Program,
    returns: Option<TyRef>,
    blocks: Vec<typed_ast::Block>,
}

#[derive(Debug, Clone)]
struct Variable {
    name: Symbol,
    var: typed_ast::Variable,
    ty: TyRef,
}

pub fn compile_func(name: Symbol, func: &ast::Func, program: &ast::Program) -> Option<typed_ast::Func> {
    let body = match &func.body {
        Some(body) => body,
        None => return None,
    };
    let returns = func.returns.as_ref().map(|ty| compile_ty(&ty, program));
    let mut scope = vec![];
    let mut params = vec![];
    for param in &func.params {
        let ty = compile_ty(&param.ty, program);
        let var = typed_ast::Variable(scope.len() as u32);
        params.push(ty.clone());
        scope.push(Variable { name: param.name, ty, var });
    }
    let mut compiler = Compiler {
        scope,
        program,
        blocks: vec![],
        returns,
    };
    let mut block_id = compiler.new_block();
    compiler.compile_block(&body, &mut block_id);
    Some(typed_ast::Func {
        blocks: compiler.blocks,
        name,
        params,
    })
}

fn compile_struct(name: Symbol, program: &ast::Program) -> Ty {
    let structure = program.structs.get(&name).unwrap();
    let mut fields = vec![];
    for field in &structure.fields {
        let ty = compile_ty(&field.ty, program);
        fields.push(Field { name: field.name, ty });
    }
    Ty::Struct(StructTyRef::new(StructTy::Known { name, fields }))
}

pub fn compile_ty(ty: &ast::Ty, program: &ast::Program) -> TyRef {
    TyRef::new(match ty {
        ast::Ty::Int(int) => Ty::Int(IntTyRef::new(IntTy::Int(match int {
            ast::Int::I8 => Int { signedness: Signedness::Signed, size: Size::B8 },
            ast::Int::I16 => Int { signedness: Signedness::Signed, size: Size::B16 },
            ast::Int::I32 => Int { signedness: Signedness::Signed, size: Size::B32 },

            ast::Int::U8 => Int { signedness: Signedness::Unsigned, size: Size::B8 },
            ast::Int::U16 => Int { signedness: Signedness::Unsigned, size: Size::B16 },
            ast::Int::U32 => Int { signedness: Signedness::Unsigned, size: Size::B32 },
        }))),
        ast::Ty::Bool => Ty::Bool,
        ast::Ty::Struct(name) => compile_struct(*name, program),
        ast::Ty::Ref(ty) => Ty::Ref(compile_ty(ty, program)),
    })
}

fn deref_ty(ty: &TyRef) -> TyRef {
    let any_ty = TyRef::new(Ty::Any);
    let ref_ty = TyRef::new(Ty::Ref(any_ty.clone()));
    unify(&ref_ty, ty).unwrap();
    any_ty
}

impl<'a> Compiler<'a> {
    fn new_block(&mut self) -> typed_ast::BlockId {
        let id = typed_ast::BlockId(self.blocks.len() as u32);
        self.blocks.push(typed_ast::Block {
            stmts: vec![],
            branch: typed_ast::Branch::Return(None),
        });
        id
    }
    fn push_stmt(&mut self, id: typed_ast::BlockId, stmt: typed_ast::Stmt) {
        self.blocks[id.0 as usize].stmts.push(stmt);
    }
    fn set_branch(&mut self, id: typed_ast::BlockId, branch: typed_ast::Branch) {
        self.blocks[id.0 as usize].branch = branch;
    }
    fn compile_block(&mut self, block: &ast::Block, block_id: &mut typed_ast::BlockId) {
        for stmt in &block.stmts {
            self.compile_stmt(stmt, block_id);
        }
    }
    fn compile_stmt(&mut self, stmt: &ast::Stmt, block_id: &mut typed_ast::BlockId) {
        match stmt {
            ast::Stmt::While { cond, body } => {
                let mut loop_block = self.new_block();
                let cond_block = self.new_block();
                let exit_block = self.new_block();
                self.set_branch(*block_id, typed_ast::Branch::Static(cond_block));
                let (cond_expr, cond_ty) = self.compile_expr(cond);
                unify(&cond_ty, &TyRef::new(Ty::Bool)).unwrap();
                self.set_branch(cond_block, typed_ast::Branch::Condition {
                    expr: cond_expr,
                    if_true: loop_block,
                    if_false: exit_block,
                });
                self.compile_block(body, &mut loop_block);
                self.set_branch(loop_block, typed_ast::Branch::Static(cond_block));
                *block_id = exit_block;
            }
            ast::Stmt::Let { ident, expr, ty: ast_ty } => {
                let var = typed_ast::Variable(self.scope.len() as u32);
                let ty = TyRef::new(Ty::Any);
                self.scope.push(Variable { name: *ident, var, ty: ty.clone() });
                self.push_stmt(*block_id, typed_ast::Stmt::Alloc(ty.clone()));

                if let Some(ast_ty) = ast_ty {
                    let ast_ty = compile_ty(ast_ty, self.program);
                    unify(&ty, &ast_ty).unwrap();
                }

                if let Some(expr) = expr {
                    let (expr, expr_ty) = self.compile_expr(expr);
                    unify(&ty, &expr_ty).unwrap();
                    self.push_stmt(*block_id, typed_ast::Stmt::Assign {
                        ref_expr: typed_ast::RefExpr::Variable(var),
                        ty,
                        expr,
                    });
                }
            }
            ast::Stmt::Assign { ref_expr, expr } => {
                let (ref_expr, ref_ty) = self.compile_ref_expr(ref_expr);
                let (expr, ty) = self.compile_expr(expr);
                unify(&ref_ty, &ty).unwrap();
                self.push_stmt(*block_id, typed_ast::Stmt::Assign { ref_expr, expr, ty })
            }
            ast::Stmt::Return(expr) => {
                let expr = expr.as_ref().map(|expr| self.compile_expr(expr));
                let expr = match (expr, &self.returns) {
                    (Some((expr, ty)), Some(returns)) => {
                        unify(&returns, &ty).unwrap();
                        Some(expr)
                    },
                    (None, None) => None,
                    _ => panic!(),
                };
                self.set_branch(*block_id, typed_ast::Branch::Return(expr));
                *block_id = self.new_block();
            }
            ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
            ast::Stmt::FuncCall(fn_call) => {
                let (args, ty) = self.compile_fn_call(fn_call);
                if ty.is_some() {
                    panic!()
                }
                self.push_stmt(*block_id, typed_ast::Stmt::FuncCall(typed_ast::FuncCall {
                    name: fn_call.name,
                    args,
                }))
            }
        }
    }
    fn compile_ref_expr(&mut self, ref_expr: &ast::RefExpr) -> (typed_ast::RefExpr, TyRef) {
        match ref_expr {
            ast::RefExpr::Ident(name) => {
                let var = self.lookup_var(*name);
                (typed_ast::RefExpr::Variable(var.var), var.ty.clone())
            }
            ast::RefExpr::Field { ref_expr, name } => {
                let (ref_expr, ref_expr_ty) = self.compile_ref_expr(ref_expr);
                let field_ty = TyRef::new(Ty::Any);
                let mut with_fields = HashMap::new();
                with_fields.insert(*name, field_ty.clone());
                let struct_ty = StructTyRef::new(StructTy::WithFields(with_fields));
                let ty = TyRef::new(Ty::Struct(struct_ty.clone()));
                unify(&ty, &ref_expr_ty).unwrap();
                (typed_ast::RefExpr::Field { ref_expr: Box::new(ref_expr), name: *name, ty: struct_ty }, field_ty)
            }
            ast::RefExpr::Deref(expr) => {
                let (expr, ty) = self.compile_expr(expr);
                (typed_ast::RefExpr::Deref(Box::new(expr)), deref_ty(&ty))
            }
        }
    }
    fn compile_if(&mut self, if_stmt: &ast::If, block_id: &mut typed_ast::BlockId) {
        let mut if_block = self.new_block();
        let mut else_block = self.new_block();
        let (cond_expr, cond_ty) = self.compile_expr(&if_stmt.cond);
        self.set_branch(*block_id, typed_ast::Branch::Condition {
            expr: cond_expr,
            if_true: if_block,
            if_false: else_block,
        });
        self.compile_block(&if_stmt.if_block, &mut if_block);
        unify(&cond_ty, &TyRef::new(Ty::Bool)).unwrap();

        match &if_stmt.else_block {
            ast::Else::Block(else_ast_block) => {
                let exit_block_id = self.new_block();
                self.compile_block(else_ast_block, &mut else_block);
                self.set_branch(else_block, typed_ast::Branch::Static(exit_block_id));
                self.set_branch(if_block, typed_ast::Branch::Static(exit_block_id));
                *block_id = exit_block_id;
            }
            ast::Else::If(if_stmt) => {
                self.compile_if(if_stmt, &mut else_block);
                self.set_branch(if_block, typed_ast::Branch::Static(else_block));
                *block_id = else_block;
            }
            ast::Else::None => {
                self.set_branch(if_block, typed_ast::Branch::Static(else_block));
                *block_id = else_block;
            }
        }
    }
    fn lookup_var(&self, name: Symbol) -> &Variable {
        self.scope.iter().find(|var| var.name == name).unwrap()
    }
    fn compile_expr(&mut self, expr: &ast::Expr) -> (typed_ast::Expr, TyRef) {
        match expr {
            ast::Expr::Integer(value) => {
                let int_ty = IntTyRef::new(IntTy::Any);
                (typed_ast::Expr::Int(*value), TyRef::new(Ty::Int(int_ty)))
            }
            ast::Expr::Bool(value) =>  {
                (typed_ast::Expr::Bool(*value), TyRef::new(Ty::Bool))
            }
            ast::Expr::Infix { left, right, op } => {
                match op {
                    ast::InfixOp::Add => self.compile_arth_expr(left, right, typed_ast::BinaryOp::Add),
                    ast::InfixOp::Subtract => self.compile_arth_expr(left, right, typed_ast::BinaryOp::Subtract),
                    ast::InfixOp::Multiply => self.compile_arth_expr(left, right, typed_ast::BinaryOp::Multiply),
                    ast::InfixOp::Divide => self.compile_arth_expr(left, right, typed_ast::BinaryOp::Divide),

                    ast::InfixOp::LessThan => self.compile_cmp_expr(left, right, typed_ast::BinaryOp::LessThan),
                    ast::InfixOp::GreaterThan => self.compile_cmp_expr(left, right, typed_ast::BinaryOp::GreaterThan),
                }
            }
            ast::Expr::Ident(ident) => {
                let var = self.lookup_var(*ident);
                (typed_ast::Expr::Load {
                    var: var.var,
                    ty: var.ty.clone(),
                }, var.ty.clone())
            }
            ast::Expr::Ref(ref_expr) => {
                let (ref_expr, ty) = self.compile_ref_expr(ref_expr);
                (typed_ast::Expr::Ref(ref_expr), TyRef::new(Ty::Ref(ty)))
            }
            ast::Expr::Prefix { op, expr } => match op {
                ast::PrefixOp::Deref => {
                    let (expr, ty) = self.compile_expr(expr);
                    let ty = deref_ty(&ty);
                    (typed_ast::Expr::Deref { expr: Box::new(expr), ty: ty.clone() }, ty)
                }
            }
            ast::Expr::FuncCall(fn_call) => {
                let (args, ty) = self.compile_fn_call(fn_call);
                let result = ty.unwrap();
                (typed_ast::Expr::FuncCall(typed_ast::FuncCall {
                    name: fn_call.name,
                    args,
                }), result)
            }
            ast::Expr::InitStruct { name, values } => {
                let structure = self.program.structs.get(name).unwrap();
                let mut done = HashSet::new();
                let mut mir_values = vec![];
                let mut tys = vec![];
                for field in structure.fields.iter() {
                    if done.contains(&field.name) {
                        panic!()
                    }
                    done.insert(field.name);
                    let value = values.iter().find(|value| value.name == field.name).unwrap();
                    let (expr, ty) = self.compile_expr(&value.expr);
                    let field_ty = compile_ty(&field.ty, self.program);
                    unify(&ty, &field_ty).unwrap();
                    tys.push(field_ty);
                    mir_values.push(typed_ast::StructValue { ty, expr });
                }
                let ty = TyRef::new(compile_struct(*name, self.program));
                (typed_ast::Expr::InitStruct(mir_values), ty)
            }
            ast::Expr::Field { expr, name } => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let field_ty = TyRef::new(Ty::Any);
                let mut with_fields = HashMap::new();
                with_fields.insert(*name, field_ty.clone());
                let struct_ty = StructTyRef::new(StructTy::WithFields(with_fields));
                let derefs_ty = TyRef::new(Ty::Struct(struct_ty.clone()));
                unify(&derefs_ty, &expr_ty).unwrap();
                (typed_ast::Expr::Field { expr: Box::new(expr), name: *name, ty: struct_ty }, field_ty)
            }
        }
    }
    fn compile_fn_call(&mut self, func_call: &ast::FuncCall) -> (Vec<typed_ast::Expr>, Option<TyRef>) {
        let func = self.program.funcs.get(&func_call.name).unwrap();
        if func_call.args.len() != func.params.len() {
            panic!()
        }
        let args = func_call.args.iter().zip(&func.params).map(|(arg, param)| {
            let (expr, ty) = self.compile_expr(arg);
            let param_ty = compile_ty(&param.ty, self.program);
            unify(&ty, &param_ty).unwrap();
            expr
        }).collect();
        (args, func.returns.as_ref().map(|ty| compile_ty(ty, self.program)))
    }
    fn compile_arth_expr(&mut self, left: &ast::Expr, right: &ast::Expr, op: typed_ast::BinaryOp) -> (typed_ast::Expr, TyRef) {
        let (left_expr, left_ty) = self.compile_expr(left);
        let (right_expr, right_ty) = self.compile_expr(right);
        let int_ty = IntTyRef::new(IntTy::Any);
        let ty = TyRef::new(Ty::Int(int_ty.clone()));
        unify(&ty, &left_ty).unwrap();
        unify(&ty, &right_ty).unwrap();
        (typed_ast::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            ty: int_ty,
            op,
        }, ty)
    }
    fn compile_cmp_expr(&mut self, left: &ast::Expr, right: &ast::Expr, op: typed_ast::BinaryOp) -> (typed_ast::Expr, TyRef) {
        let (left_expr, left_ty) = self.compile_expr(left);
        let (right_expr, right_ty) = self.compile_expr(right);
        let int_ty = IntTyRef::new(IntTy::Any);
        let ty = TyRef::new(Ty::Int(int_ty.clone()));
        let bool_ty = TyRef::new(Ty::Bool);
        unify(&ty, &left_ty).unwrap();
        unify(&ty, &right_ty).unwrap();
        (typed_ast::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            ty: int_ty,
            op,
        }, bool_ty)
    }
}