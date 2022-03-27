use std::collections::HashSet;

use crate::{ast::{self, Program}, mir, ty::{TyRef, Ty, IntTyRef, IntTy, Signedness, Size}, infer::{unify, InferTy}};

struct Compiler<'s, 'c> {
    scope: Vec<Variable<'s>>,
    program: &'c ast::Program<'s>,
    returns: Option<TyRef<'s>>,
    blocks: Vec<mir::Block<'s>>,
}

#[derive(Debug, Clone)]
struct Variable<'s> {
    name: &'s str,
    stack_slot: u32,
    ty: TyRef<'s>,
}

pub fn compile_fun<'s>(name: &'s str, func: &ast::Func<'s>, program: &'s ast::Program) -> Option<mir::Func<'s>> {
    let body = match &func.body {
        Some(body) => body,
        None => return None,
    };
    let returns = func.returns.as_ref().map(|ty| compile_ty(&ty, program));
    let mut scope = vec![];
    let mut params = vec![];
    for param in &func.params {
        let ty = compile_ty(&param.ty, program);
        let stack_slot = scope.len() as u32;
        params.push(ty.clone());
        scope.push(Variable { name: param.name, ty, stack_slot });
    }
    let mut compiler = Compiler {
        scope,
        program,
        blocks: vec![],
        returns,
    };
    let mut block_id = compiler.new_block();
    compiler.compile_block(&body, &mut block_id);
    Some(mir::Func {
        blocks: compiler.blocks,
        returns: compiler.returns,
        name,
        params,
    })
}

pub fn compile_ty<'a>(ty: &ast::Ty<'a>, program: &Program<'a>) -> TyRef<'a> {
    match ty {
        ast::Ty::Name(name) => match *name {
            "u8" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B8 }))),
            "u16" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B16 }))),
            "u32" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Unsigned, size: Size::B32 }))),

            "i8" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B8 }))),
            "i16" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B16 }))),
            "i32" => TyRef::known(Ty::Int(IntTyRef::known(IntTy { signedness: Signedness::Signed, size: Size::B32 }))),

            "bool" => TyRef::known(Ty::Bool),

            name => {
                let structure = program.structs.get(name).unwrap();
                let tys = structure.fields.iter().map(|field| compile_ty(&field.ty, program)).collect();
                TyRef::known(Ty::Struct { name, tys })
            },
        }
        ast::Ty::Ref(ty) => TyRef::known(Ty::Ref(compile_ty(ty, program))),
    }
}

fn deref_ty<'a>(ty: &TyRef<'a>) -> TyRef<'a> {
    match &*ty.infer_ty() {
        InferTy::Any => {
            let any_ty = TyRef::any();
            let ref_ty = TyRef::known(Ty::Ref(any_ty.clone()));
            unify(&ty, &ref_ty).unwrap();
            any_ty
        }
        InferTy::Equal(ty) => deref_ty(ty),
        InferTy::Known(Ty::Ref(ty)) => ty.clone(),
        _ => panic!(),
    }
}

impl<'s, 'c> Compiler<'s, 'c> {
    fn new_block(&mut self) -> mir::BlockId {
        let id = mir::BlockId(self.blocks.len() as u32);
        self.blocks.push(mir::Block {
            stmts: vec![],
            branch: mir::Branch::Return(None),
        });
        id
    }
    fn push_stmt(&mut self, id: mir::BlockId, stmt: mir::Stmt<'s>) {
        self.blocks[id.0 as usize].stmts.push(stmt);
    }
    fn set_branch(&mut self, id: mir::BlockId, branch: mir::Branch<'s>) {
        self.blocks[id.0 as usize].branch = branch;
    }
    fn compile_block(&mut self, block: &ast::Block<'s>, block_id: &mut mir::BlockId) {
        for stmt in &block.stmts {
            match stmt {
                ast::Stmt::While { cond, body } => {
                    let mut loop_block = self.new_block();
                    let cond_block = self.new_block();
                    let exit_block = self.new_block();
                    self.set_branch(*block_id, mir::Branch::Static(cond_block));
                    let (cond_expr, cond_ty) = self.compile_expr(cond);
                    unify(&cond_ty, &TyRef::known(Ty::Bool)).unwrap();
                    self.set_branch(cond_block, mir::Branch::Condition {
                        expr: cond_expr,
                        if_true: loop_block,
                        if_false: exit_block,
                    });
                    self.compile_block(body, &mut loop_block);
                    self.set_branch(loop_block, mir::Branch::Static(cond_block));
                    *block_id = exit_block;
                }
                ast::Stmt::Let { ident, expr, ty: ast_ty } => {
                    let stack_slot = self.scope.len() as u32;
                    let ty = TyRef::any();
                    self.scope.push(Variable { name: *ident, stack_slot, ty: ty.clone() });
                    self.push_stmt(*block_id, mir::Stmt::Alloc(ty.clone()));

                    if let Some(ast_ty) = ast_ty {
                        let ast_ty = compile_ty(ast_ty, self.program);
                        unify(&ty, &ast_ty).unwrap();
                    }
                    
                    if let Some(expr) = expr {
                        let (expr, expr_ty) = self.compile_expr(expr);
                        unify(&ty, &expr_ty).unwrap();
                        self.push_stmt(*block_id, mir::Stmt::Assign {
                            assign: mir::Assign::Stack(stack_slot),
                            ty,
                            expr,
                        });
                    }
                }
                ast::Stmt::Assign { assign, expr } => {
                    let (expr, expr_ty) = self.compile_expr(expr);
                    let (assign, ty) = self.compile_assign(assign);
                    unify(&expr_ty, &ty).unwrap();
                    self.push_stmt(*block_id, mir::Stmt::Assign {
                        assign,
                        expr,
                        ty,
                    });
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
                    self.set_branch(*block_id, mir::Branch::Return(expr));
                    break
                }
                ast::Stmt::If(if_stmt) => self.compile_if(if_stmt, block_id),
                ast::Stmt::FnCall(fn_call) => {
                    let (args, ty) = self.compile_fn_call(fn_call);
                    if ty.is_some() {
                        panic!()
                    }
                    self.push_stmt(*block_id, mir::Stmt::FuncCall(mir::FuncCall {
                        name: fn_call.name,
                        args,
                    }))
                }
            }
        }
    }
    fn compile_assign(&mut self, assign: &ast::Assign<'s>) -> (mir::Assign, TyRef<'s>) {
        match assign {
            ast::Assign::Deref(assign) => {
                let (assign, ty) = self.compile_assign(assign);
                (mir::Assign::Deref(Box::new(assign)), deref_ty(&ty))
            }
            ast::Assign::Name(name) => {
                let var = self.lookup_var(*name);
                (mir::Assign::Stack(var.stack_slot), var.ty.clone())
            }
        }
    }
    fn compile_if(&mut self, if_stmt: &ast::If<'s>, block_id: &mut mir::BlockId) {
        let mut if_block = self.new_block();
        let mut else_block = self.new_block();
        self.compile_block(&if_stmt.if_block, &mut if_block);
        let (cond_expr, cond_ty) = self.compile_expr(&if_stmt.cond);
        unify(&cond_ty, &TyRef::known(Ty::Bool)).unwrap();
        self.set_branch(*block_id, mir::Branch::Condition {
            expr: cond_expr,
            if_true: if_block,
            if_false: else_block,
        });

        match &if_stmt.else_block {
            ast::Else::Block(else_ast_block) => {
                let exit_block_id = self.new_block();
                self.compile_block(else_ast_block, &mut else_block);
                self.set_branch(else_block, mir::Branch::Static(exit_block_id));
                self.set_branch(if_block, mir::Branch::Static(exit_block_id));
                *block_id = exit_block_id;
            }
            ast::Else::If(if_stmt) => {
                self.compile_if(if_stmt, &mut else_block);
                self.set_branch(if_block, mir::Branch::Static(else_block));
                *block_id = else_block;
            }
            ast::Else::None => {
                self.set_branch(if_block, mir::Branch::Static(else_block));
                *block_id = else_block;
            }
        }
    }
    fn lookup_var(&self, name: &str) -> &Variable<'s> {
        self.scope.iter().find(|var| var.name == name).unwrap()
    }
    fn compile_expr(&mut self, expr: &ast::Expr<'s>) -> (mir::Expr<'s>, TyRef<'s>) {
        match expr {
            ast::Expr::Integer(value) => {
                let int_ty = IntTyRef::any();
                let value = value.parse().unwrap();
                (mir::Expr::Int(value), TyRef::known(Ty::Int(int_ty)))
            }
            ast::Expr::Bool(value) =>  {
                (mir::Expr::Bool(*value), TyRef::known(Ty::Bool))
            }
            ast::Expr::Infix { left, right, op } => {
                match op {
                    ast::InfixOp::Add => self.compile_arth_expr(left, right, mir::BinaryOp::Add),
                    ast::InfixOp::Subtract => self.compile_arth_expr(left, right, mir::BinaryOp::Subtract),
                    ast::InfixOp::Multiply => self.compile_arth_expr(left, right, mir::BinaryOp::Multiply),
                    ast::InfixOp::Divide => self.compile_arth_expr(left, right, mir::BinaryOp::Divide),

                    ast::InfixOp::LessThan => self.compile_cmp_expr(left, right, mir::BinaryOp::LessThan),
                    ast::InfixOp::GreaterThan => self.compile_cmp_expr(left, right, mir::BinaryOp::GreaterThan),
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
                    let ty = deref_ty(&ty);
                    (mir::Expr::Deref { expr: Box::new(expr), ty: ty.clone() }, ty)
                }
                ast::PrefixOp::Ref => match &**expr {
                    ast::Expr::Ident(name) => {
                        let var = self.lookup_var(*name);
                        (mir::Expr::Ref(var.stack_slot), TyRef::known(Ty::Ref(var.ty.clone())))
                    }
                    _ => panic!(),
                }
            }
            ast::Expr::FnCall(fn_call) => {
                let (args, ty) = self.compile_fn_call(fn_call);
                let result = ty.unwrap();
                (mir::Expr::FnCall { fn_call: mir::FuncCall {
                    name: fn_call.name,
                    args,
                }, result: result.clone() }, result)
            }
            ast::Expr::InitStruct { name, values } => {
                let structure = self.program.structs.get(name).unwrap();
                let mut done = HashSet::new();
                let mut mir_values = vec![];
                let mut tys = vec![];
                for field in structure.fields.iter() {
                    if done.contains(field.name) {
                        panic!()
                    }
                    done.insert(field.name);
                    let value = values.iter().find(|value| value.name == field.name).unwrap();
                    let (expr, ty) = self.compile_expr(&value.expr);
                    let field_ty = compile_ty(&field.ty, self.program);
                    unify(&ty, &field_ty).unwrap();
                    tys.push(field_ty);
                    mir_values.push(mir::StructValue { ty, expr });
                }
                (mir::Expr::InitStruct(mir_values), TyRef::known(Ty::Struct { name, tys }))
            }
            ast::Expr::Property { expr, name } => todo!(),
        }
    }
    fn compile_fn_call(&mut self, fn_call: &ast::FnCall<'s>) -> (Vec<mir::Arg<'s>>, Option<TyRef<'s>>) {
        let func = self.program.funcs.get(fn_call.name).unwrap();
        if fn_call.args.len() != func.params.len() {
            panic!()
        }
        let args = fn_call.args.iter().zip(&func.params).map(|(arg, param)| {
            let (expr, ty) = self.compile_expr(arg);
            let param_ty = compile_ty(&param.ty, self.program);
            unify(&ty, &param_ty).unwrap();
            mir::Arg { expr, ty }
        }).collect();
        (args, func.returns.as_ref().map(|ty| compile_ty(ty, self.program)))
    }
    fn compile_arth_expr(&mut self, left: &ast::Expr<'s>, right: &ast::Expr<'s>, op: mir::BinaryOp) -> (mir::Expr<'s>, TyRef<'s>) {
        let (left_expr, left_ty) = self.compile_expr(left);
        let (right_expr, right_ty) = self.compile_expr(right);
        let int_ty = IntTyRef::any();
        let ty = TyRef::known(Ty::Int(int_ty.clone()));
        unify(&ty, &left_ty).unwrap();
        unify(&ty, &right_ty).unwrap();
        (mir::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            ty: int_ty,
            op,
        }, ty)
    }
    fn compile_cmp_expr(&mut self, left: &ast::Expr<'s>, right: &ast::Expr<'s>, op: mir::BinaryOp) -> (mir::Expr<'s>, TyRef<'s>) {
        let (left_expr, left_ty) = self.compile_expr(left);
        let (right_expr, right_ty) = self.compile_expr(right);
        let int_ty = IntTyRef::any();
        let ty = TyRef::known(Ty::Int(int_ty.clone()));
        let bool_ty = TyRef::known(Ty::Bool);
        unify(&ty, &left_ty).unwrap();
        unify(&ty, &right_ty).unwrap();
        (mir::Expr::Binary {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            ty: int_ty,
            op,
        }, bool_ty)
    }
}