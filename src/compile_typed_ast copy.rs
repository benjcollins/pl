use crate::{typed_ast, ir, ty};

pub fn compile_func(func: &typed_ast::Func) -> ir::Func {
    let blocks = func.blocks.iter().map(|block| compile_block(block)).collect();
    let params = func.params.iter().map(|ty| concrete_ty(ty)).collect();
    ir::Func { blocks, name: func.name, params }
}

fn concrete_ty(ty: &ty::TyRef) -> ir::Ty {
    ty.map(|ty| match ty {
        ty::Ty::Bool => ir::Ty::Bool,
        ty::Ty::Ref(_) => ir::Ty::Ptr,
        ty::Ty::Int(int) => ir::Ty::Int(concrete_int(int)),
        ty::Ty::Struct(s) => ir::Ty::Struct(concrete_struct(s)),
        ty::Ty::Any => panic!(),
    })
}

fn concrete_int(ty: &ty::IntTyRef) -> ty::Int {
    ty.map(|ty| match ty {
        ty::IntTy::Int(int) => *int,
        ty::IntTy::Any => ty::Int { signedness: ty::Signedness::Signed, size: ty::Size::B32 },
    })
}

fn concrete_struct(ty: &ty::StructTyRef) -> Vec<ir::StructField> {
    ty.map(|ty| match ty {
        ty::StructTy::Known { fields, .. } => {
            fields.iter().map(|field| ir::StructField { name: field.name, ty: concrete_ty(&field.ty) }).collect()
        }
        ty::StructTy::WithFields(_) => panic!(),
    })
}

fn compile_block(block: &typed_ast::Block) -> ir::Block {
    let stmts = block.stmts.iter().map(|stmt| compile_stmt(stmt)).collect();
    let branch = match &block.branch {
        typed_ast::Branch::Return(expr) => ir::Branch::Return(expr.as_ref().map(|expr| compile_expr(expr))),
        typed_ast::Branch::Static(block) => ir::Branch::Static(*block),
        typed_ast::Branch::Condition { expr, if_true, if_false } => ir::Branch::Condition {
            expr: compile_expr(&expr),
            if_true: *if_true,
            if_false: *if_false,
        },
    };
    ir::Block { stmts, branch }
}

fn compile_stmt(stmt: &typed_ast::Stmt) -> ir::Stmt {
    match stmt {
        typed_ast::Stmt::Alloc(ty) => ir::Stmt::Alloc(concrete_ty(ty)),
        typed_ast::Stmt::DerefAssign { assign, expr, ty } => {
            ir::Stmt::DerefAssign { assign: compile_expr(assign), expr: compile_expr(expr), ty: concrete_ty(ty) }
        }
        typed_ast::Stmt::Assign { ref_expr, expr, ty } => {
            ir::Stmt::Assign { ref_expr: compile_ref_expr(ref_expr), ty: concrete_ty(ty), expr: compile_expr(expr) }
        }
        typed_ast::Stmt::FuncCall(func_call) => {
            let args = func_call.args.iter().map(|expr| compile_expr(expr)).collect();
            ir::Stmt::FuncCall(ir::FuncCall { name: func_call.name, args })
        }
    }
}

fn compile_ref_expr(ref_expr: &typed_ast::RefExpr) -> ir::RefExpr {
    match ref_expr {
        typed_ast::RefExpr::Variable(var) => ir::RefExpr::Variable(*var),
        typed_ast::RefExpr::Field { ref_expr, name, ty } => {
            let ref_expr = Box::new(compile_ref_expr(ref_expr));
            let fields = concrete_struct(ty);
            ir::RefExpr::Field { ref_expr, fields, name: *name }
        }
    }
}

fn compile_expr(expr: &typed_ast::Expr) -> ir::Expr {
    match expr {
        typed_ast::Expr::Int { value, .. } => ir::Expr::Int(*value),
        typed_ast::Expr::Bool(value) => ir::Expr::Bool(*value),
        typed_ast::Expr::Binary { left, right, ty, op } => ir::Expr::Binary {
            left: Box::new(compile_expr(left)),
            right: Box::new(compile_expr(right)),
            ty: concrete_int(ty),
            op: *op,
        },
        typed_ast::Expr::Load { var, ty } => ir::Expr::Load { var: *var, ty: concrete_ty(ty) },
        typed_ast::Expr::Ref(ref_expr) => ir::Expr::Ref(compile_ref_expr(ref_expr)),
        typed_ast::Expr::Deref { expr, ty } => ir::Expr::Deref {
            expr: Box::new(compile_expr(expr)),
            ty: concrete_ty(ty),
        },
        typed_ast::Expr::FuncCall(func_call) => ir::Expr::FuncCall(compile_func_call(func_call)),
        typed_ast::Expr::InitStruct(values) => {
            let values = values.iter().map(|value| ir::StructValue {
                expr: compile_expr(&value.expr),
                ty: concrete_ty(&value.ty),
            }).collect();
            ir::Expr::InitStruct(values)
        }
        typed_ast::Expr::Field { expr, name, ty } => {
            let expr = Box::new(compile_expr(expr));
            let fields = concrete_struct(ty);
            ir::Expr::Field { expr, fields, name: *name }
        }
    }
}

fn compile_func_call(func_call: &typed_ast::FuncCall) -> ir::FuncCall {
    let args = func_call.args.iter().map(|expr| compile_expr(expr)).collect();
    ir::FuncCall { name: func_call.name, args }
}