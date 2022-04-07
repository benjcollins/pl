use crate::{typed_ast, ir};

pub fn lower_func(func: &typed_ast::Func) -> ir::Func {
    let blocks = func.blocks.iter().map(|block| lower_block(block)).collect();
    let params = func.params.iter().map(|ty| ty.concrete()).collect();
    ir::Func { blocks, name: func.name, params }
}

fn lower_block(block: &typed_ast::Block) -> ir::Block {
    let stmts = block.stmts.iter().map(|stmt| lower_stmt(stmt)).collect();
    let branch = match &block.branch {
        typed_ast::Branch::Return(expr) => ir::Branch::Return(expr.as_ref().map(|expr| lower_expr(expr))),
        typed_ast::Branch::Static(block) => ir::Branch::Static(*block),
        typed_ast::Branch::Condition { expr, if_true, if_false } => ir::Branch::Condition {
            expr: lower_expr(&expr),
            if_true: *if_true,
            if_false: *if_false,
        },
    };
    ir::Block { stmts, branch }
}

fn lower_stmt(stmt: &typed_ast::Stmt) -> ir::Stmt {
    match stmt {
        typed_ast::Stmt::Alloc(ty) => ir::Stmt::Alloc(ty.concrete()),
        typed_ast::Stmt::DerefAssign { assign, expr, ty } => {
            ir::Stmt::DerefAssign { assign: lower_expr(assign), expr: lower_expr(expr), ty: ty.concrete() }
        }
        typed_ast::Stmt::Assign { ref_expr, expr, ty } => {
            ir::Stmt::Assign { ref_expr: lower_ref_expr(ref_expr), ty: ty.concrete(), expr: lower_expr(expr) }
        }
        typed_ast::Stmt::FuncCall(func_call) => {
            let args = func_call.args.iter().map(|expr| lower_expr(expr)).collect();
            ir::Stmt::FuncCall(ir::FuncCall { name: func_call.name, args })
        }
    }
}

fn lower_ref_expr(ref_expr: &typed_ast::RefExpr) -> ir::RefExpr {
    match ref_expr {
        typed_ast::RefExpr::Variable(var) => ir::RefExpr::Variable(*var),
    }
}

fn lower_expr(expr: &typed_ast::Expr) -> ir::Expr {
    match expr {
        typed_ast::Expr::Int(value) => ir::Expr::Int(*value),
        typed_ast::Expr::Bool(value) => ir::Expr::Bool(*value),
        typed_ast::Expr::Binary { left, right, ty, op } => ir::Expr::Binary {
            left: Box::new(lower_expr(left)),
            right: Box::new(lower_expr(right)),
            ty: ty.concrete(),
            op: *op,
        },
        typed_ast::Expr::Load { var, ty } => ir::Expr::Load { var: *var, ty: ty.concrete() },
        typed_ast::Expr::Ref(ref_expr) => ir::Expr::Ref(lower_ref_expr(ref_expr)),
        typed_ast::Expr::Deref { expr, ty } => ir::Expr::Deref {
            expr: Box::new(lower_expr(expr)),
            ty: ty.concrete()
        },
        typed_ast::Expr::FuncCall(func_call) => ir::Expr::FuncCall(lower_func_call(func_call)),
        typed_ast::Expr::InitStruct(values) => {
            let values = values.iter().map(|value| ir::StructValue {
                expr: lower_expr(&value.expr),
                ty: value.ty.concrete(),
            }).collect();
            ir::Expr::InitStruct(values)
        }
        typed_ast::Expr::Field { expr, name, ty } => {
            let expr = Box::new(lower_expr(expr));
            let fields = ty.concrete();
            ir::Expr::Field { expr, fields, name: *name }
        }
    }
}

fn lower_func_call(func_call: &typed_ast::FuncCall) -> ir::FuncCall {
    let args = func_call.args.iter().map(|expr| lower_expr(expr)).collect();
    ir::FuncCall { name: func_call.name, args }
}