use crate::{mir, lir};

pub fn lower_func(func: &mir::Func) -> lir::Func {
    let blocks = func.blocks.iter().map(|block| lower_block(block)).collect();
    let params = func.params.iter().map(|ty| ty.concrete()).collect();
    lir::Func { blocks, name: func.name, params }
}

fn lower_block(block: &mir::Block) -> lir::Block {
    let stmts = block.stmts.iter().map(|stmt| lower_stmt(stmt)).collect();
    let branch = match &block.branch {
        mir::Branch::Return(expr) => lir::Branch::Return(expr.as_ref().map(|expr| lower_expr(expr))),
        mir::Branch::Static(block) => lir::Branch::Static(*block),
        mir::Branch::Condition { expr, if_true, if_false } => lir::Branch::Condition {
            expr: lower_expr(&expr),
            if_true: *if_true,
            if_false: *if_false,
        },
    };
    lir::Block { stmts, branch }
}

fn lower_stmt(stmt: &mir::Stmt) -> lir::Stmt {
    match stmt {
        mir::Stmt::Alloc(ty) => lir::Stmt::Alloc(ty.concrete()),
        mir::Stmt::Assign { assign, expr, ty } => {
            lir::Stmt::Assign { assign: lower_assign(assign), expr: lower_expr(expr), ty: ty.concrete() }
        }
        mir::Stmt::FuncCall(func_call) => {
            let args = func_call.args.iter().map(|expr| lower_expr(expr)).collect();
            lir::Stmt::FuncCall(lir::FuncCall { name: func_call.name, args })
        }
    }
}

fn lower_assign(assign: &mir::Assign) -> lir::Assign {
    match assign {
        mir::Assign::Deref(assign) => lir::Assign::Deref(Box::new(lower_assign(assign))),
        mir::Assign::Variable(var) => lir::Assign::Variable(*var),
    }
}

fn lower_expr(expr: &mir::Expr) -> lir::Expr {
    match expr {
        mir::Expr::Int(value) => lir::Expr::Int(*value),
        mir::Expr::Bool(value) => lir::Expr::Bool(*value),
        mir::Expr::Binary { left, right, ty, op } => lir::Expr::Binary {
            left: Box::new(lower_expr(left)),
            right: Box::new(lower_expr(right)),
            ty: ty.concrete(),
            op: *op,
        },
        mir::Expr::Load { var, ty } => lir::Expr::Load { var: *var, ty: ty.concrete() },
        mir::Expr::Ref(var) => lir::Expr::Ref(*var),
        mir::Expr::Deref { expr, ty } => lir::Expr::Deref {
            expr: Box::new(lower_expr(expr)),
            ty: ty.concrete()
        },
        mir::Expr::FuncCall(func_call) => lir::Expr::FuncCall(lower_func_call(func_call)),
        mir::Expr::InitStruct(values) => {
            let values = values.iter().map(|value| lir::StructValue {
                expr: lower_expr(&value.expr),
                ty: value.ty.concrete(),
            }).collect();
            lir::Expr::InitStruct(values)
        }
    }
}

fn lower_func_call(func_call: &mir::FuncCall) -> lir::FuncCall {
    let args = func_call.args.iter().map(|expr| lower_expr(expr)).collect();
    lir::FuncCall { name: func_call.name, args }
}