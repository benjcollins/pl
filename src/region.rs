use crate::ir;

struct State {}

impl State {
    fn analyse_func(&self, func: &ir::Func) {}
    fn analyse_block(&self, block: &ir::Block) {}
    fn analyse_stmt(&self, stmt: &ir::Stmt) {}
    fn analyse_expr(&self, expr: &ir::Expr) {}
}
