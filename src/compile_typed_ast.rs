use crate::{typed_ast, ir, ty};

struct Compiler {
    scope: Vec<Variable>,
}

struct Variable {
    ty: ir::Ty,
    region: ir::Region,
}

fn concrete_int(ty: &ty::IntTyRef) -> ty::Int {
    ty.map(|ty| match ty {
        ty::IntTy::Int(int) => *int,
        ty::IntTy::Any => ty::Int { signedness: ty::Signedness::Signed, size: ty::Size::B32 },
    })
}

impl Compiler {
    fn compile_expr(&self, expr: &typed_ast::Expr) -> (ir::Expr, ir::Ty) {
        match expr {
            typed_ast::Expr::Int { value, ty } => (ir::Expr::Int(*value), ir::Ty::Int(concrete_int(ty))),
            typed_ast::Expr::Bool(value) => (ir::Expr::Bool(*value), ir::Ty::Bool),

            typed_ast::Expr::Binary { left, right, ty, op } => {
                let (left_expr, _) = self.compile_expr(left);
                let (right_expr, _) = self.compile_expr(right);
                let ty = concrete_int(ty);
                let expr = ir::Expr::Binary {
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                    op: *op,
                    ty,
                };
                (expr, ir::Ty::Int(ty))
            }

            typed_ast::Expr::Load(var_id) => {
                let var = &self.scope[var_id.0 as usize];
                let expr = ir::Expr::Load { var: *var_id, ty: var.ty.clone() };
                (expr, var.ty.clone())
            }

            typed_ast::Expr::Ref(ref_expr) => {

            }
            typed_ast::Expr::Deref { expr, ty } => todo!(),
            typed_ast::Expr::FuncCall(_) => todo!(),
            typed_ast::Expr::InitStruct(_) => todo!(),
            typed_ast::Expr::Field { expr, ty, name } => todo!(),
        }
    }
    fn compile_ref_expr(&self, ref_expr: typed_ast::RefExpr) -> (ir::RefExpr, ir::Region) {
        match ref_expr {
            typed_ast::RefExpr::Variable(var_id) => {
                let var = &self.scope[var_id.0 as usize];
                (ir::RefExpr::Variable(var_id), var.region)
            }
            typed_ast::RefExpr::Field { ref_expr, name, ty } => {
                
            }
        }
    }
}