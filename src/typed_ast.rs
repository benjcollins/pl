use crate::{ty::{TyRef, IntTyRef, StructTyRef}, symbols::Symbol};

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Symbol,
    pub params: Vec<TyRef>,
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Variable(pub u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub enum Branch {
    Return(Option<Expr>),
    Static(BlockId),
    Condition {
        expr: Expr,
        if_true: BlockId,
        if_false: BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc(TyRef),
    DerefAssign {
        assign: Expr,
        expr: Expr,
        ty: TyRef,
    },
    Assign {
        ref_expr: RefExpr,
        expr: Expr,
        ty: TyRef,
    },
    FuncCall(FuncCall),
}

#[derive(Debug, Clone)]
pub enum RefExpr {
    Variable(Variable),
    Field {
        ref_expr: Box<RefExpr>,
        name: Symbol,
        ty: StructTyRef,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        ty: IntTyRef,
        op: BinaryOp,
    },
    Bool(bool),
    Load {
        var: Variable,
        ty: TyRef,
    },
    Ref(RefExpr),
    Deref {
        expr: Box<Expr>,
        ty: TyRef,
    },
    FuncCall(FuncCall),
    InitStruct(Vec<StructValue>),
    Field {
        expr: Box<Expr>,
        ty: StructTyRef,
        name: Symbol,
    }
}

#[derive(Debug, Clone)]
pub struct StructValue {
    pub expr: Expr,
    pub ty: TyRef,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name: Symbol,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

// impl BlockId {
//     pub fn id(&self) -> u32 {
//         self.0
//     }
// }

// impl fmt::Display for Func {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         writeln!(f, "fn {}()", self.name)?;
//         if let Some(ty) = &self.returns {
//             write!(f, " {}", TyOption(ty.concrete()))?;
//         }
//         writeln!(f, "")?;
//         for (id, block) in self.blocks.iter().enumerate() {
//             writeln!(f, "b{}:", id)?;
//             write!(f, "{}", block)?;
//         }
//         Ok(())
//     }
// }

// impl fmt::Display for Block {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         for stmt in &self.stmts {
//             writeln!(f, "  {}", stmt)?;
//         }
//         Ok(())
//     }
// }

// impl fmt::Display for Stmt {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Stmt::Alloc(ty) => write!(f, "alloc({})", TyOption(ty.concrete())),
//             Stmt::Assign { assign, expr, .. } => write!(f, "{} = {}", assign, expr),
//             Stmt::FuncCall { .. } => write!(f, "call"),
//         }
//     }
// }

// impl fmt::Display for Assign {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Assign::Deref(assign) => write!(f, "*{}", assign),
//             Assign::Stack(stack_slot) => write!(f, "${}", stack_slot),
//         }
//     }
// }

// impl fmt::Display for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Expr::Int(value) => write!(f, "{}", value),
//             Expr::Binary { left, right, op, .. } => write!(f, "({} {} {})", left, op, right),
//             Expr::Bool(value) => write!(f, "{}", if *value { "true" } else { "false" }),
//             Expr::Load { stack_slot, ty } => write!(f, "({}) ${}", TyOption(ty.concrete()), stack_slot),
//             Expr::Ref(stack_slot) => write!(f, "&${}", stack_slot),
//             Expr::Deref { expr, ty } => write!(f, "*({}) {}", TyOption(ty.concrete()), expr),
//             Expr::FnCall { .. } => write!(f, "call"),
//             Expr::InitStruct { .. } => todo!(),
//         }
//     }
// }

// impl fmt::Display for BinaryOp {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", match self {
//             BinaryOp::Add => "+",
//             BinaryOp::Subtract => "-",
//             BinaryOp::Multiply => "*",
//             BinaryOp::Divide => "/",
//             BinaryOp::LessThan => "<",
//             BinaryOp::GreaterThan => ">",
//         })
//     }
// }