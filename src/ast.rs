#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub start: usize,
    pub end: usize,
}

impl Ident {
    pub fn as_str<'src>(&self, src: &'src str) -> &'src str {
        &src[self.start..self.end]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub if_block: Block,
    pub else_block: Else,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Else {
    Block(Block),
    If(Box<If>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer {
        start: usize,
        end: usize,
    },
    Bool(bool),
    Ident(Ident),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { ident: Ident, expr: Option<Expr>, ty: Option<Ty> },
    Assign { ident: Ident, expr: Expr },
    Return { expr: Expr },
    While {
        cond: Expr,
        body: Block,
    },
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub params: Vec<(Ident, Ty)>,
    pub returns: Option<Ty>,
    pub block: Block,
    pub name: Ident,
}