#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int {
    pub signedness: Signedness,
    pub size: Size,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    B8,
    B16,
    B32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AtomicTy {
    Int(Int),
    None,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Atomic(AtomicTy)
}