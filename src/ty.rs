use crate::infer::InferTyRef;

pub type TyRef = InferTyRef<Ty>;
pub type IntTyRef = InferTyRef<IntTy>;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Bool,
    None,
    Ref,
    Int(IntTyRef),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntTy {
    signedness: Signedness,
    size: Size,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Signedness {
    Signed, Unsigned
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    B8, B16, B32
}