use crate::infer::{InferTyRef, Unify, unify};

pub type TyRef = InferTyRef<Ty>;
pub type IntTyRef = InferTyRef<IntTy>;

#[derive(Debug, Clone)]
pub enum Ty {
    Bool,
    None,
    Ref,
    Int(IntTyRef),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntTy {
    pub signedness: Signedness,
    pub size: Size,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Signedness {
    Signed, Unsigned
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    B8, B16, B32
}

impl Unify for Ty {
    fn unify(a: &Ty, b: &Ty) -> Result<(), ()> {
        match (a, b) {
            (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::None, Ty::None) => Ok(()),
            (Ty::Ref, Ty::Ref) => Ok(()),
            (Ty::Int(a), Ty::Int(b)) => unify(&a, &b).map(|_| ()),
            _ => Err(())
        }
    }
}

impl Unify for IntTy {
    fn unify(_: &IntTy, _: &IntTy) -> Result<(), ()> {
        Ok(())
    }
}