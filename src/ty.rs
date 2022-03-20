use std::fmt;

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

impl fmt::Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let signedness = match self.signedness {
            Signedness::Signed => "i",
            Signedness::Unsigned => "u",
        };
        let size = match self.size {
            Size::B8 => "8",
            Size::B16 => "16",
            Size::B32 => "32",
        };
        write!(f, "{}{}", signedness, size)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Bool => write!(f, "bool"),
            Ty::None => write!(f, "none"),
            Ty::Ref => write!(f, "ref"),
            Ty::Int(int_ty) => write!(f, "{}", int_ty),
        }
    }
}