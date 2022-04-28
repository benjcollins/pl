use std::{collections::HashMap, fmt};

use crate::{infer::{InferTyRef, Unify, unify}, symbols::Symbol};

pub type TyRef = InferTyRef<Ty>;
pub type IntTyRef = InferTyRef<IntTy>;
pub type StructTyRef = InferTyRef<StructTy>;

#[derive(Debug, Clone)]
pub enum Ty {
    Bool,
    Ref(TyRef),
    Int(IntTyRef),
    Struct(StructTyRef),
    Any,
}

#[derive(Debug, Clone)]
pub enum IntTy {
    Int(Int),
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int {
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

#[derive(Debug, Clone)]
pub enum StructTy {
    Known {
        name: Symbol,
        fields: Vec<Field>,
    },
    WithFields(HashMap<Symbol, TyRef>),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Symbol,
    pub ty: TyRef,
}

impl Unify for Ty {
    fn unify(a: Ty, b: Ty) -> Result<Ty, ()> {
        match (a, b) {
            (Ty::Any, Ty::Any) => Ok(Ty::Any),
            (Ty::Any, ty) | (ty,  Ty::Any) => Ok(ty),
            (Ty::Bool, Ty::Bool) => Ok(Ty::Bool),
            (Ty::Ref(a), Ty::Ref(b)) => Ok(Ty::Ref(unify(&a, &b)?)),
            (Ty::Int(a), Ty::Int(b)) => Ok(Ty::Int(unify(&a, &b)?)),
            (Ty::Struct(a), Ty::Struct(b)) => Ok(Ty::Struct(unify(&a, &b)?)),
            (a, b) => {
                println!("{:?}\n{:?}", a, b);
                Err(())?
            }
        }
    }
}

impl Unify for IntTy {
    fn unify(a: Self, b: Self) -> Result<Self, ()> {
        Ok(match (a, b) {
            (IntTy::Any, IntTy::Any) => IntTy::Any,
            (ty, IntTy::Any) | (IntTy::Any, ty) => ty,
            (IntTy::Int(a), IntTy::Int(b)) => if a == b {
                IntTy::Int(a)
            } else {
                Err(())?
            } 
        })
    }
}

impl Unify for StructTy {
    fn unify(a: Self, b: Self) -> Result<Self, ()> {
        Ok(match (a, b) {
            (StructTy::Known { name: a, fields }, StructTy::Known { name: b, .. }) => if a == b {
                StructTy::Known { name: a, fields }
            } else {
                Err(())?
            }
            (StructTy::Known { name, fields }, StructTy::WithFields(required_fields)) |
            (StructTy::WithFields(required_fields), StructTy::Known { name, fields }) => {
                for (name, ty) in required_fields {
                    let field = fields.iter().find(|field| field.name == name).unwrap();
                    unify(&field.ty, &ty)?;
                }
                StructTy::Known { name, fields }
            }
            (StructTy::WithFields(mut a), StructTy::WithFields(b)) => {
                for (name, b_ty) in b {
                    match a.get(&name) {
                        Some(a_ty) => _ = unify(a_ty, &b_ty)?,
                        None => _ = a.insert(name, b_ty),
                    }
                }
                StructTy::WithFields(a)
            }
        })
    }
}

impl fmt::Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntTy::Int(int) => {
                let signedness = match int.signedness {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                };
                let size = match int.size {
                    Size::B8 => "8",
                    Size::B16 => "16",
                    Size::B32 => "32",
                };
                write!(f, "{}{}", signedness, size)
            }
            IntTy::Any => write!(f, "int?"),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Bool => write!(f, "bool"),
            Ty::Ref(ty) => write!(f, "&{}", ty),
            Ty::Int(int_ty) => write!(f, "{}", int_ty),
            Ty::Struct(s) => write!(f, "{}", s),
            Ty::Any => write!(f, "any?"),
        }
    }
}

impl fmt::Display for StructTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructTy::Known { name, .. } => write!(f, "{}", name.0),
            StructTy::WithFields(_) => write!(f, "struct?"),
        }
    }
}