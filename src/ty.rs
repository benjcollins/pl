use std::{rc::Rc, cell::RefCell};

pub type TyRef = Rc<RefCell<Ty>>;
pub type IntTyRef = Rc<RefCell<IntTy>>;

#[derive(Debug, Clone)]
pub enum Ty {
    Any,
    Equal(TyRef),
    Bool,
    Ref(TyRef),
    Int(IntTyRef),
    None,
}

#[derive(Debug, Clone)]
pub enum IntTy {
    Any,
    Equal(IntTyRef),
    Int(Int),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int {
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

impl Ty {
    pub fn as_ref(self) -> TyRef {
        Rc::new(RefCell::new(self))
    }
}

impl IntTy {
    pub fn as_ref(self) -> IntTyRef {
        Rc::new(RefCell::new(self))
    }
}

pub fn unify(a: &TyRef, b: &TyRef) -> Ty {
    let unified = match (&*a.borrow(), &*b.borrow()) {
        (Ty::Equal(a), _) => unify(&a, b),
        (_, Ty::Equal(b)) => unify(a, &b),

        (Ty::Any, Ty::Any) => Ty::Equal(Ty::Any.as_ref()),
        (Ty::Any, ty) => ty.clone(),
        (ty, Ty::Any) => ty.clone(),

        (Ty::Bool, Ty::Bool) => Ty::Bool,
        (Ty::None, Ty::None) => Ty::None,

        (Ty::Int(a), Ty::Int(b)) => Ty::Int(unify_ints(a, b).as_ref()),
        (Ty::Ref(inner_a), Ty::Ref(inner_b)) => Ty::Ref(unify(a, b).as_ref()),

        _ => panic!(),
    };
    *a.borrow_mut() = unified.clone();
    *b.borrow_mut() = unified.clone();
    unified
}

pub fn unify_ints(a: &IntTyRef, b: &IntTyRef) -> IntTy {
    let unified = match (&*a.borrow(), &*b.borrow()) {
        (IntTy::Equal(a), _) => unify_ints(&a, b),
        (_, IntTy::Equal(b)) => unify_ints(a, &b),

        (IntTy::Any, IntTy::Any) => IntTy::Equal(IntTy::Any.as_ref()),
        (IntTy::Any, ty) => ty.clone(),
        (ty, IntTy::Any) => ty.clone(),

        (IntTy::Int(a), IntTy::Int(b)) => IntTy::Int(*a),
    };
    *a.borrow_mut() = unified.clone();
    *b.borrow_mut() = unified.clone();
    unified
}