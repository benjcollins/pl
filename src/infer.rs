use std::{rc::Rc, cell::RefCell};

#[derive(Debug, Clone)]
pub struct InferTyRef<T: Unify + Clone>(Rc<RefCell<InferTy<T>>>);

#[derive(Debug, Clone)]
pub enum InferTy<T: Unify + Clone> {
    Equal(InferTyRef<T>),
    Known(T),
}

pub trait Unify where Self: Sized {
    type Concrete;

    fn unify(a: Self, b: Self) -> Result<Self, ()>;
    fn concrete(&self) -> Self::Concrete;
}

impl<T: Unify<Concrete = C> + Clone, C> InferTyRef<T> {
    pub fn new(ty: T) -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Known(ty))))
    }
    pub fn concrete(&self) -> C {
        match &*self.0.borrow() {
            InferTy::Equal(r) => r.concrete(),
            InferTy::Known(ty) => ty.concrete(),
        }
    }
}

// pub fn unify<T: Unify + Clone>(a: &InferTyRef<T>, b: &InferTyRef<T>) -> Result<InferTyRef<T>, ()> {
//     let a_ty = std::mem::replace(&mut *a.0.borrow_mut(), InferTy::Empty);
//     let b_ty = std::mem::replace(&mut *b.0.borrow_mut(), InferTy::Empty);

//     let ty = match (a_ty, b_ty) {
//         (InferTy::Equal(a), b_ty) => {
//             *b.0.borrow_mut() = b_ty;
//             unify(&a, b)?
//         },
//         (a_ty, InferTy::Equal(b)) => {
//             *a.0.borrow_mut() = a_ty;
//             unify(a, &b)?
//         },
        
//         (InferTy::Known(a), InferTy::Known(b)) => {
//             InferTyRef::new(T::unify(a, b)?)
//         }

//         _ => unreachable!(),
//     };

//     *a.0.borrow_mut() = InferTy::Equal(ty.clone());
//     *b.0.borrow_mut() = InferTy::Equal(ty.clone());
//     Ok(ty)
// }

pub fn unify<T: Unify + Clone>(a: &InferTyRef<T>, b: &InferTyRef<T>) -> Result<InferTyRef<T>, ()> {
    if Rc::ptr_eq(&a.0, &b.0) {
        return Ok(a.clone())
    }

    let mut a_ref = a.0.borrow_mut();
    let mut b_ref = b.0.borrow_mut();

    match (&mut *a_ref, &mut *b_ref) {
        (InferTy::Equal(a), _) => {
            drop(b_ref);
            let unified = unify(a, b)?;
            *a = unified.clone();
            Ok(unified)
        }
        (_, InferTy::Equal(b)) => {
            drop(a_ref);
            let unified = unify(a, b)?;
            *b = unified.clone();
            Ok(unified)
        }
        (InferTy::Known(a), InferTy::Known(b)) => {
            let unified = InferTyRef::new(T::unify(a.clone(), b.clone())?);
            *a_ref = InferTy::Equal(unified.clone());
            *b_ref = InferTy::Equal(unified.clone());
            Ok(unified)
        }
    }
}