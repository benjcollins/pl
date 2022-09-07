use std::{cell::RefCell, fmt, rc::Rc};

#[derive(Debug)]
pub struct InferTyRef<T: Unify>(Rc<RefCell<InferTy<T>>>);

#[derive(Debug)]
pub enum InferTy<T: Unify> {
    Equal(InferTyRef<T>),
    Known(Option<T>),
}

impl<T: Unify> Clone for InferTyRef<T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

pub trait Unify
where
    Self: Sized,
{
    fn unify(a: Self, b: Self) -> Result<Self, ()>;
}

impl<T: Unify> InferTyRef<T> {
    pub fn new(ty: T) -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Known(Some(ty)))))
    }
    pub fn map<T1>(&self, f: impl Fn(&T) -> T1) -> T1 {
        match &*self.0.borrow() {
            InferTy::Equal(r) => r.map(f),
            InferTy::Known(ty) => f(ty.as_ref().unwrap()),
        }
    }
}

pub fn unify<T: Unify>(a: &InferTyRef<T>, b: &InferTyRef<T>) -> Result<InferTyRef<T>, ()> {
    if Rc::ptr_eq(&a.0, &b.0) {
        return Ok(a.clone());
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
            let unified = InferTyRef::new(T::unify(a.take().unwrap(), b.take().unwrap())?);
            *a_ref = InferTy::Equal(unified.clone());
            *b_ref = InferTy::Equal(unified.clone());
            Ok(unified)
        }
    }
}

impl<T: fmt::Display + Unify> fmt::Display for InferTyRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0.borrow() {
            InferTy::Equal(ty) => write!(f, "{}", ty),
            InferTy::Known(ty) => write!(f, "{}", ty.as_ref().unwrap()),
        }
    }
}
