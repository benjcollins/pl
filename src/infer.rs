use std::{rc::Rc, cell::{RefCell, Ref}};

#[derive(Debug, Clone)]
pub struct InferTyRef<T: Unify + Clone>(Rc<RefCell<InferTy<T>>>);

#[derive(Debug, Clone)]
pub enum InferTy<T: Unify + Clone> {
    Any,
    Equal(InferTyRef<T>),
    Known(T),
}

pub trait Unify {
    fn unify(a: &Self, b: &Self) -> Result<(), ()>;
}

impl<T: Unify + Clone> InferTyRef<T> {
    pub fn any() -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Any)))
    }
    pub fn known(ty: T) -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Known(ty))))
    }
    pub fn infer_ty(&self) -> Ref<'_, InferTy<T>> {
        self.0.borrow()
    }
    pub fn concrete(&self) -> Option<T> {
        match &*self.0.borrow() {
            InferTy::Any => None,
            InferTy::Equal(ty) => ty.concrete(),
            InferTy::Known(ty) => Some(ty.clone()),
        }
    }
}

pub fn unify<T: Unify + Clone>(a: &InferTyRef<T>, b: &InferTyRef<T>) -> Result<InferTy<T>, ()> {
    // if Rc::ptr_eq(&a.0, &b.0) {
    //     return Ok(a.0.borrow().clone())
    // };
    let unified = {
        let a_ref = a.0.borrow();
        let b_ref = b.0.borrow();
        match (&*a_ref, &*b_ref) {
            (InferTy::Equal(a), _) => {
                let a = a.clone();
                drop(a_ref);
                drop(b_ref);
                unify(&a, b)?
            }
            (_, InferTy::Equal(b)) => {
                let b = b.clone();
                drop(a_ref);
                drop(b_ref);
                unify(a, &b)?
            }
            
            (InferTy::Any, InferTy::Any) => InferTy::Equal(InferTyRef::any()),
            (InferTy::Any, ty) | (ty, InferTy::Any) => ty.clone(),

            (InferTy::Known(a), InferTy::Known(b)) => {
                T::unify(&a, &b)?;
                InferTy::Known(a.clone())
            }
        }
    };
    *a.0.borrow_mut() = unified.clone();
    *b.0.borrow_mut() = unified.clone();
    Ok(unified)
}

// impl<T: fmt::Display + Unify + Clone + fmt::Debug> fmt::Display for InferTyRef<T> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match &*self.0.borrow() {
//             InferTy::Any => write!(f, "ANY"),
//             InferTy::Equal(ty) => write!(f, "EQUAL({})", ty),
//             InferTy::Known(ty) => write!(f, "{}", ty),
//         }
//     }
// }