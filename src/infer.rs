use std::{rc::Rc, cell::RefCell};

#[derive(Debug, Clone)]
pub struct InferTyRef<T: PartialEq + Clone>(Rc<RefCell<InferTy<T>>>);

#[derive(Debug, Clone)]
pub enum InferTy<T: PartialEq + Clone> {
    Any,
    Equal(InferTyRef<T>),
    Known {
        ty: T,
        args: Vec<InferTyRef<T>>,
    },
}

impl<T: PartialEq + Clone> InferTy<T> {
    pub fn as_ref(self) -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(self)))
    }
}

impl<T: PartialEq + Clone> InferTyRef<T> {
    pub fn any() -> InferTyRef<T> {
        InferTy::Any.as_ref()
    }
    pub fn known(ty: T, args: Vec<InferTyRef<T>>) -> InferTyRef<T> {
        InferTy::Known { ty, args }.as_ref()
    }
}

impl<T: PartialEq + Clone> PartialEq for InferTyRef<T> {
    fn eq(&self, other: &Self) -> bool {
        match unify(self, other, false) {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

pub fn unify<T: PartialEq + Clone>(a: &InferTyRef<T>, b: &InferTyRef<T>, should_unify: bool) -> Result<InferTy<T>, ()> {
    let unified = match (&*a.0.borrow(), &*b.0.borrow()) {
        (InferTy::Equal(a), _) => unify(&a, b, should_unify)?,
        (_, InferTy::Equal(b)) => unify(a, &b, should_unify)?,

        (InferTy::Any, InferTy::Any) => InferTy::Equal(InferTyRef::any()),
        (InferTy::Any, ty) => ty.clone(),
        (ty, InferTy::Any) => ty.clone(),

        (InferTy::Known { ty: ty_a, args: args_a }, InferTy::Known { ty: ty_b, args: args_b }) => {
            if ty_a != ty_b {
                panic!()
            } else if args_a.len() != args_b.len() {
                panic!()
            } else {
                let mut args = vec![];
                for (arg_a, arg_b) in args_a.iter().zip(args_b) {
                    args.push(unify(arg_a, arg_b, should_unify)?.as_ref());
                }
                InferTy::Known { ty: ty_a.clone(), args }
            }
        }
    };
    if should_unify {
        *a.0.borrow_mut() = unified.clone();
        *b.0.borrow_mut() = unified.clone();
    }
    Ok(unified)
}