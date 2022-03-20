use std::{rc::Rc, cell::{RefCell, Ref}, fmt};

#[derive(Debug, Clone)]
pub struct InferTyRef<T: Unify + Clone>(Rc<RefCell<InferTy<T>>>);

#[derive(Debug, Clone)]
pub enum InferTy<T: Unify + Clone> {
    Any,
    Equal(InferTyRef<T>),
    Known {
        ty: T,
        args: Vec<InferTyRef<T>>,
    },
}

pub trait Unify {
    fn unify(a: &Self, b: &Self) -> Result<(), ()>;
}

impl<T: Unify + Clone> InferTyRef<T> {
    pub fn any() -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Any)))
    }
    pub fn known_with_args(ty: T, args: Vec<InferTyRef<T>>) -> InferTyRef<T> {
        InferTyRef(Rc::new(RefCell::new(InferTy::Known { ty, args })))
    }
    pub fn known(ty: T) -> InferTyRef<T> {
        InferTyRef::known_with_args(ty, vec![])
    }
    pub fn infer_ty(&self) -> Ref<'_, InferTy<T>> {
        self.0.borrow()
    }
    pub fn concrete(&self) -> T {
        match &*self.0.borrow() {
            InferTy::Any => panic!(),
            InferTy::Equal(ty) => ty.concrete(),
            InferTy::Known { ty, .. } => ty.clone(),
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

            (InferTy::Known { ty: ty_a, args: args_a }, InferTy::Known { ty: ty_b, args: args_b }) => {
                T::unify(&ty_a, &ty_b)?;
                if args_a.len() != args_b.len() {
                    Err(())?
                } else {
                    let mut args = vec![];
                    for (arg_a, arg_b) in args_a.iter().zip(args_b) {
                        args.push(InferTyRef(Rc::new(RefCell::new(unify(arg_a, arg_b)?))));
                    }
                    InferTy::Known { ty: ty_a.clone(), args }
                }
            }
        }
    };
    *a.0.borrow_mut() = unified.clone();
    *b.0.borrow_mut() = unified.clone();
    Ok(unified)
}

impl<T: fmt::Display + Unify + Clone + fmt::Debug> fmt::Display for InferTyRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0.borrow() {
            InferTy::Any => write!(f, "ANY"),
            InferTy::Equal(ty) => write!(f, "EQUAL({})", ty),
            InferTy::Known { ty, args } => {
                write!(f, "{}[", ty)?;
                let mut arg_iter = args.iter();
                if let Some(arg) = arg_iter.next() {
                    write!(f, "{}", arg)?;
                    for arg in arg_iter {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}