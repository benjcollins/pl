use std::{io::{Write, self}, fmt};

use crate::{mir::{Fun, BlockId, Branch, Stmt, Expr, Assign, BinaryOp}, ty::{IntTy, TyRef, Ty, Size, Signedness}};

struct Compiler<'f, W: Write> {
    stack_slots: Vec<Temp>,
    temp_count: u32,
    fun: &'f Fun<'f>,
    output: W,
}

#[derive(Debug, Clone, Copy)]
struct Temp(u32);

#[derive(Debug, Clone, Copy)]
struct Label(u32);

enum Value {
    Int {
        temp: Temp,
        ty: IntTy,
    },
    Pointer(Temp),
    Bool(Temp),
}

fn size_bytes(ty: &TyRef) -> u32 {
    match ty.concrete().unwrap() {
        Ty::Bool => 1,
        Ty::Int(int_ty) => match int_ty.concrete().unwrap().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref(_) => 8,
    }
}

fn align_bytes(ty: &TyRef) -> u32 {
    match ty.concrete().unwrap() {
        Ty::Bool => 1,
        Ty::Int(int_ty) => match int_ty.concrete().unwrap().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref(_) => 8,
    }
}

fn ty_name(ty: &TyRef) -> &str {
    match ty.concrete().unwrap() {
        Ty::Bool => "w",
        Ty::Ref(_) => "l",
        Ty::Int(_) => "w",
    }
}

impl Value {
    fn name(&self) -> &str {
        match self {
            Value::Int { .. } => "w",
            Value::Pointer(_) => "l",
            Value::Bool(_) => "w",
        }
    }
    fn temp(&self) -> Temp {
        match *self {
            Value::Int { temp, .. } => temp,
            Value::Pointer(temp) => temp,
            Value::Bool(temp) => temp,
        }
    }
}

impl fmt::Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%v{}", self.0)
    }
}

impl Label {
    fn from_block(block: BlockId) -> Label {
        Label(block.id())
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@l{}", self.0)
    }
}

pub fn compile_fun<W: Write>(fun: &Fun, output: W) -> io::Result<()> {
    let mut compiler = Compiler {
        stack_slots: vec![],
        temp_count: 0,
        fun,
        output,
    };
    let params: Vec<_> = fun.params.iter().map(|param| (compiler.new_temp(), param)).collect();
    if fun.is_extern {
        write!(compiler.output, "export ")?;
    }
    write!(compiler.output, "function {} ${}(", fun.returns.as_ref().map_or("", |ty| ty_name(ty)), fun.name)?;
    let mut param_iter = params.iter();
    if let Some((temp, ty)) = param_iter.next() {
        write!(compiler.output, "{} {}", ty_name(ty), temp)?;
        for (temp, ty) in param_iter {
            write!(compiler.output, ", {} {}", ty_name(ty), temp)?;
        }
    }
    writeln!(compiler.output, ") {{")?;
    writeln!(compiler.output, "@start")?;
    for (temp, ty) in &params {
        let addr = compiler.alloc_ty(ty)?;
        match ty.concrete().unwrap() {
            Ty::Bool => compiler.store(Value::Bool(*temp), addr)?,
            Ty::Ref(_) => compiler.store(Value::Pointer(*temp), addr)?,
            Ty::Int(ty) => compiler.store(Value::Int { temp: *temp, ty: ty.concrete().unwrap() }, addr)?,
        }
    }
    for block_id in fun.blocks() {
        compiler.compile_block(block_id)?;
    }
    writeln!(compiler.output, "}}\n")?;
    Ok(())
}

impl<'f, W: Write> Compiler<'f, W> {
    fn compile_block(&mut self, block_id: BlockId) -> io::Result<()> {
        let block = self.fun.get_block(block_id);
        writeln!(self.output, "{}", Label::from_block(block_id))?;
        for stmt in &block.stmts {
            self.compile_stmt(stmt)?;
        }
        match &block.branch {
            Branch::Return(expr) => {
                match expr {
                    Some(expr) => {
                        let value = self.compile_expr(expr)?;
                        match value {
                            Value::Bool(temp) | Value::Int { temp, .. } | Value::Pointer(temp) => {
                                writeln!(self.output, "  ret {}", temp)?;
                            }
                        };
                    }
                    None => writeln!(self.output, "  ret")?,
                }
            }
            Branch::Static(target) => writeln!(self.output, "  jmp {}", Label::from_block(*target))?,
            Branch::Condition { expr, if_true, if_false } => {
                let value = self.compile_expr(&expr)?;
                let temp = match value {
                    Value::Bool(val) => val,
                    _ => panic!(),
                };
                writeln!(self.output, "  jnz {}, {}, {}", temp, Label::from_block(*if_true), Label::from_block(*if_false))?;
            }
        };
        Ok(())
    }
    fn alloc_ty(&mut self, ty: &TyRef) -> io::Result<Temp> {
        let align = match align_bytes(ty) {
            0..=4 => "4",
            5..=8 => "8",
            9..=16 => "16",
            _ => panic!(),
        };
        let temp = self.new_temp();
        self.stack_slots.push(temp);
        writeln!(self.output, "  {} =l alloc{} {}", temp, align, size_bytes(ty))?;
        Ok(temp)
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Alloc(ty) => _ = self.alloc_ty(ty)?,
            Stmt::Assign { assign, expr } => {
                let value = self.compile_expr(expr)?;
                let temp = self.compile_assign(assign)?;
                self.store(value, temp)?;
            }
        };
        Ok(())
    }
    fn compile_assign(&mut self, assign: &Assign) -> io::Result<Temp> {
        Ok(match assign {
            Assign::Deref(assign) => {
                let addr = self.compile_assign(assign)?;
                let temp = self.new_temp();
                writeln!(self.output, "  {} =l loadl {}", temp, addr)?;
                temp
            }
            Assign::Stack(stack_slot) => {
                self.stack_slots[*stack_slot as usize]
            }
        })
    }
    fn compile_expr(&mut self, expr: &Expr) -> io::Result<Value> {
        Ok(match expr {
            Expr::Int { value, ty } => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w copy {}", temp, value)?;
                Value::Int { temp, ty: ty.concrete().unwrap() }
            }
            Expr::Bool(value) => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w copy {}", temp, if *value { 1 } else { 0 })?;
                Value::Bool(temp)
            }
            Expr::Binary { left, right, op: bin_op } => {
                let (left_temp, left_ty) = match self.compile_expr(left)? {
                    Value::Int { temp, ty } => (temp, ty),
                    _ => panic!(),
                };
                let (right_temp, right_ty) = match self.compile_expr(right)? {
                    Value::Int { temp, ty } => (temp, ty),
                    _ => panic!(),
                };
                assert_eq!(left_ty, right_ty);
                let ty = left_ty;
                let op = match bin_op {
                    BinaryOp::Add => "add",
                    BinaryOp::Subtract => "sub",
                    BinaryOp::Multiply => "mul",
                    BinaryOp::Divide => match ty.signedness {
                        Signedness::Signed => "div",
                        Signedness::Unsigned => "udiv",
                    }
                    BinaryOp::LessThan => match ty.signedness {
                        Signedness::Signed => "csltw",
                        Signedness::Unsigned => "cultw",
                    }
                    BinaryOp::GreaterThan => match ty.signedness {
                        Signedness::Signed => "csgtw",
                        Signedness::Unsigned => "cugtw",
                    }
                };
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w {} {}, {}", temp, op, left_temp, right_temp)?;
                match bin_op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => Value::Int { temp, ty },
                    BinaryOp::LessThan | BinaryOp::GreaterThan => Value::Bool(temp),
                }
            }
            Expr::Load { stack_slot, ty } => {
                let temp = self.stack_slots[*stack_slot as usize];
                self.load(ty, temp)?
            }
            Expr::Ref(stack_slot) => {
                Value::Pointer(self.stack_slots[*stack_slot as usize])
            }
            Expr::Deref { expr, ty } => {
                let value = self.compile_expr(expr)?;
                let temp = match value {
                    Value::Pointer(temp) => temp,
                    _ => panic!(),
                };
                self.load(ty, temp)?
            }
            Expr::FnCall { name, args, result } => {
                let temp = self.new_temp();
                let value = match result.concrete().unwrap() {
                    Ty::Bool => Value::Bool(temp),
                    Ty::Ref(_) => Value::Pointer(temp),
                    Ty::Int(ty) => Value::Int { temp, ty: ty.concrete().unwrap() },
                };
                let values: Vec<_> = args.iter().map(|arg| self.compile_expr(arg).unwrap()).collect();
                write!(self.output, "  {} ={} call ${}(", temp, ty_name(result), name)?;
                let mut value_iter = values.iter();
                if let Some(value) = value_iter.next() {
                    write!(self.output, "{} {}", value.name(), value.temp())?;
                    for value in value_iter {
                        write!(self.output, ", {} {}", value.name(), value.temp())?;
                    }
                }
                writeln!(self.output, ")")?;
                value
            }
        })
    }
    fn store(&mut self, value: Value, addr: Temp) -> io::Result<()> {
        match value {
            Value::Bool(temp) => {
                writeln!(self.output, "  storeb {}, {}", temp, addr)?;
            }
            Value::Int { temp, ty } => {
                let op = match ty.size {
                    Size::B8 => "storeb",
                    Size::B16 => "storeh",
                    Size::B32 => "storew",
                };
                writeln!(self.output, "  {} {}, {}", op, temp, addr)?;
            }
            Value::Pointer(temp) => {
                writeln!(self.output, "  storel {}, {}", temp, addr)?;
            }
        }
        Ok(())
    }
    fn load(&mut self, ty: &TyRef, addr: Temp) -> io::Result<Value> {
        Ok(match ty.concrete().unwrap() {
            Ty::Bool => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w loadb {}", temp, addr)?;
                Value::Bool(temp)
            }
            Ty::Int(int_ty) => {
                let temp = self.new_temp();
                let int = int_ty.concrete().unwrap();
                let op = match (int.signedness, int.size) {
                    (Signedness::Signed, Size::B8) => "loadsb",
                    (Signedness::Signed, Size::B16) => "loadsh",
                    (Signedness::Signed, Size::B32) => "loadsw",
                    (Signedness::Unsigned, Size::B8) => "loadub",
                    (Signedness::Unsigned, Size::B16) => "loaduh",
                    (Signedness::Unsigned, Size::B32) => "loaduw",
                };
                writeln!(self.output, "  {} =w {} {}", temp, op, addr)?;
                Value::Int { temp, ty: int }
            }
            Ty::Ref(_) => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =l loadl {}", temp, addr)?;
                Value::Pointer(temp)
            }
        })
    }
    fn new_temp(&mut self) -> Temp {
        let temp = Temp(self.temp_count);
        self.temp_count += 1;
        temp
    }
}