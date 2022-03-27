use std::{io::{Write, self}, fmt};

use crate::{mir::{Func, BlockId, Branch, Stmt, Expr, Assign, BinaryOp, FnCall, Block}, ty::{TyRef, Ty, Size, Signedness}, ast, compiler};

struct Compiler<W: Write> {
    stack_slots: Vec<Temp>,
    temp_count: u32,
    output: W,
}

#[derive(Debug, Clone, Copy)]
struct Temp(u32);

enum Value {
    Temp(Temp),
    Const(i32),
}

#[derive(Debug, Clone, Copy)]
struct Label(u32);

fn size_bytes(ty: &TyRef) -> u32 {
    match ty.concrete().unwrap() {
        Ty::Bool => 1,
        Ty::Int(int_ty) => match int_ty.concrete().unwrap().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref(_) => 8,
        Ty::Struct { tys, .. } => {
            let mut size = 0;
            for ty in &tys {
                size = align_to(size, align_bytes(ty)) + size_bytes(ty);
            }
            size
        }
    }
}

fn align_to(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align-1)
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
        Ty::Struct { tys, .. } => {
            let mut max = 0;
            for ty in &tys {
                let align =  align_bytes(ty);
                if align > max {
                    max = align
                }
            }
            max
        }
    }
}

struct TyName<'a, 'b>(&'a TyRef<'b>);

impl<'a, 'b> fmt::Display for TyName<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.concrete().unwrap() {
            Ty::Bool => write!(f, "w"),
            Ty::Ref(_) => write!(f, "l"),
            Ty::Int(_) => write!(f, "w"),
            Ty::Struct { name, .. } => write!(f, ":{}", name),
        }
    }
}

impl fmt::Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%v{}", self.0)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Temp(temp) => write!(f, "{}", temp),
            Value::Const(value) => write!(f, "{}", value),
        }
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

pub fn compile_fun<W: Write>(func: &Func, output: W) -> io::Result<()> {
    let mut compiler = Compiler {
        stack_slots: vec![],
        temp_count: 0,
        output,
    };
    let params: Vec<_> = func.params.iter().map(|param| (compiler.new_temp(), param)).collect();
    write!(compiler.output, "export function ")?;
    if let Some(ty) = &func.returns {
        write!(compiler.output, "{} ", TyName(ty))?;
    }
    write!(compiler.output, "${}(", func.name)?;
    let mut param_iter = params.iter();
    if let Some((temp, ty)) = param_iter.next() {
        write!(compiler.output, "{} {}", TyName(ty), temp)?;
        for (temp, ty) in param_iter {
            write!(compiler.output, ", {} {}", TyName(ty), temp)?;
        }
    }
    writeln!(compiler.output, ") {{")?;
    writeln!(compiler.output, "@start")?;
    for (temp, ty) in &params {
        let addr = compiler.alloc_ty(ty)?;
        compiler.stack_slots.push(addr);
        compiler.store(Value::Temp(*temp), ty, addr)?;
    }
    for (id, block) in func.blocks.iter().enumerate() {
        writeln!(compiler.output, "{}", Label::from_block(BlockId(id as u32)))?;
        compiler.compile_block(block)?;
    }
    writeln!(compiler.output, "}}\n")?;
    Ok(())
}

pub fn compile_struct<'f, W: Write>(name: &str, structure: &ast::Struct, program: &ast::Program<'f>, mut output: W) -> io::Result<()> {
    write!(output, "type :{} = {{ ", name)?;
    for field in &structure.fields {
        write!(output, "{}, ", TyName(&compiler::compile_ty(&field.ty, program)))?;
    }
    writeln!(output, "}}\n")
}

impl<W: Write> Compiler<W> {
    fn compile_block<'s>(&mut self, block: &Block<'s>) -> io::Result<()> {
        for stmt in &block.stmts {
            self.compile_stmt(stmt)?;
        }
        match &block.branch {
            Branch::Return(expr) => {
                match expr {
                    Some(expr) => {
                        let temp = self.compile_expr(&expr)?;
                        writeln!(self.output, "  ret {}", temp)?;
                    }
                    None => writeln!(self.output, "  ret")?,
                }
            }
            Branch::Static(target) => writeln!(self.output, "  jmp {}", Label::from_block(*target))?,
            Branch::Condition { expr, if_true, if_false } => {
                let temp = self.compile_expr(&expr)?;
                writeln!(self.output, "  jnz {}, {}, {}", temp, Label::from_block(*if_true), Label::from_block(*if_false))?;
            }
        };
        Ok(())
    }
    fn alloc_size(&mut self, size: u32, align: u32) -> io::Result<Temp> {
        let align = match align {
            0 | 1 | 2 | 4 => "4",
            8 => "8",
            16 => "16",
            _ => panic!(),
        };
        let temp = self.new_temp();
        writeln!(self.output, "  {} =l alloc{} {}", temp, align, size)?;
        Ok(temp)
    }
    fn alloc_ty(&mut self, ty: &TyRef) -> io::Result<Temp> {
        self.alloc_size(size_bytes(ty), align_bytes(ty))
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Alloc(ty) => {
                let temp = self.alloc_ty(ty)?;
                self.stack_slots.push(temp);
            }
            Stmt::Assign { assign, expr, ty } => {
                let temp = self.compile_expr(expr)?;
                let addr = self.compile_assign(assign)?;
                self.store(temp, ty, addr)?;
            }
            Stmt::FnCall(fn_call) => {
                self.compile_fn_call(fn_call, None)?;
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
            Expr::Int(value) => Value::Const(*value),
            Expr::Bool(value) => Value::Const(if *value { 1 } else { 0 }),

            Expr::Binary { left, right, op: bin_op, ty } => {
                let left_temp = self.compile_expr(left)?;
                let right_temp = self.compile_expr(right)?;
                let ty = ty.concrete().unwrap();
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
                Value::Temp(temp)
            }
            Expr::Load { stack_slot, ty } => {
                let temp = self.stack_slots[*stack_slot as usize];
                Value::Temp(self.load(ty, Value::Temp(temp))?)
            }
            Expr::Ref(stack_slot) => {
                Value::Temp(self.stack_slots[*stack_slot as usize])
            }
            Expr::Deref { expr, ty } => {
                let temp = self.compile_expr(expr)?;
                Value::Temp(self.load(ty, temp)?)
            }
            Expr::FnCall { fn_call, result } => {
                let temp = self.new_temp();
                self.compile_fn_call(fn_call, Some((temp, result)))?;
                Value::Temp(temp)
            }
            Expr::InitStruct(values) => {
                let size = values.iter().fold(0, |size, value| align_to(size, align_bytes(&value.ty)) + size_bytes(&value.ty));
                let align = values.iter().map(|value| align_bytes(&value.ty)).max().unwrap_or(0);
                let temp = self.alloc_size(size, align)?;
                let mut offset = 0;
                for value in values {
                    offset = align_to(offset, align_bytes(&value.ty));
                    let offset_temp = self.new_temp();
                    writeln!(self.output, "  {} =l add {}, {}", offset_temp, temp, offset)?;
                    let expr_temp = self.compile_expr(&value.expr)?;
                    self.store(expr_temp, &value.ty, offset_temp)?;
                    offset += size_bytes(&value.ty);
                }
                Value::Temp(temp)
            }
        })
    }
    fn compile_fn_call(&mut self, fn_call: &FnCall, returns: Option<(Temp, &TyRef)>) -> io::Result<()> {
        let values: Vec<_> = fn_call.args.iter().map(|arg| (self.compile_expr(&arg.expr).unwrap(), &arg.ty)).collect();
        write!(self.output, "  ")?;
        if let Some((temp, ty)) = returns {
            write!(self.output, "  {} ={} ", temp, TyName(ty))?;
        }
        write!(self.output, "call ${}(", fn_call.name)?;
        let mut value_iter = values.iter();
        if let Some((temp, ty)) = value_iter.next() {
            write!(self.output, "{} {}", TyName(ty), temp)?;
            for (temp, ty) in value_iter {
                write!(self.output, ", {} {}", TyName(ty), temp)?;
            }
        }
        writeln!(self.output, ")")?;
        Ok(())
    }
    fn copy_struct(&mut self, src: Value, dest: Temp, tys: Vec<TyRef>) -> io::Result<()> {
        let mut offset = 0;
        for ty in &tys {
            offset = align_to(offset, align_bytes(ty));

            let src_off = self.new_temp();
            writeln!(self.output, "  {} =l add {}, {}", src_off, src, offset)?;
            let value = self.load(ty, Value::Temp(src_off))?;
            
            let dest_off = self.new_temp();
            writeln!(self.output, "  {} =l add {}, {}", dest_off, dest, offset)?;
            
            self.store(Value::Temp(value), ty, dest_off)?;
            offset += size_bytes(ty);
        }
        Ok(())
    }
    fn store(&mut self, value: Value, ty: &TyRef, addr: Temp) -> io::Result<()> {
        match ty.concrete().unwrap() {
            Ty::Bool => {
                writeln!(self.output, "  storeb {}, {}", value, addr)?;
            }
            Ty::Int(ty) => {
                let op = match ty.concrete().unwrap().size {
                    Size::B8 => "storeb",
                    Size::B16 => "storeh",
                    Size::B32 => "storew",
                };
                writeln!(self.output, "  {} {}, {}", op, value, addr)?;
            }
            Ty::Ref(_) => {
                writeln!(self.output, "  storel {}, {}", value, addr)?;
            }
            Ty::Struct { tys, .. } => {
                self.copy_struct(value, addr, tys)?;
            }
        }
        Ok(())
    }
    fn load(&mut self, ty: &TyRef, addr: Value) -> io::Result<Temp> {
        Ok(match ty.concrete().unwrap() {
            Ty::Bool => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w loadb {}", temp, addr)?;
                temp
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
                temp
            }
            Ty::Ref(_) => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =l loadl {}", temp, addr)?;
                temp
            }
            Ty::Struct { tys, .. } => {
                let temp = self.alloc_ty(ty)?;
                self.copy_struct(addr, temp, tys)?;
                temp
            }
        })
    }
    fn new_temp(&mut self) -> Temp {
        let temp = Temp(self.temp_count);
        self.temp_count += 1;
        temp
    }
}