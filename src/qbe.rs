use std::{io::{Write, self}, fmt};

use crate::{mir::{Fun, BlockId, Branch, Stmt, Expr, Assign, BinaryOp}, ty::{IntTy, TyRef, Ty, Size, Signedness}};

struct Compiler<'f, W: Write> {
    stack_slots: Vec<Temp>,
    temp_count: u32,
    fun: &'f Fun,
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
    None,
}

fn size_bytes(ty: &TyRef) -> u32 {
    match ty.concrete() {
        Ty::Bool => 1,
        Ty::None => 0,
        Ty::Int(int_ty) => match int_ty.concrete().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref => 8,
    }
}

fn align_bytes(ty: &TyRef) -> u32 {
    match ty.concrete() {
        Ty::Bool => 1,
        Ty::None => 0,
        Ty::Int(int_ty) => match int_ty.concrete().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref => 8,
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
    writeln!(compiler.output, "export function w $test() {{")?;
    for block_id in fun.blocks() {
        compiler.compile_block(block_id)?;
    }
    writeln!(compiler.output, "}}")?;
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
                            Value::None => writeln!(self.output, "  ret")?,
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
    fn compile_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Alloc(ty) => {
                let align = match align_bytes(ty) {
                    0..=4 => "4",
                    5..=8 => "8",
                    9..=16 => "16",
                    _ => panic!(),
                };
                let temp = self.new_temp();
                self.stack_slots.push(temp);
                writeln!(self.output, "  {} =l alloc{} {}", temp, align, size_bytes(ty))?;
            }
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
                writeln!(self.output, "  {} =w add {}, 0", temp, value)?;
                Value::Int { temp, ty: ty.concrete() }
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
                writeln!(self.output, "  storew {}, {}", temp, addr)?;
            }
            Value::None => panic!(),
        }
        Ok(())
    }
    fn load(&mut self, ty: &TyRef, addr: Temp) -> io::Result<Value> {
        Ok(match ty.concrete() {
            Ty::Bool => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w loadb {}", temp, addr)?;
                Value::Bool(temp)
            }
            Ty::Int(int_ty) => {
                let temp = self.new_temp();
                let int = int_ty.concrete();
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
            Ty::Ref => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =l loadl {}", temp, addr)?;
                Value::Pointer(temp)
            }
            Ty::None => Value::None,
        })
    }
    fn new_temp(&mut self) -> Temp {
        let temp = Temp(self.temp_count);
        self.temp_count += 1;
        temp
    }
}