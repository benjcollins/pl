use std::{io::{Write, self}, fmt};

use crate::{ty::{Size, Signedness}, ast, symbols::{Symbols, Symbol}, lir::{self, Stmt, Assign, Branch, Expr, Ty, FuncCall}, mir::BinaryOp};

struct Compiler<'a, W: Write> {
    stack_slots: Vec<Temp>,
    temp_count: u32,
    output: W,
    symbols: &'a Symbols<'a>,
    program: &'a ast::Program,
}

#[derive(Debug, Clone, Copy)]
struct Temp(u32);

enum Value {
    Temp(Temp),
    Const(i64),
}

#[derive(Debug, Clone, Copy)]
struct Label(u32);

fn size_bytes(ty: &lir::Ty) -> u32 {
    match ty {
        lir::Ty::Bool => 1,
        lir::Ty::Int(int_ty) => match int_ty.size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        lir::Ty::Ptr => 8,
        lir::Ty::Struct(fields) => {
            let mut size = 0;
            for ty in fields {
                size = align_to(size, align_bytes(ty)) + size_bytes(ty);
            }
            size
        }
    }
}

fn align_to(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align-1)
}

fn align_bytes(ty: &lir::Ty) -> u32 {
    match ty {
        lir::Ty::Bool => 1,
        lir::Ty::Int(int_ty) => match int_ty.size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        lir::Ty::Ptr => 8,
        lir::Ty::Struct(fields) => {
            let mut max = 0;
            for ty in fields {
                let align =  align_bytes(ty);
                if align > max {
                    max = align
                }
            }
            max
        }
    }
}

struct TyName<'a> {
    ty: &'a ast::Ty,
    symbols: &'a Symbols<'a>,
}

impl<'a> TyName<'a> {
    fn new(ty: &'a ast::Ty, symbols: &'a Symbols<'a>) -> TyName<'a> {
        TyName { ty, symbols }
    }
}

impl<'a> fmt::Display for TyName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            ast::Ty::Bool => write!(f, "w"),
            ast::Ty::Ref(_) => write!(f, "l"),
            ast::Ty::Int(_) => write!(f, "w"),
            ast::Ty::Struct(name) => write!(f, ":{}", self.symbols.get_str(*name)),
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

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@l{}", self.0)
    }
}

pub fn compile_fun<'a, W: Write>(func: &lir::Func, output: W, symbols: &'a Symbols<'a>, program: &ast::Program) -> io::Result<()> {
    let mut compiler = Compiler {
        stack_slots: vec![],
        temp_count: 0,
        output,
        symbols,
        program,
    };
    let func_ast = program.funcs.get(&func.name).unwrap();
    write!(compiler.output, "export function ")?;
    if let Some(ty) = &func_ast.returns {
        write!(compiler.output, "{} ", TyName::new(ty, symbols))?;
    }
    write!(compiler.output, "${}(", symbols.get_str(func.name))?;
    let param_temps: Vec<_> = (0..func_ast.params.len()).map(|_| compiler.new_temp()).collect();
    let mut param_iter = param_temps.iter().zip(&func_ast.params);
    if let Some((temp, param)) = param_iter.next() {
        write!(compiler.output, "{} {}", TyName::new(&param.ty, symbols), temp)?;
        for (temp, param) in param_iter {
            write!(compiler.output, ", {} {}", TyName::new(&param.ty, symbols), temp)?;
        }
    }
    writeln!(compiler.output, ") {{")?;
    writeln!(compiler.output, "@start")?;
    for (temp, ty) in param_temps.iter().zip(&func.params) {
        let addr = compiler.alloc_ty(&ty)?;
        compiler.stack_slots.push(addr);
        compiler.store(Value::Temp(*temp), &ty, addr)?;
    }
    for (id, block) in func.blocks.iter().enumerate() {
        writeln!(compiler.output, "{}", Label(id as u32))?;
        compiler.compile_block(block)?;
    }
    writeln!(compiler.output, "}}\n")?;
    Ok(())
}

pub fn compile_struct<W: Write>(name: Symbol, structure: &ast::Struct, mut output: W, symbols: &Symbols) -> io::Result<()> {
    write!(output, "type :{} = {{ ", symbols.get_str(name))?;
    for field in &structure.fields {
        write!(output, "{}, ", TyName::new(&field.ty, symbols))?;
    }
    writeln!(output, "}}\n")
}

impl<'a, W: Write> Compiler<'a, W> {
    fn compile_block<'s>(&mut self, block: &lir::Block) -> io::Result<()> {
        for stmt in &block.stmts {
            self.compile_stmt(stmt)?;
        }
        match &block.branch {
            Branch::Return(expr) => {
                match expr {
                    Some(expr) => {
                        let temp = self.compile_expr(expr)?;
                        writeln!(self.output, "  ret {}", temp)?;
                    }
                    None => writeln!(self.output, "  ret")?,
                }
            }
            Branch::Static(target) => writeln!(self.output, "  jmp {}", Label(target.0))?,
            Branch::Condition { expr, if_true, if_false } => {
                let temp = self.compile_expr(&expr)?;
                writeln!(self.output, "  jnz {}, {}, {}", temp, Label(if_true.0), Label(if_false.0))?;
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
    fn alloc_ty(&mut self, ty: &lir::Ty) -> io::Result<Temp> {
        self.alloc_size(size_bytes(ty), align_bytes(ty))
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Alloc(ty) => {
                let temp = self.alloc_ty(&ty)?;
                self.stack_slots.push(temp);
            }
            Stmt::Assign { assign, expr, ty } => {
                let temp = self.compile_expr(expr)?;
                let addr = self.compile_assign(assign)?;
                self.store(temp, &ty, addr)?;
            }
            Stmt::FuncCall(func_call) => {
                let returns = self.compile_func_call(func_call)?;
                if returns.is_some() {
                    panic!()
                }
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
            Assign::Variable(var) => {
                self.stack_slots[var.0 as usize]
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
            Expr::Load { var, ty } => {
                let temp = self.stack_slots[var.0 as usize];
                self.load(&ty, Value::Temp(temp))?
            }
            Expr::Ref(var) => {
                Value::Temp(self.stack_slots[var.0 as usize])
            }
            Expr::Deref { expr, ty } => {
                let temp = self.compile_expr(expr)?;
                self.load(&ty, temp)?
            }
            Expr::FuncCall(func_call) => {
                let temp = self.compile_func_call(func_call)?.unwrap();
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
    fn compile_func_call(&mut self, func_call: &FuncCall) -> io::Result<Option<Temp>> {
        let func = self.program.funcs.get(&func_call.name).unwrap();

        let values: Vec<_> = func_call.args.iter().map(|expr| self.compile_expr(&expr).unwrap()).collect();
        write!(self.output, "  ")?;
        let temp = if let Some(ty) = func.returns.as_ref() {
            let temp = self.new_temp();
            write!(self.output, "{} ={} ", temp, TyName::new(&ty, self.symbols))?;
            Some(temp)
        } else {
            None
        };

        write!(self.output, "call ${}(", self.symbols.get_str(func_call.name))?;
        let mut value_iter = values.iter().zip(&func.params);
        if let Some((temp, param)) = value_iter.next() {
            write!(self.output, "{} {}", TyName::new(&param.ty, self.symbols), temp)?;
            for (temp, param) in value_iter {
                write!(self.output, ", {} {}", TyName::new(&param.ty, self.symbols), temp)?;
            }
        }
        writeln!(self.output, ")")?;
        Ok(temp)
    }
    fn copy_struct(&mut self, src: Value, dest: Temp, tys: &[Ty]) -> io::Result<()> {
        let mut offset = 0;
        for ty in tys {
            offset = align_to(offset, align_bytes(ty));

            let src_off = self.new_temp();
            writeln!(self.output, "  {} =l add {}, {}", src_off, src, offset)?;
            let value = self.load(ty, Value::Temp(src_off))?;
            
            let dest_off = self.new_temp();
            writeln!(self.output, "  {} =l add {}, {}", dest_off, dest, offset)?;
            
            self.store(value, ty, dest_off)?;
            offset += size_bytes(ty);
        }
        Ok(())
    }
    fn store(&mut self, value: Value, ty: &Ty, addr: Temp) -> io::Result<()> {
        match ty {
            Ty::Bool => {
                writeln!(self.output, "  storeb {}, {}", value, addr)?;
            }
            Ty::Int(ty) => {
                let op = match ty.size {
                    Size::B8 => "storeb",
                    Size::B16 => "storeh",
                    Size::B32 => "storew",
                };
                writeln!(self.output, "  {} {}, {}", op, value, addr)?;
            }
            Ty::Ptr => {
                writeln!(self.output, "  storel {}, {}", value, addr)?;
            }
            Ty::Struct(tys) => {
                self.copy_struct(value, addr, tys)?;
            }
        }
        Ok(())
    }
    fn load(&mut self, ty: &Ty, addr: Value) -> io::Result<Value> {
        Ok(match &ty {
            Ty::Bool => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =w loadb {}", temp, addr)?;
                Value::Temp(temp)
            }
            Ty::Int(int) => {
                let temp = self.new_temp();
                let op = match (int.signedness, int.size) {
                    (Signedness::Signed, Size::B8) => "loadsb",
                    (Signedness::Signed, Size::B16) => "loadsh",
                    (Signedness::Signed, Size::B32) => "loadsw",
                    (Signedness::Unsigned, Size::B8) => "loadub",
                    (Signedness::Unsigned, Size::B16) => "loaduh",
                    (Signedness::Unsigned, Size::B32) => "loaduw",
                };
                writeln!(self.output, "  {} =w {} {}", temp, op, addr)?;
                Value::Temp(temp)
            }
            Ty::Ptr => {
                let temp = self.new_temp();
                writeln!(self.output, "  {} =l loadl {}", temp, addr)?;
                Value::Temp(temp)
            }
            Ty::Struct(_) => {
                // let temp = self.alloc_ty(ty)?;
                // self.copy_struct(addr, temp, s)?;
                addr
            }
        })
    }
    fn new_temp(&mut self) -> Temp {
        let temp = Temp(self.temp_count);
        self.temp_count += 1;
        temp
    }
}