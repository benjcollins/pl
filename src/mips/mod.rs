use crate::{mir::{Fun, BlockId, Stmt, Branch, Expr, CompareOp, BinaryOp, Assign}, ty::{IntTy, Ty, TyRef, Size, Signedness}};

use self::regs::{TempReg, TEMP_REGS, Reg, ValReg};

pub mod regs;

struct Compiler<'a> {
    stack_slots: Vec<u32>,
    stack_len: u32,
    free_temp_regs: Vec<TempReg>,
    fun: &'a Fun,
    output: String,
}

enum Value {
    Int {
        reg: TempReg,
        ty: IntTy,
    },
    Pointer(TempReg),
    Bool(TempReg),
    None,
}

fn ty_len_bytes(ty: &TyRef) -> u32 {
    match ty.concrete() {
        Ty::Bool => 1,
        Ty::None => 0,
        Ty::Int(int_ty) => match int_ty.concrete().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref => 4,
    }
}

fn ty_align_bytes(ty: &TyRef) -> u32 {
    match ty.concrete() {
        Ty::Bool => 1,
        Ty::None => 0,
        Ty::Int(int_ty) => match int_ty.concrete().size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
        Ty::Ref => 4,
    }
}

pub fn compile_fun(fun: &Fun) -> String {
    let mut compiler = Compiler {
        free_temp_regs: TEMP_REGS.iter().copied().collect(),
        fun,
        output: String::new(),
        stack_len: 0,
        stack_slots: vec![],
    };
    for block_id in fun.blocks() {
        compiler.compile_block(block_id)
    }
    compiler.output
}

impl<'a> Compiler<'a> {
    fn compile_block(&mut self, block_id: BlockId) {
        self.output.push_str(&format!("l{}:\n", block_id.id()));
        let block = self.fun.get_block(block_id);
        for stmt in &block.stmts {
            self.compile_stmt(stmt);
        }
        match &block.branch {
            Branch::End => {}
            Branch::Static(block_id) => {
                self.output.push_str(&format!("  j l{}\n", block_id.id()))
            }
            Branch::Bool { expr, if_true, if_false } => {
                let value = self.compile_expr(expr);
                let reg = match value {
                    Value::Bool(reg) => reg,
                    _ => panic!(),
                };
                self.output.push_str(&format!("  bne $zero, {}, l{}\n", Reg::TempReg(reg), if_true.id()));
                self.output.push_str(&format!("  j l{}\n", if_false.id()));
            }
            Branch::Compare { a, b, cmp, if_true, if_false } => {
                let (a_reg, a_ty) = match self.compile_expr(a) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                let (b_reg, b_ty) = match self.compile_expr(b) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                assert_eq!(a_ty, b_ty);
                let reg = self.alloc_temp_reg();
                let (rs, rt) = match cmp {
                    CompareOp::LessThan => (b_reg, a_reg),
                    CompareOp::GreaterThan => (a_reg, b_reg),
                };
                let op = match a_ty.signedness {
                    Signedness::Signed => "sub",
                    Signedness::Unsigned => "subu",
                };
                self.output.push_str(&format!("  {} {} {} {}\n", op, Reg::TempReg(reg), Reg::TempReg(rs), Reg::TempReg(rt)));
                self.output.push_str(&format!("  bgtz {}, l{}\n", Reg::TempReg(reg), if_true.id()));
                self.output.push_str(&format!("  j l{}\n", if_false.id()));
            }
        }
    }
    fn compile_assign(&mut self, assign: &Assign) -> (Reg, i32) {
        match assign {
            Assign::Deref(assign) => {
                let (base_reg, offset) = self.compile_assign(assign);
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  lw {}, {}({})\n", Reg::TempReg(reg), offset, base_reg));
                (Reg::TempReg(reg), 0)
            }
            Assign::Stack(stack_slot) => {
                let offset = -(self.stack_slots[*stack_slot as usize] as i32);
                (Reg::FP, offset)
            }
        }
    }
    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Alloc(ty) => {
                let align = ty_align_bytes(ty);
                let padding = if self.stack_len % align == 0 { 0 } else { align - self.stack_len % align };
                self.stack_len += padding + ty_len_bytes(ty);
                self.stack_slots.push(self.stack_len);
            }
            Stmt::Drop => {
                self.stack_slots.pop();
                self.stack_len = *self.stack_slots.last().unwrap_or(&0);
            }
            Stmt::Assign { assign, expr } => {
                let value = self.compile_expr(expr);
                let (base_reg, offset) = self.compile_assign(assign);
                self.store(value, base_reg, offset);
            }
            Stmt::Return(expr) => {
                let value = self.compile_expr(expr);
                match value {
                    Value::None => {},
                    Value::Bool(reg) | Value::Int { reg, .. } | Value::Pointer(reg) => {
                        self.output.push_str(&format!("  move {}, {}\n", Reg::ValReg(ValReg::V0), Reg::TempReg(reg)))
                    }
                }
                self.output.push_str("  jr $ra\n");
            }
        }
    }
    fn compile_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Int { value, ty } => {
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  li {}, {}\n", Reg::TempReg(reg), *value));
                Value::Int { ty: ty.concrete(), reg }
            }
            Expr::Bool(value) => {
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  li {}, {}\n", Reg::TempReg(reg), if *value { 1 } else { 0 }));
                Value::Bool(reg)
            }
            Expr::Load { stack_slot, ty } => {
                let addr = -(self.stack_slots[*stack_slot as usize] as i32);
                self.load(ty, Reg::FP, addr)
            }
            Expr::Binary { left, right, op } => {
                let (left_reg, left_ty) = match self.compile_expr(left) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                let (right_reg, right_ty) = match self.compile_expr(right) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                assert_eq!(left_ty, right_ty);
                let op = match (op, left_ty.signedness) {
                    (BinaryOp::Add, Signedness::Signed) => "add",
                    (BinaryOp::Add, Signedness::Unsigned) => "addu",
                    (BinaryOp::Subtract, Signedness::Signed) => "sub",
                    (BinaryOp::Subtract, Signedness::Unsigned) => "subu",
                    
                    (BinaryOp::Multiply, Signedness::Signed) => todo!(),
                    (BinaryOp::Multiply, Signedness::Unsigned) => todo!(),
                    (BinaryOp::Divide, Signedness::Signed) => todo!(),
                    (BinaryOp::Divide, Signedness::Unsigned) => todo!(),
                };
                let reg = self.alloc_temp_reg();
                
                self.output.push_str(&format!("  {} {}, {}, {}\n", op, Reg::TempReg(reg), Reg::TempReg(left_reg), Reg::TempReg(right_reg)));

                self.free_temp_reg(left_reg);
                self.free_temp_reg(right_reg);

                Value::Int { reg, ty: left_ty }
            }
            Expr::Ref(stack_slot) => {
                let addr = self.stack_slots[*stack_slot as usize];
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  addi {}, $sp, -{}\n", Reg::TempReg(reg), addr));
                Value::Pointer(reg)
            }
            Expr::Deref { ty, expr } => {
                let value = self.compile_expr(expr);
                let reg = match value {
                    Value::Pointer(reg) => reg,
                    _ => panic!(),
                };
                self.load(ty, Reg::TempReg(reg), 0)
            }
        }
    }
    fn store(&mut self, value: Value, base_reg: Reg, offset: i32) {
        match value {
            Value::Bool(reg) => {
                self.output.push_str(&format!("  sb {}, {}({})\n", Reg::TempReg(reg), offset, base_reg));
                self.free_temp_reg(reg);
            }
            Value::Int { reg, ty } => {
                let op = match ty.size {
                    Size::B8 => "sb",
                    Size::B16 => "sh",
                    Size::B32 => "sw",
                };
                self.output.push_str(&format!("  {} {}, {}({})\n", op, Reg::TempReg(reg), offset, base_reg));
                self.free_temp_reg(reg);
            }
            Value::Pointer(reg) => {
                self.output.push_str(&format!("  sw {}, {}({})\n", Reg::TempReg(reg), offset, base_reg));
                self.free_temp_reg(reg);
            }
            Value::None => panic!(),
        }
    }
    fn load(&mut self, ty: &TyRef, base_reg: Reg, offset: i32) -> Value {
        match ty.concrete() {
            Ty::Bool => {
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  lb {}, {}({})\n", Reg::TempReg(reg), offset, base_reg));
                Value::Bool(reg)
            }
            Ty::Int(int_ty) => {
                let reg = self.alloc_temp_reg();
                let int = int_ty.concrete();
                let op = match (int.signedness, int.size) {
                    (Signedness::Signed, Size::B8) => "lb",
                    (Signedness::Signed, Size::B16) => "lh",
                    (Signedness::Signed, Size::B32) => "lw",
                    (Signedness::Unsigned, Size::B8) => "lbu",
                    (Signedness::Unsigned, Size::B16) => "lhu",
                    (Signedness::Unsigned, Size::B32) => "lw",
                };
                self.output.push_str(&format!("  {} {}, {}({})\n", op, Reg::TempReg(reg), offset, base_reg));
                Value::Int { reg, ty: int }
            }
            Ty::Ref => {
                let reg = self.alloc_temp_reg();
                self.output.push_str(&format!("  lw {}, {}({})\n", Reg::TempReg(reg), offset, base_reg));
                Value::Pointer(reg)
            }
            Ty::None => Value::None,
        }
    }
    fn alloc_temp_reg(&mut self) -> TempReg {
        self.free_temp_regs.pop().unwrap()
    }
    fn free_temp_reg(&mut self, reg: TempReg) {
        self.free_temp_regs.push(reg);
    }
}