use crate::{mir::{Block, Stmt, Expr, BinaryOp, Fun, Branch, BlockId, Int, Size, Signedness, ConcreteTy, Compare}, mips::{inst::{Inst, OpcodeI, Funct}, regs::Reg}};

use super::{regs::{TempReg, TEMP_REGS, ValReg}, asm::{AsmBuilder, Label}, inst::OpcodeJ};

pub struct Compiler<'a> {
    stack_slots: Vec<u16>,
    stack_len: u16,
    free_temp_regs: Vec<TempReg>,
    builder: AsmBuilder,
    fun: &'a Fun,
}

pub enum Value {
    Int {
        reg: TempReg,
        ty: Int,
    },
    Bool(TempReg),
    None,
}

fn ty_len_bytes(ty: ConcreteTy) -> u16 {
    match ty {
        ConcreteTy::Bool => 1,
        ConcreteTy::None => 0,
        ConcreteTy::Int(int) => match int.size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
    }
}

fn ty_align_bytes(ty: ConcreteTy) -> u16 {
    match ty {
        ConcreteTy::Bool => 1,
        ConcreteTy::None => 0,
        ConcreteTy::Int(int) => match int.size {
            Size::B8 => 1,
            Size::B16 => 2,
            Size::B32 => 4,
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn compile_fun(fun: &Fun) -> String {
        let mut compiler = Compiler {
            stack_slots: vec![],
            stack_len: 0,
            free_temp_regs: TEMP_REGS.iter().copied().collect(),
            builder: AsmBuilder::new(),
            fun,
        };
        for block_id in fun.blocks() {
            compiler.compile_block(block_id, fun.get_block(block_id));
        }
        compiler.builder.output()
    }
    pub fn compile_block(&mut self, block_id: BlockId, block: &Block) {
        self.builder.insert_label(Label(block_id.id()));
        for stmt in &block.stmts {
            self.compile_stmt(stmt);
        }
        match &block.branch {
            Branch::End => {},
            Branch::Static(block_id) => {
                self.builder.push_inst(Inst::J { op: OpcodeJ::J, label: Label(block_id.id()) })
            }
            Branch::Bool { expr, if_true, if_false } => {
                let value = self.compile_expr(expr);
                let reg = match value {
                    Value::Bool(reg) => reg,
                    _ => panic!(),
                };
                self.builder.push_inst(Inst::ILabel { op: OpcodeI::BNE, rs: Reg::TempReg(reg), rt: Reg::Zero, label: Label(if_true.id()) });
                self.builder.push_inst(Inst::J { op: OpcodeJ::J, label: Label(if_false.id()) })
            }
            Branch::Comparison { a, b, cmp, if_true, if_false } => {
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
                    Compare::LessThan => (a_reg, b_reg),
                    Compare::GreaterThan => (b_reg, a_reg),
                };
                let funct = match a_ty.signedness {
                    Signedness::Signed => Funct::SUB,
                    Signedness::Unsigned => Funct::SUBU,
                };
                self.builder.push_inst(Inst::R {
                    funct,
                    rd: Reg::TempReg(reg),
                    rs: Reg::TempReg(rs),
                    rt: Reg::TempReg(rt),
                    shamt: 0,
                });
                self.builder.push_inst(Inst::ILabel {
                    op: OpcodeI::BGTZ,
                    rs: Reg::TempReg(reg),
                    rt: Reg::Zero,
                    label: Label(if_true.id()),
                });
                self.builder.push_inst(Inst::J {
                    op: OpcodeJ::J,
                    label: Label(if_false.id()),
                })
            }
        }
    }
    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Alloc(ty_name) => {
                let ty = self.fun.get_ty(*ty_name).concrete(self.fun);
                let align = ty_align_bytes(ty);
                let padding = if self.stack_len % align == 0 { 0 } else { align - self.stack_len % align };
                self.stack_len += padding + ty_len_bytes(ty);
                self.stack_slots.push(self.stack_len);
            }
            Stmt::Drop => {
                self.stack_slots.pop();
                self.stack_len = *self.stack_slots.last().unwrap_or(&0);
            }
            Stmt::Assign { stack_slot, expr } => {
                let value = self.compile_expr(expr);
                match value {
                    Value::Bool(reg) => {
                        self.builder.push_inst(Inst::I {
                            op: OpcodeI::SB,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot as usize] as i16),
                        });
                        self.free_temp_reg(reg);
                    }
                    Value::Int { reg, ty } => {
                        let op = match ty.size {
                            Size::B8 => OpcodeI::SB,
                            Size::B16 => OpcodeI::SH,
                            Size::B32 => OpcodeI::SW,
                        };
                        self.builder.push_inst(Inst::I {
                            op,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot as usize] as i16),
                        });
                        self.free_temp_reg(reg);
                    }
                    Value::None => panic!(),
                }
            }
            Stmt::Return(expr) => {
                let value = self.compile_expr(expr);
                match value {
                    Value::None => {},
                    Value::Bool(reg) | Value::Int { reg, .. } => {
                        self.builder.push_inst(Inst::R {
                            funct: Funct::OR,
                            rd: Reg::ValReg(ValReg::V0),
                            rt: Reg::TempReg(reg),
                            rs: Reg::Zero,
                            shamt: 0,
                        });
                    }
                }
                self.builder.push_inst(Inst::R {
                    funct: Funct::JR,
                    rs: Reg::RA,
                    rd: Reg::Zero,
                    rt: Reg::Zero,
                    shamt: 0,
                })
            }
        }
    }
    fn compile_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Int { value, ty } => {
                let reg = self.alloc_temp_reg();
                let int_ty = self.fun.get_int_ty(*ty).concrete(self.fun);
                let op = match int_ty.signedness {
                    Signedness::Signed => OpcodeI::ADDI,
                    Signedness::Unsigned => OpcodeI::ORI,
                };
                self.builder.push_inst(Inst::I {
                    op,
                    rt: Reg::TempReg(reg),
                    rs: Reg::Zero,
                    imm: *value as i16,
                });
                Value::Int { ty: int_ty, reg }
            }
            Expr::Bool(value) => {
                let reg = self.alloc_temp_reg();
                self.builder.push_inst(Inst::I {
                    op: OpcodeI::ORI,
                    imm: if *value { 1 } else { 0 },
                    rt: Reg::TempReg(reg),
                    rs: Reg::Zero,
                });
                Value::Bool(reg)
            }
            Expr::Load { stack_slot, ty } => {
                match self.fun.get_ty(*ty).concrete(self.fun) {
                    ConcreteTy::Bool => {
                        let reg = self.alloc_temp_reg();
                        self.builder.push_inst(Inst::I {
                            op: OpcodeI::LB,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot as usize] as i16),
                        });
                        Value::Bool(reg)
                    }
                    ConcreteTy::Int(int) => {
                        let reg = self.alloc_temp_reg();
                        let op = match (int.signedness, int.size) {
                            (Signedness::Signed, Size::B8) => OpcodeI::LB,
                            (Signedness::Signed, Size::B16) => OpcodeI::LH,
                            (Signedness::Signed, Size::B32) => OpcodeI::LW,
                            (Signedness::Unsigned, Size::B8) => OpcodeI::LBU,
                            (Signedness::Unsigned, Size::B16) => OpcodeI::LHU,
                            (Signedness::Unsigned, Size::B32) => OpcodeI::LW,
                        };
                        self.builder.push_inst(Inst::I {
                            op,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot as usize] as i16),
                        });
                        Value::Int { reg, ty: int }
                    }
                    ConcreteTy::None => Value::None,
                }
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
                let funct = match (op, left_ty.signedness) {
                    (BinaryOp::Add, Signedness::Signed) => Funct::ADD,
                    (BinaryOp::Add, Signedness::Unsigned) => Funct::ADDU,
                    (BinaryOp::Subtract, Signedness::Signed) => Funct::SUB,
                    (BinaryOp::Subtract, Signedness::Unsigned) => Funct::SUBU,
                    
                    (BinaryOp::Multiply, Signedness::Signed) => todo!(),
                    (BinaryOp::Multiply, Signedness::Unsigned) => todo!(),
                    (BinaryOp::Divide, Signedness::Signed) => todo!(),
                    (BinaryOp::Divide, Signedness::Unsigned) => todo!(),
                };
                let reg = self.alloc_temp_reg();
                self.builder.push_inst(Inst::R {
                    funct,
                    rd: Reg::TempReg(reg),
                    rs: Reg::TempReg(left_reg),
                    rt: Reg::TempReg(right_reg),
                    shamt: 0,
                });

                self.free_temp_reg(left_reg);
                self.free_temp_reg(right_reg);

                Value::Int { reg, ty: left_ty }
            }
        }
    }
    fn alloc_temp_reg(&mut self) -> TempReg {
        self.free_temp_regs.pop().unwrap()
    }
    fn free_temp_reg(&mut self, reg: TempReg) {
        self.free_temp_regs.push(reg);
    }
}