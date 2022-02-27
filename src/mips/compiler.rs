use std::collections::HashSet;

use crate::{mir::{Block, Stmt, Expr, ArithmeticOp}, ty::{Ty, Size, Int, Signedness, AtomicTy}, mips::{inst::{Inst, OpcodeI, Funct}, regs::Reg}};

use super::regs::{TempReg, TEMP_REGS, ValReg};

pub struct Compiler {
    stack_slots: Vec<u16>,
    stack_len: u16,
    free_temp_regs: HashSet<TempReg>,
    output: String,
}

pub enum Value {
    Int {
        reg: TempReg,
        ty: Int,
    },
    Bool(TempReg),
    None,
}

fn ty_len_bytes(ty: &Ty) -> u16 {
    match ty {
        Ty::Atomic(ty) => match ty {
            AtomicTy::Int(int) => match int.size {
                Size::B8 => 1,
                Size::B16 => 2,
                Size::B32 => 4,
            }
            AtomicTy::None => 0,
            AtomicTy::Bool => 1,
        }
    }
}

fn ty_align_bytes(ty: &Ty) -> u16 {
    match ty {
        Ty::Atomic(ty) => match ty {
            AtomicTy::Int(int) => match int.size {
                Size::B8 => 1,
                Size::B16 => 2,
                Size::B32 => 4,
            }
            AtomicTy::None => 0,
            AtomicTy::Bool => 1,
        }
    }
}

impl Compiler {
    pub fn compile_fun(block: &Block) -> String {
        let mut compiler = Compiler {
            output: String::new(),
            stack_slots: vec![],
            stack_len: 0,
            free_temp_regs: HashSet::from_iter(TEMP_REGS.iter().copied())
        };
        compiler.compile_block(block);
        compiler.output
    }
    pub fn append_inst(&mut self, inst: Inst) {
        self.output.push_str(&inst.asm());
    }
    pub fn compile_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
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
                Stmt::Assign { stack_slot, expr, .. } => {
                    let value = self.compile_expr(expr);
                    match value {
                        Value::Bool(reg) => {
                            self.append_inst(Inst::I {
                                op: OpcodeI::SB,
                                rt: Reg::TempReg(reg),
                                rs: Reg::FP,
                                imm: -(self.stack_slots[*stack_slot] as i16),
                            });
                            self.free_temp_reg(reg);
                        }
                        Value::Int { reg, ty } => {
                            let op = match ty.size {
                                Size::B8 => OpcodeI::SB,
                                Size::B16 => OpcodeI::SH,
                                Size::B32 => OpcodeI::SW,
                            };
                            self.append_inst(Inst::I {
                                op,
                                rt: Reg::TempReg(reg),
                                rs: Reg::FP,
                                imm: -(self.stack_slots[*stack_slot] as i16),
                            });
                            self.free_temp_reg(reg);
                        }
                        Value::None => panic!(),
                    }
                }
                Stmt::Return { expr, .. } => {
                    let value = self.compile_expr(expr);
                    match value {
                        Value::None => {},
                        Value::Bool(reg) | Value::Int { reg, .. } => {
                            self.append_inst(Inst::R {
                                funct: Funct::OR,
                                rd: Reg::ValReg(ValReg::V0),
                                rt: Reg::TempReg(reg),
                                rs: Reg::Zero,
                                shamt: 0,
                            });
                        }
                    }
                    self.append_inst(Inst::R {
                        funct: Funct::JR,
                        rs: Reg::RA,
                        rd: Reg::Zero,
                        rt: Reg::Zero,
                        shamt: 0,
                    })
                }
            }
        }
    }
    pub fn compile_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Int { value, ty } => {
                let reg = self.alloc_temp_reg();
                let op = match ty.signedness {
                    Signedness::Signed => OpcodeI::ADDI,
                    Signedness::Unsigned => OpcodeI::ORI,
                };
                self.append_inst(Inst::I {
                    op,
                    rt: Reg::TempReg(reg),
                    rs: Reg::Zero,
                    imm: *value as i16,
                });
                Value::Int { ty: *ty, reg }
            }
            Expr::Bool(value) => {
                let reg = self.alloc_temp_reg();
                self.append_inst(Inst::I {
                    op: OpcodeI::ORI,
                    imm: if *value { 1 } else { 0 },
                    rt: Reg::TempReg(reg),
                    rs: Reg::Zero,
                });
                Value::Bool(reg)
            }
            Expr::Load { stack_slot, ty } => {
                match ty {
                    Ty::Atomic(AtomicTy::Bool) => {
                        let reg = self.alloc_temp_reg();
                        self.append_inst(Inst::I {
                            op: OpcodeI::LB,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot] as i16),
                        });
                        Value::Bool(reg)
                    }
                    Ty::Atomic(AtomicTy::Int(ty)) => {
                        let reg = self.alloc_temp_reg();
                        let op = match (ty.signedness, ty.size) {
                            (Signedness::Signed, Size::B8) => OpcodeI::LB,
                            (Signedness::Signed, Size::B16) => OpcodeI::LH,
                            (Signedness::Signed, Size::B32) => OpcodeI::LW,
                            (Signedness::Unsigned, Size::B8) => OpcodeI::LBU,
                            (Signedness::Unsigned, Size::B16) => OpcodeI::LHU,
                            (Signedness::Unsigned, Size::B32) => OpcodeI::LW,
                        };
                        self.append_inst(Inst::I {
                            op,
                            rt: Reg::TempReg(reg),
                            rs: Reg::FP,
                            imm: -(self.stack_slots[*stack_slot] as i16),
                        });
                        Value::Int { reg, ty: *ty }
                    }
                    Ty::Atomic(AtomicTy::None) => Value::None,
                }
            }
            Expr::Arithmetic { left, right, ty, op } => {
                let (left_reg, left_ty) = match self.compile_expr(left) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                let (right_reg, right_ty) = match self.compile_expr(right) {
                    Value::Int { reg, ty } => (reg, ty),
                    _ => panic!(),
                };
                assert_eq!(&left_ty, ty);
                assert_eq!(&right_ty, ty);
                let funct = match (op, left_ty.signedness) {
                    (ArithmeticOp::Add, Signedness::Signed) => Funct::ADD,
                    (ArithmeticOp::Add, Signedness::Unsigned) => Funct::ADDU,
                    (ArithmeticOp::Subtract, Signedness::Signed) => Funct::SUB,
                    (ArithmeticOp::Subtract, Signedness::Unsigned) => Funct::SUBU,
                    
                    (ArithmeticOp::Multiply, Signedness::Signed) => todo!(),
                    (ArithmeticOp::Multiply, Signedness::Unsigned) => todo!(),
                    (ArithmeticOp::Divide, Signedness::Signed) => todo!(),
                    (ArithmeticOp::Divide, Signedness::Unsigned) => todo!(),
                };
                let reg = self.alloc_temp_reg();
                self.append_inst(Inst::R {
                    funct,
                    rd: Reg::TempReg(reg),
                    rs: Reg::TempReg(left_reg),
                    rt: Reg::TempReg(right_reg),
                    shamt: 0,
                });

                self.free_temp_reg(left_reg);
                self.free_temp_reg(right_reg);

                Value::Int { reg, ty: *ty }
            }
        }
    }
    fn alloc_temp_reg(&mut self) -> TempReg {
        let reg = *self.free_temp_regs.iter().next().unwrap();
        self.free_temp_regs.remove(&reg);
        reg
    }
    fn free_temp_reg(&mut self, reg: TempReg) {
        self.free_temp_regs.insert(reg);
    }
}