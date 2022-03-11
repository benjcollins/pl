use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    Zero,
    
    ValReg(ValReg),
    ArgReg(ArgReg),
    TempReg(TempReg),
    SavedReg(SavedReg),

    GP,
    SP,
    FP,
    RA,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValReg {
    V0,
    V1,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArgReg {
    A0,
    A1,
    A2,
    A3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TempReg {
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SavedReg {
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::Zero => write!(f, "$zero"),
            Reg::ValReg(reg) => write!(f, "$v{}", *reg as u32),
            Reg::ArgReg(reg) => write!(f, "$a{}", *reg as u32),
            Reg::TempReg(reg) => write!(f, "$t{}", *reg as u32),
            Reg::SavedReg(reg) => write!(f, "$s{}", *reg as u32),
            Reg::GP => write!(f, "$gp"),
            Reg::SP => write!(f, "$sp"),
            Reg::FP => write!(f, "$fp"),
            Reg::RA => write!(f, "$ra"),
        }
    }
}

pub const TEMP_REGS: &[TempReg] = &[
    TempReg::T0,
    TempReg::T1,
    TempReg::T2,
    TempReg::T3,
    TempReg::T4,
    TempReg::T5,
    TempReg::T6,
    TempReg::T7,
    TempReg::T8,
    TempReg::T9,
];