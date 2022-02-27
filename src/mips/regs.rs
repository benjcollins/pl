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

impl Reg {
    pub fn name(&self) -> &str {
        match self {
            Reg::Zero => "$zero",
            Reg::ValReg(reg) => match reg {
                ValReg::V0 => "$v0",
                ValReg::V1 => "$v1",
            }
            Reg::ArgReg(reg) => match reg {
                ArgReg::A0 => "$a0",
                ArgReg::A1 => "$a1",
                ArgReg::A2 => "$a2",
                ArgReg::A3 => "$a3",
            }
            Reg::TempReg(reg) => match reg {
                TempReg::T0 => "$t0",
                TempReg::T1 => "$t1",
                TempReg::T2 => "$t2",
                TempReg::T3 => "$t3",
                TempReg::T4 => "$t4",
                TempReg::T5 => "$t5",
                TempReg::T6 => "$t6",
                TempReg::T7 => "$t7",
                TempReg::T8 => "$t8",
                TempReg::T9 => "$t9",
            }
            Reg::SavedReg(reg) => match reg {
                SavedReg::S0 => "$s0",
                SavedReg::S1 => "$s1",
                SavedReg::S2 => "$s2",
                SavedReg::S3 => "$s3",
                SavedReg::S4 => "$s4",
                SavedReg::S5 => "$s5",
                SavedReg::S6 => "$s6",
                SavedReg::S7 => "$s7",
            }
            Reg::GP => "$gp",
            Reg::SP => "$sp",
            Reg::FP => "$fp",
            Reg::RA => "$ra",
        }
    }
}