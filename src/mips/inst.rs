use super::regs::Reg;

pub enum Funct {
    SLL = 0,
    SRL = 2,
    SRA = 3,
    SLLV = 4,
    SRLV = 6,
    SRAV = 7,
    JR = 8,
    JALR = 9,
    SYSCALL = 12,
    MFHI = 16,
    MTHI = 17,
    MFLO = 18,
    MTLO = 19,
    MULT = 24,
    MULTU = 25,
    DIV = 26,
    DIVU = 27,
    ADD = 32,
    ADDU = 33,
    SUB = 34,
    SUBU = 35,
    AND = 36,
    OR = 37,
    XOR = 38,
    NOR = 39,
    SLT = 42,
    SLTU = 43,
}

pub enum OpcodeJ {
    J = 2,
    JAL = 3,
}

pub enum OpcodeI {
    BEQ = 4,
    BNE = 5,
    BLEZ = 6,
    BGTZ = 7,
    ADDI = 8,
    ADDIU = 9,
    SLTI = 10,
    SLTIU = 11,
    ANDI = 12,
    ORI = 13,
    XORI = 14,
    LUI = 15,
    LB = 32,
    LH = 33,
    LW = 34,
    LBU = 35,
    LHU = 36,
    SB = 37,
    SH = 38,
    SW = 39,
}

pub enum Inst<L> {
    R {
        rs: Reg,
        rt: Reg,
        rd: Reg,
        funct: Funct,
        shamt: u8,
    },
    J {
        op: OpcodeJ,
        label: L,
    },
    I {
        op: OpcodeI,
        rs: Reg,
        rt: Reg,
        imm: i16,
    },
    ILabel {
        op: OpcodeI,
        rs: Reg,
        rt: Reg,
        label: L,
    }
}