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

pub enum Inst {
    R {
        rs: Reg,
        rt: Reg,
        rd: Reg,
        funct: Funct,
        shamt: u8,
    },
    J {
        op: OpcodeJ,
        label: Label,
    },
    I {
        op: OpcodeI,
        rs: Reg,
        rt: Reg,
        imm: i16,
    },
}

pub struct Label(u32);

impl Funct {
    fn asm_format(&self) -> &str {
        match self {
            Funct::SLL => "sll $rd, $rt, shamt\n",
            Funct::SRL => "srl $rd, $rt, shamt\n",
            Funct::SRA => "sra $rd, $rt, shamt\n",
            Funct::SLLV => "sllv $rd, $rt, $rs\n",
            Funct::SRLV => "srlv $rd, $rt, $rs\n",
            Funct::SRAV => "srav $rd, $rt, $rs\n",
            Funct::JR => "jr $rs\n",
            Funct::JALR => "jalr $rd, $rs\n",
            Funct::SYSCALL => "syscall\n",
            Funct::MFHI => "mfhi $rd\n",
            Funct::MTHI => "mthi $rs\n",
            Funct::MFLO => "mflo $rd\n",
            Funct::MTLO => "mtlo $rs\n",
            Funct::MULT => "mult $rs, $rt\n",
            Funct::MULTU => "multu $rs, $rt\n",
            Funct::DIV => "div $rs, $rt\n",
            Funct::DIVU => "divu $rs, $rt\n",
            Funct::ADD => "add $rd, $rs, $rt\n",
            Funct::ADDU => "addu $rd, $rs, $rt\n",
            Funct::SUB => "sub $rd, $rs, $rt\n",
            Funct::SUBU => "subu $rd, $rs, $rt\n",
            Funct::AND => "and $rd, $rs, $rt\n",
            Funct::OR => "or $rd, $rs, $rt\n",
            Funct::XOR => "xor $rd, $rs, $rt\n",
            Funct::NOR => "nor $rd, $rs, $rt\n",
            Funct::SLT => "slt $rd, $rs, $rt\n",
            Funct::SLTU => "sltu $rd, $rs, $rt\n",
        }
    }
}

impl OpcodeJ {
    fn asm_format(&self) -> &str {
        match self {
            OpcodeJ::J => "j address\n",
            OpcodeJ::JAL => "jal address\n",
        }
    }
}

impl OpcodeI {
    fn asm_format(&self) -> &str {
        match self {
            OpcodeI::BEQ => "beq $rs, $rt, imm\n",
            OpcodeI::BNE => "bne $rs, $rt, imm\n",
            OpcodeI::BLEZ => "blez $rs, imm\n",
            OpcodeI::BGTZ => "bgtz $rs, imm\n",
            OpcodeI::ADDI => "addi $rt, $rs, imm\n",
            OpcodeI::ADDIU => "addiu $rt, $rs, imm\n",
            OpcodeI::SLTI => "slti $rt, $rs, imm\n",
            OpcodeI::SLTIU => "sltiu $rt, $rs, imm\n",
            OpcodeI::ANDI => "andi $rt, $rs, imm\n",
            OpcodeI::ORI => "ori $rt, $rs, imm\n",
            OpcodeI::XORI => "xori $rt, $rs, imm\n",
            OpcodeI::LUI => "lui $rt, imm\n",
            OpcodeI::LB => "lb $rt, imm($rs)\n",
            OpcodeI::LH => "lh $rt, imm($rs)\n",
            OpcodeI::LW => "lw $rt, imm($rs)\n",
            OpcodeI::LBU => "lbu $rt, imm($rs)\n",
            OpcodeI::LHU => "lhu $rt, imm($rs)\n",
            OpcodeI::SB => "sb $rt, imm($rs)\n",
            OpcodeI::SH => "sh $rt, imm($rs)\n",
            OpcodeI::SW => "sw $rt, imm($rs)\n",
        }
    }
}

impl Inst {
    pub fn asm(&self) -> String {
        match self {
            Inst::R { rs, rt, rd, funct, shamt } => {
                funct.asm_format().to_string()
                    .replace("$rs", rs.name())
                    .replace("$rt", rt.name())
                    .replace("$rd", rd.name())
                    .replace("shamt", &shamt.to_string())
            }
            Inst::J { op, label } => {
                op.asm_format().to_string()
                    .replace("address", &format!("l{}", label.0))
            }
            Inst::I { op, rs, rt, imm } => {
                op.asm_format().to_string()
                    .replace("$rs", rs.name())
                    .replace("$rt", rt.name())
                    .replace("imm", &imm.to_string())
            }
        }
    }
}