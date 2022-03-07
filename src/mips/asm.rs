use super::inst::{Inst, OpcodeI, OpcodeJ, Funct};

pub struct AsmBuilder {
    output: String,
    label_count: u32,
}

pub struct Label(pub u32);

impl AsmBuilder {
    pub fn new() -> AsmBuilder {
        AsmBuilder { output: "".to_string(), label_count: 0 }
    }
    pub fn push_inst(&mut self, inst: Inst<Label>) {
        self.output.push_str(&format!("  {}\n", inst_to_asm(inst)))
    }
    pub fn insert_label(&mut self, label: Label) {
        self.output.push_str(&format!("l{}:\n", label.0))
    }
    pub fn output(self) -> String {
        self.output
    }
}

fn funct_asm_format(funct: Funct) -> &'static str {
    match funct {
        Funct::SLL => "sll $rd, $rt, shamt",
        Funct::SRL => "srl $rd, $rt, shamt",
        Funct::SRA => "sra $rd, $rt, shamt",
        Funct::SLLV => "sllv $rd, $rt, $rs",
        Funct::SRLV => "srlv $rd, $rt, $rs",
        Funct::SRAV => "srav $rd, $rt, $rs",
        Funct::JR => "jr $rs",
        Funct::JALR => "jalr $rd, $rs",
        Funct::SYSCALL => "syscall",
        Funct::MFHI => "mfhi $rd",
        Funct::MTHI => "mthi $rs",
        Funct::MFLO => "mflo $rd",
        Funct::MTLO => "mtlo $rs",
        Funct::MULT => "mult $rs, $rt",
        Funct::MULTU => "multu $rs, $rt",
        Funct::DIV => "div $rs, $rt",
        Funct::DIVU => "divu $rs, $rt",
        Funct::ADD => "add $rd, $rs, $rt",
        Funct::ADDU => "addu $rd, $rs, $rt",
        Funct::SUB => "sub $rd, $rs, $rt",
        Funct::SUBU => "subu $rd, $rs, $rt",
        Funct::AND => "and $rd, $rs, $rt",
        Funct::OR => "or $rd, $rs, $rt",
        Funct::XOR => "xor $rd, $rs, $rt",
        Funct::NOR => "nor $rd, $rs, $rt",
        Funct::SLT => "slt $rd, $rs, $rt",
        Funct::SLTU => "sltu $rd, $rs, $rt",
    }
}

fn opcode_j_asm_format(opcode: OpcodeJ) -> &'static str {
    match opcode {
        OpcodeJ::J => "j address",
        OpcodeJ::JAL => "jal address",
    }
}

fn opcode_i_asm_format(opcode: OpcodeI) -> &'static str {
    match opcode {
        OpcodeI::BEQ => "beq $rs, $rt, imm",
        OpcodeI::BNE => "bne $rs, $rt, imm",
        OpcodeI::BLEZ => "blez $rs, imm",
        OpcodeI::BGTZ => "bgtz $rs, imm",
        OpcodeI::ADDI => "addi $rt, $rs, imm",
        OpcodeI::ADDIU => "addiu $rt, $rs, imm",
        OpcodeI::SLTI => "slti $rt, $rs, imm",
        OpcodeI::SLTIU => "sltiu $rt, $rs, imm",
        OpcodeI::ANDI => "andi $rt, $rs, imm",
        OpcodeI::ORI => "ori $rt, $rs, imm",
        OpcodeI::XORI => "xori $rt, $rs, imm",
        OpcodeI::LUI => "lui $rt, imm",
        OpcodeI::LB => "lb $rt, imm($rs)",
        OpcodeI::LH => "lh $rt, imm($rs)",
        OpcodeI::LW => "lw $rt, imm($rs)",
        OpcodeI::LBU => "lbu $rt, imm($rs)",
        OpcodeI::LHU => "lhu $rt, imm($rs)",
        OpcodeI::SB => "sb $rt, imm($rs)",
        OpcodeI::SH => "sh $rt, imm($rs)",
        OpcodeI::SW => "sw $rt, imm($rs)",
    }
}

pub fn inst_to_asm(inst: Inst<Label>) -> String {
    match inst {
        Inst::R { rs, rt, rd, funct, shamt } => {
            funct_asm_format(funct).to_string()
                .replace("$rs", rs.name())
                .replace("$rt", rt.name())
                .replace("$rd", rd.name())
                .replace("shamt", &shamt.to_string())
        }
        Inst::J { op, label } => {
            opcode_j_asm_format(op).to_string()
                .replace("address", &format!("l{}", label.0))
        }
        Inst::I { op, rs, rt, imm } => {
            opcode_i_asm_format(op).to_string()
                .replace("$rs", rs.name())
                .replace("$rt", rt.name())
                .replace("imm", &imm.to_string())
        }
        Inst::ILabel { op, rs, rt, label } => {
            opcode_i_asm_format(op).to_string()
                .replace("$rs", rs.name())
                .replace("$rt", rt.name())
                .replace("imm", &format!("l{}", label.0))
        }
    }
}