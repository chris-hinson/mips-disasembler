mod instr;

use instr::opcode::*;
use instr::source;
use instr::source::*;
use instr::DisasemblerError;
use instr::DisasemblerError::*;
use instr::Instruction;
use instr::InstructionFormat;
use instr::InstructionFormat::*;

#[rustfmt::skip]
static opcode_main: [Result<(instr::opcode,InstructionFormat),DisasemblerError>; 64] = [
    Err(Lookup),     Err(Lookup),      Ok((J,J_t)),    Ok((JAL,J_t)),   Ok((BEQ,I_t)),  Ok((BNE,I_t)),  Ok((BLEZ,I_t)),  Ok((BGTZ,I_t)),
    Ok((ADDI,I_t)),  Ok((ADDIU,I_t)),  Ok((SLTI,I_t)), Ok((SLTIU,I_t)), Ok((ANDI,I_t)), Ok((ORI,I_t)),  Ok((XORI,I_t)),  Ok((LUI,I_t)),
    Err(Lookup),     Err(Lookup),      Err(Lookup),    Err(RIE),        Ok((BEQL,I_t)), Ok((BNEL,I_t)), Ok((BLEZL,I_t)), Ok((BGTZL,I_t)), 
    Ok((DADDI,I_t)), Ok((DADDIU,I_t)), Ok((LDL,I_t)),  Ok((LDR,I_t)),   Err(RIE),       Err(RIE),       Err(RIE),        Err(RIE), 
    Ok((LB,I_t)),    Ok((LH,I_t)),     Ok((LWL,I_t)),  Ok((LW,I_t)),    Ok((LBU,I_t)),  Ok((LHU,I_t)),  Ok((LWR,I_t)),   Ok((LWU,I_t)),
    Ok((SB,I_t)),    Ok((SH,I_t)),     Ok((SWL,I_t)),  Ok((SW,I_t)),    Ok((SDL,I_t)),  Ok((SDR,I_t)),  Ok((SWR,I_t)),   Ok((CACHE,I_t)),
    Ok((LL,I_t)),    Ok((LWC1,I_t)),   Ok((LWC2,I_t)), Ok((LLD,I_t)),   Err(RIE),       Ok((LDC1,I_t)), Ok((LDC2,I_t)),  Ok((LD,I_t)),
    Ok((SC,I_t)),    Ok((SWC1,I_t)),   Ok((SWC2,I_t)), Ok((SCD,I_t)),   Err(RIE),       Ok((SDC1,I_t)), Ok((SDC2,I_t)),  Ok((SD,I_t)),
];

#[rustfmt::skip]
static special_lookup: [Result<(instr::opcode,InstructionFormat),DisasemblerError>;64] = [
    Ok((SLL,R_t)),  Err(RIE),        Ok((SRL,R_t)),  Ok((SRA,R_t)),   Ok((SLLV,R_t)),    Err(RIE),          Ok((SRLV,R_t)),   Ok((SRAV,R_t)),
    Ok((JR,R_t)),   Ok((JALR,R_t)),  Err(RIE),       Err(RIE),        Ok((SYSCALL,R_t)), Ok((BREAK,R_t)),   Err(RIE),         Ok((SYNC,R_t)),
    Ok((MFHI,R_t)), Ok((MTHI,R_t)),  Ok((MFLO,R_t)), Ok((MTLO,R_t)),  Ok((DSLLV,R_t)),   Err(RIE),          Ok((DSRLV,R_t)),  Ok((DSRAV,R_t)),
    Ok((MULT,R_t)), Ok((MULTU,R_t)), Ok((DIV,R_t)),  Ok((DIVU,R_t)),  Ok((DMULT,R_t)),   Ok((DMULTU,R_t)),  Ok((DDIV,R_t)),   Ok((DDIVU,R_t)),
    Ok((ADD,R_t)),  Ok((ADDU,R_t)),  Ok((SUB,R_t)),  Ok((SUBU,R_t)),  Ok((AND,R_t)),     Ok((OR,R_t)),      Ok((XOR,R_t)),    Ok((NOR,R_t)),
    Err(RIE),       Err(RIE),        Ok((SLT,R_t)),  Ok((SLTU,R_t)),  Ok((DADD,R_t)),    Ok((DADDU,R_t)),   Ok((DSUB,R_t)),   Ok((DSUBU,R_t)),
    Ok((TGE,R_t)),  Ok((TGEU,R_t)),  Ok((TLT,R_t)),  Ok((TLTU,R_t)),  Ok((TEQ,R_t)),     Err(RIE),          Ok((TNE,R_t)),    Err(RIE),
    Ok((DSLL,R_t)), Err(RIE),        Ok((DSRL,R_t)), Ok((DSRA,R_t)),  Ok((DSLL32,R_t)),  Err(RIE),          Ok((DSRL32,R_t)), Ok((DSRA32,R_t)),
];

#[rustfmt::skip]
static regimm_lookup: [Result<(instr::opcode,InstructionFormat),DisasemblerError>;32] = [
    Ok((BLTZ,I_t)),   Ok((BGEZ,I_t)),   Ok((BLTZI,I_t)),   Ok((BGEZL,I_t)),   Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Ok((TGEI,I_t)),   Ok((TGEIU,I_t)),  Ok((TLTI,I_t)),    Ok((TLTIU,I_t)),   Ok((TEQI,I_t)), Err(RIE), Ok((TNEI,I_t)), Err(RIE),
    Ok((BLTZAL,I_t)), Ok((BGEZAL,I_t)), Ok((BLTZALL,I_t)), Ok((BGEZALL,I_t)), Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Err(RIE),         Err(RIE),         Err(RIE),          Err(RIE),          Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
];

#[rustfmt::skip]
static coprs_lookup: [Result<(instr::opcode,InstructionFormat),DisasemblerError>;32] = [
    Ok((MF,R_t)), Ok((DMF,R_t)),  Ok((CF,R_t)),   Err(RIE),     Ok((MT,R_t)), Ok((DMT,R_t)),  Ok((CT,R_t)),   Err(RIE),
    Ok((BC,cop)), Err(RIE),       Err(RIE),       Err(RIE),     Err(RIE),     Err(RIE),       Err(RIE),       Err(RIE),
    Ok((CO,cop)), Ok((CO,cop)),   Ok((CO,cop)),   Ok((CO,cop)), Ok((CO,cop)), Ok((CO,cop)),   Ok((CO,cop)),   Ok((CO,cop)),
    Ok((CO,cop)), Ok((CO,cop)),   Ok((CO,cop)),   Ok((CO,cop)), Ok((CO,cop)), Ok((CO,cop)),   Ok((CO,cop)),   Ok((CO,cop)), 
];

#[rustfmt::skip]
static coprt_lookup: [Result<instr::opcode,DisasemblerError>;32] = [
Ok(BCF),  Ok(BCT),  Ok(BCFL), Ok(BCTL), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
];

#[rustfmt::skip]
static copzero_lookup: [Result<instr::opcode,DisasemblerError>;64] = [
    Err(InvOp), Ok(TLBR), Ok(TLBWI),Err(InvOp), Err(InvOp), Err(InvOp), Ok(TLBWR),Err(InvOp),
    Ok(TLBP), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Err(RIE), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Ok(ERET), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
    Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),
];

pub fn decode(raw: u32) -> Instruction {
    let bytes: [u8; 4] = raw.to_be_bytes();

    let opcode_bits = (raw & 0xFC00_0000) >> 26;
    let special_index = raw & 0x0000_003F;
    let regimm_index = (raw & 0x001F_0000) >> 16;
    let copzrs_index = (raw & 0x03E0_0000) >> 21;
    let copzrt_index = regimm_index;
    let cp0_index = special_index;

    //parse out all the fields
    let r_op_rs = ((raw & 0x03E0_0000) >> 21) as u8;
    let r_op_rt = ((raw & 0x001F_0000) >> 16) as u8;
    let r_op_rd = ((raw & 0x0000_F800) >> 11) as u8;
    let r_op_shamt = ((raw & 0x0000_07C0) >> 6) as u8;
    let r_sub_op = (raw & 0x0000_003F) as u8;

    let i_op_rs = ((raw & 0x03E0_0000) >> 21) as u8;
    let i_op_rt = ((raw & 0x001F_0000) >> 16) as u8;
    let i_op_imm = (raw & 0x0000_FFFF) as u16;

    let j_op_imm = raw & 0x007FF_FFFF;

    let (opcode, sources, dest) = match opcode_bits {
        0x00 => unimplemented!("SPECIAL encoding"),
        0x01 => unimplemented!("REGIMM encoding"),
        0x02 => (J, [Some(IMM(j_op_imm as u64)), None, None], None),
        0x03 => (JAL, [Some(IMM(j_op_imm as u64)), None, None], None),
        0x04 => (
            BEQ,
            [
                Some(GPR(i_op_rs.into())),
                Some(GPR(i_op_rt.into())),
                Some(IMM(i_op_imm as u64)),
            ],
            None,
        ),
        0x05 => (
            BNE,
            [
                Some(GPR(i_op_rs.into())),
                Some(GPR(i_op_rt.into())),
                Some(IMM(i_op_imm as u64)),
            ],
            None,
        ),
        0x06 => (
            BLEZ,
            [
                Some(GPR(i_op_rs.into())),
                Some(GPR(i_op_rt.into())),
                Some(IMM(i_op_imm as u64)),
            ],
            None,
        ),
        0x07 => (
            BGTZ,
            [
                Some(GPR(i_op_rs.into())),
                Some(GPR(i_op_rt.into())),
                Some(IMM(i_op_imm as u64)),
            ],
            None,
        ),
        _ => unreachable!("how."),
    };

    return Instruction {
        bytes,
        opcode,
        sources,
        dest,
    };
}
