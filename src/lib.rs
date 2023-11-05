mod instr;

use instr::opcode::*;
use instr::source;
use instr::source::*;
use instr::DisasemblerError;
use instr::DisasemblerError::*;
use instr::Instruction;

#[rustfmt::skip]
static opcode_main: [Result<instr::opcode,DisasemblerError>; 64] = [
    Err(Lookup), Err(Lookup), Ok(J),       Ok(JAL),   Ok(BEQ),  Ok(BNE),  Ok(BLEZ),  Ok(BGTZ),
    Ok(ADDI),    Ok(ADDIU),   Ok(SLTI),    Ok(SLTIU), Ok(ANDI), Ok(ORI),  Ok(XORI),  Ok(LUI),
    Err(Lookup), Err(Lookup), Err(Lookup), Err(RIE),  Ok(BEQL), Ok(BNEL), Ok(BLEZL), Ok(BGTZL), 
    Ok(DADDI),   Ok(DADDIU),  Ok(LDL),     Ok(LDR),   Err(RIE), Err(RIE), Err(RIE),  Err(RIE), 
    Ok(LB),      Ok(LH),      Ok(LWL),     Ok(LW),    Ok(LBU),  Ok(LHU),  Ok(LWR),   Ok(LWU),
    Ok(SB),      Ok(SH),      Ok(SWL),     Ok(SW),    Ok(SDL),  Ok(SDR),  Ok(SWR),   Ok(CACHE),
    Ok(LL),      Ok(LWC1),    Ok(LWC2),    Ok(LLD),   Err(RIE), Ok(LDC1), Ok(LDC2),  Ok(LD),
    Ok(SC),      Ok(SWC1),    Ok(SWC2),    Ok(SCD),   Err(RIE), Ok(SDC1), Ok(SDC2),  Ok(SD),
];

#[rustfmt::skip]
static special_lookup: [Result<instr::opcode,DisasemblerError>;64] = [
    Ok(SLL),  Err(RIE),  Ok(SRL),  Ok(SRA),  Ok(SLLV),    Err(RIE),   Ok(SRLV),   Ok(SRAV),
    Ok(JR),   Ok(JALR),  Err(RIE), Err(RIE), Ok(SYSCALL), Ok(BREAK),  Err(RIE),   Ok(SYNC),
    Ok(MFHI), Ok(MTHI),  Ok(MFLO), Ok(MTLO), Ok(DSLLV),   Err(RIE),   Ok(DSRLV),  Ok(DSRAV),
    Ok(MULT), Ok(MULTU), Ok(DIV),  Ok(DIVU), Ok(DMULT),   Ok(DMULTU), Ok(DDIV),   Ok(DDIVU),
    Ok(ADD),  Ok(ADDU),  Ok(SUB),  Ok(SUBU), Ok(AND),     Ok(OR),     Ok(XOR),    Ok(NOR),
    Err(RIE), Err(RIE),  Ok(SLT),  Ok(SLTU), Ok(DADD),    Ok(DADDU),  Ok(DSUB),   Ok(DSUBU),
    Ok(TGE),  Ok(TGEU),  Ok(TLT),  Ok(TLTU), Ok(TEQ),     Err(RIE),   Ok(TNE),    Err(RIE),
    Ok(DSLL), Err(RIE),  Ok(DSRL), Ok(DSRA), Ok(DSLL32),  Err(RIE),   Ok(DSRL32), Ok(DSRA32),
];

#[rustfmt::skip]
static regimm_lookup: [Result<instr::opcode,DisasemblerError>;32] = [
    Ok(BLTZ),   Ok(BGEZ),   Ok(BLTZI),   Ok(BGEZL),   Err(RIE), Err(RIE), Err(RIE), Err(RIE),
    Ok(TGEI),   Ok(TGEIU),  Ok(TLTI),    Ok(TLTIU),   Ok(TEQI), Err(RIE), Ok(TNEI), Err(RIE),
    Ok(BLTZAL), Ok(BGEZAL), Ok(BLTZALL), Ok(BGEZALL), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
    Err(RIE),   Err(RIE),   Err(RIE),    Err(RIE),    Err(RIE), Err(RIE), Err(RIE), Err(RIE),
];

#[rustfmt::skip]
static coprs_lookup: [Result<instr::opcode,DisasemblerError>;32] = [
Ok(MF), Ok(DMF),  Ok(CF),   Err(RIE), Ok(MT),   Ok(DMT),  Ok(CT),   Err(RIE),
Ok(BC), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
Ok(CO), Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),
Ok(CO), Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO),   Ok(CO), 
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
