mod instr;

use instr::opcode::*;
use instr::source;
use instr::source::*;
use instr::Cpu;
use instr::DisasemblerError;
use instr::DisasemblerError::*;
use instr::Instruction;
use instr::InstructionFormat;
use instr::InstructionFormat::*;
use instr::GPR;

use crate::instr::dest;

#[rustfmt::skip]
static opcode_main: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;64],usize,usize) = ([
    Err(Lookup64(&special_lookup)), Err(Lookup32(&regimm_lookup)),  Ok((J,J_t)),                  Ok((JAL,J_t)),   Ok((BEQ,I_t)),  Ok((BNE,I_t)),  Ok((BLEZ,I_t)),  Ok((BGTZ,I_t)),
    Ok((ADDI,I_t)),                 Ok((ADDIU,I_t)),                Ok((SLTI,I_t)),               Ok((SLTIU,I_t)), Ok((ANDI,I_t)), Ok((ORI,I_t)),  Ok((XORI,I_t)),  Ok((LUI,I_t)),
    Err(Lookup32(&coprs_lookup)),   Err(Lookup32(&coprs_lookup)),   Err(Lookup32(&coprs_lookup)), Err(RIE),        Ok((BEQL,I_t)), Ok((BNEL,I_t)), Ok((BLEZL,I_t)), Ok((BGTZL,I_t)), 
    Ok((DADDI,I_t)),                Ok((DADDIU,I_t)),               Ok((LDL,I_t)),                Ok((LDR,I_t)),   Err(RIE),       Err(RIE),       Err(RIE),        Err(RIE), 
    Ok((LB,I_t)),                   Ok((LH,I_t)),                   Ok((LWL,I_t)),                Ok((LW,I_t)),    Ok((LBU,I_t)),  Ok((LHU,I_t)),  Ok((LWR,I_t)),   Ok((LWU,I_t)),
    Ok((SB,I_t)),                   Ok((SH,I_t)),                   Ok((SWL,I_t)),                Ok((SW,I_t)),    Ok((SDL,I_t)),  Ok((SDR,I_t)),  Ok((SWR,I_t)),   Ok((CACHE,I_t)),
    Ok((LL,I_t)),                   Ok((LWC1,I_t)),                 Ok((LWC2,I_t)),               Ok((LLD,I_t)),   Err(RIE),       Ok((LDC1,I_t)), Ok((LDC2,I_t)),  Ok((LD,I_t)),
    Ok((SC,I_t)),                   Ok((SWC1,I_t)),                 Ok((SWC2,I_t)),               Ok((SCD,I_t)),   Err(RIE),       Ok((SDC1,I_t)), Ok((SDC2,I_t)),  Ok((SD,I_t)),
],
0b1111_1100_0000_0000_0000_0000_0000_0000,
26);

#[rustfmt::skip]
static special_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;64],usize,usize) = ([
    Ok((SLL,R_t)),  Err(RIE),        Ok((SRL,R_t)),  Ok((SRA,R_t)),   Ok((SLLV,R_t)),    Err(RIE),          Ok((SRLV,R_t)),   Ok((SRAV,R_t)),
    Ok((JR,R_t)),   Ok((JALR,R_t)),  Err(RIE),       Err(RIE),        Ok((SYSCALL,R_t)), Ok((BREAK,R_t)),   Err(RIE),         Ok((SYNC,R_t)),
    Ok((MFHI,R_t)), Ok((MTHI,R_t)),  Ok((MFLO,R_t)), Ok((MTLO,R_t)),  Ok((DSLLV,R_t)),   Err(RIE),          Ok((DSRLV,R_t)),  Ok((DSRAV,R_t)),
    Ok((MULT,R_t)), Ok((MULTU,R_t)), Ok((DIV,R_t)),  Ok((DIVU,R_t)),  Ok((DMULT,R_t)),   Ok((DMULTU,R_t)),  Ok((DDIV,R_t)),   Ok((DDIVU,R_t)),
    Ok((ADD,R_t)),  Ok((ADDU,R_t)),  Ok((SUB,R_t)),  Ok((SUBU,R_t)),  Ok((AND,R_t)),     Ok((OR,R_t)),      Ok((XOR,R_t)),    Ok((NOR,R_t)),
    Err(RIE),       Err(RIE),        Ok((SLT,R_t)),  Ok((SLTU,R_t)),  Ok((DADD,R_t)),    Ok((DADDU,R_t)),   Ok((DSUB,R_t)),   Ok((DSUBU,R_t)),
    Ok((TGE,R_t)),  Ok((TGEU,R_t)),  Ok((TLT,R_t)),  Ok((TLTU,R_t)),  Ok((TEQ,R_t)),     Err(RIE),          Ok((TNE,R_t)),    Err(RIE),
    Ok((DSLL,R_t)), Err(RIE),        Ok((DSRL,R_t)), Ok((DSRA,R_t)),  Ok((DSLL32,R_t)),  Err(RIE),          Ok((DSRL32,R_t)), Ok((DSRA32,R_t)),
],
0b0000_0000_0000_0000_0000_0000_0011_1111,
0);

#[rustfmt::skip]
static regimm_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize) = ([
    Ok((BLTZ,I_t)),   Ok((BGEZ,I_t)),   Ok((BLTZI,I_t)),   Ok((BGEZL,I_t)),   Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Ok((TGEI,I_t)),   Ok((TGEIU,I_t)),  Ok((TLTI,I_t)),    Ok((TLTIU,I_t)),   Ok((TEQI,I_t)), Err(RIE), Ok((TNEI,I_t)), Err(RIE),
    Ok((BLTZAL,I_t)), Ok((BGEZAL,I_t)), Ok((BLTZALL,I_t)), Ok((BGEZALL,I_t)), Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Err(RIE),         Err(RIE),         Err(RIE),          Err(RIE),          Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
static coprs_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize)= ([
    Ok((MF,R_t)),   Ok((DMF,R_t)),    Ok((CF,R_t)),     Err(RIE),       Ok((MT,R_t)),   Ok((DMT,R_t)),    Ok((CT,R_t)),     Err(RIE),
    Ok((BC,I_t)),   Err(RIE),         Err(RIE),         Err(RIE),       Err(RIE),       Err(RIE),         Err(RIE),         Err(RIE),
    Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)), Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)),
    Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)), Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)), 
],
0b0000_0011_1110_0000_0000_0000_0000_0000,
21);

#[rustfmt::skip]
static coprt_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize) = ([
	Ok((BCF,I_t)),  Ok((BCT,I_t)),  Ok((BCFL,I_t)), Ok((BCTL,I_t)), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
static copzero_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;64],usize,usize) = ([
    Err(InvOp),     Ok((TLBR,cop)), Ok((TLBWI,cop)), Err(InvOp), Err(InvOp), Err(InvOp), Ok((TLBWR,cop)), Err(InvOp),
    Ok((TLBP,cop)), Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Err(RIE),       Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Ok((ERET,cop)), Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Err(InvOp),     Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Err(InvOp),     Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Err(InvOp),     Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
    Err(InvOp),     Err(InvOp),     Err(InvOp),      Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),      Err(InvOp),
],
0b0000_0000_0000_0000_0000_0000_0011_1111,
0);

pub fn decode(raw: u32) -> Instruction<Cpu> {
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

    let mut op: &Result<(instr::opcode, InstructionFormat), DisasemblerError> =
        &Err(Lookup64(&opcode_main));
    while op.as_ref().is_err_and(|x| *x != RIE && *x != InvOp) {
        match op.as_ref().unwrap_err() {
            Lookup32(x) => {
                op = &x.0[((raw as usize & x.1) >> x.2) as usize];
            }
            Lookup64(x) => {
                op = &x.0[((raw as usize & x.1) >> x.2) as usize];
            }
            _ => unreachable!("?"),
        }
    }

    //println!("final val after lookups: {:?}", op);

    if op.is_err() {
        panic!("decoded invalid opcode: {:?}, bytes: {:#x}", op, raw);
    };

    let (dest, sources): (Option<dest>, [Option<source>; 3]) = match op.as_ref().unwrap().1 {
        I_t => (
            Some(dest::GPR(GPR::from(i_op_rt))),
            [
                Some(source::GPR(GPR::from(i_op_rs))),
                Some(source::IMM(i_op_imm.into())),
                None,
            ],
        ),
        J_t => (None, [Some(source::IMM(j_op_imm.into())), None, None]),
        R_t => (
            Some(dest::GPR(GPR::from(r_op_rd))),
            [
                Some(source::GPR(GPR::from(r_op_rs))),
                Some(source::GPR(GPR::from(r_op_rt))),
                Some(source::IMM(r_op_shamt.into())),
            ],
        ),
        cop => panic!("cop opcodes arent supported yet"),
    };

    return Instruction {
        bytes,
        opcode: op.as_ref().unwrap().0,
        sources,
        dest,
        operation: Box::new(|_bleh: &mut Cpu| {}),
        //operation: test,
        machine_code: Vec::new(),
    };
}

pub fn test(cpu: &mut Cpu) {}
