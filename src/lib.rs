pub mod instr;
pub mod instr_impl;

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::sync::Arc;

use instr::opcode::*;
use instr::source;
use instr::Cpu;
use instr::DisasemblerError;
use instr::DisasemblerError::*;
use instr::Instruction;
use instr::InstructionFormat;
use instr::InstructionFormat::*;
use instr::GPR;
use instr::INSTR_WHICH_END_BASIC_BLOCK;

use instr_impl::*;

use crate::instr::dest;

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static opcode_main: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;64],usize,usize) = ([
    Err(Lookup64(&special_lookup)), Err(Lookup32(&regimm_lookup)),  Ok((J,J_t,j)),                Ok((JAL,J_t,jal)),     Ok((BEQ,I_t,beq)),   Ok((BNE,I_t,bne)),   Ok((BLEZ,I_t,blez)),   Ok((BGTZ,I_t,bgtz)),
    Ok((ADDI,I_t,addi)),            Ok((ADDIU,I_t,addiu)),          Ok((SLTI,I_t,slti)),          Ok((SLTIU,I_t,sltiu)), Ok((ANDI,I_t,andi)), Ok((ORI,I_t,ori)),   Ok((XORI,I_t,xori)),   Ok((LUI,I_t,lui)),
    Err(Lookup32(&coprs_lookup)),   Err(Lookup32(&coprs_lookup)),   Err(Lookup32(&coprs_lookup)), Err(RIE),              Ok((BEQL,I_t,beql)), Ok((BNEL,I_t,bnel)), Ok((BLEZL,I_t,blezl)), Ok((BGTZL,I_t,bgtzl)),
    Ok((DADDI,I_t,daddi)),          Ok((DADDIU,I_t,daddiu)),        Ok((LDL,I_t,ldl)),            Ok((LDR,I_t,ldr)),     Err(RIE),            Err(RIE),            Err(RIE),              Err(RIE),
    Ok((LB,I_t,lb)),                Ok((LH,I_t,lh)),                Ok((LWL,I_t,lwl)),            Ok((LW,I_t,lw)),       Ok((LBU,I_t,lbu)),   Ok((LHU,I_t,lhu)),   Ok((LWR,I_t,lwr)),     Ok((LWU,I_t,lwu)),
    Ok((SB,I_t,sb)),                Ok((SH,I_t,sh)),                Ok((SWL,I_t,swl)),            Ok((SW,I_t,sw)),       Ok((SDL,I_t,sdl)),   Ok((SDR,I_t,sdr)),   Ok((SWR,I_t,swr)),     Ok((CACHE,I_t,cache)),
    Ok((LL,I_t,ll)),                Ok((LWC1,I_t,lwc1)),            Ok((LWC2,I_t,lwc2)),          Ok((LLD,I_t,lld)),     Err(RIE),            Ok((LDC1,I_t,ldc1)), Ok((LDC2,I_t,ldc2)),   Ok((LD,I_t,ld)),
    Ok((SC,I_t,sc)),                Ok((SWC1,I_t,swc1)),            Ok((SWC2,I_t,swc2)),          Ok((SCD,I_t,scd)),     Err(RIE),            Ok((SDC1,I_t,sdc1)), Ok((SDC2,I_t,sdc2)),   Ok((SD,I_t,sd)),
],
0b1111_1100_0000_0000_0000_0000_0000_0000,
26);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static special_lookup: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;64],usize,usize) = ([
    Ok((SLL,R_t,sll)),   Err(RIE),              Ok((SRL,R_t,srl)),   Ok((SRA,R_t,sra)),    Ok((SLLV,R_t,sllv)),       Err(RIE),                Ok((SRLV,R_t,srlv)),     Ok((SRAV,R_t,srav)),
    Ok((JR,R_t,jr)),     Ok((JALR,R_t,jalr)),   Err(RIE),            Err(RIE),             Ok((SYSCALL,R_t,syscall)), Ok((BREAK,R_t,break_)),  Err(RIE),                Ok((SYNC,R_t,sync)),
    Ok((MFHI,R_t,mfhi)), Ok((MTHI,R_t,mthi)),   Ok((MFLO,R_t,mflo)), Ok((MTLO,R_t,mtlo)),  Ok((DSLLV,R_t,dsllv)),     Err(RIE),                Ok((DSRLV,R_t,dsrlv)),   Ok((DSRAV,R_t,dsrav)),
    Ok((MULT,R_t,mult)), Ok((MULTU,R_t,multu)), Ok((DIV,R_t,div)),   Ok((DIVU,R_t,divu)),  Ok((DMULT,R_t,dmult)),     Ok((DMULTU,R_t,dmultu)), Ok((DDIV,R_t,ddiv)),     Ok((DDIVU,R_t,ddivu)),
    Ok((ADD,R_t,add)),   Ok((ADDU,R_t,addu)),   Ok((SUB,R_t,sub)),   Ok((SUBU,R_t,subu)),  Ok((AND,R_t,and)),         Ok((OR,R_t,or)),         Ok((XOR,R_t,xor)),       Ok((NOR,R_t,nor)),
    Err(RIE),            Err(RIE),              Ok((SLT,R_t,slt)),   Ok((SLTU,R_t,sltu)),  Ok((DADD,R_t,dadd)),       Ok((DADDU,R_t,daddu)),   Ok((DSUB,R_t,dsub)),     Ok((DSUBU,R_t,dsubu)),
    Ok((TGE,R_t,tge)),   Ok((TGEU,R_t,tgeu)),   Ok((TLT,R_t,tlt)),   Ok((TLTU,R_t,tltu)),  Ok((TEQ,R_t,teq)),         Err(RIE),                Ok((TNE,R_t,tne)),       Err(RIE),
    Ok((DSLL,R_t,dsll)), Err(RIE),              Ok((DSRL,R_t,dsrl)), Ok((DSRA,R_t,dsra)),  Ok((DSLL32,R_t,dsll32)),   Err(RIE),                Ok((DSRL32,R_t,dsrl32)), Ok((DSRA32,R_t,dsra32)),
],
0b0000_0000_0000_0000_0000_0000_0011_1111,
0);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static regimm_lookup: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;32],usize,usize) = ([
    Ok((BLTZ,I_t,bltz)),     Ok((BGEZ,I_t,bgez)),     Ok((BLTZI,I_t,bltzi)),     Ok((BGEZL,I_t,bgezl)),     Err(RIE),            Err(RIE), Err(RIE),            Err(RIE),
    Ok((TGEI,I_t,tgei)),     Ok((TGEIU,I_t,tgeiu)),   Ok((TLTI,I_t,tlti)),       Ok((TLTIU,I_t,tltiu)),     Ok((TEQI,I_t,teqi)), Err(RIE), Ok((TNEI,I_t,tnei)), Err(RIE),
    Ok((BLTZAL,I_t,bltzal)), Ok((BGEZAL,I_t,bgezal)), Ok((BLTZALL,I_t,bltzall)), Ok((BGEZALL,I_t,bgezall)), Err(RIE),            Err(RIE), Err(RIE),            Err(RIE),
    Err(RIE),                Err(RIE),                Err(RIE),                  Err(RIE),                  Err(RIE),            Err(RIE), Err(RIE),            Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static coprs_lookup: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;32],usize,usize)= ([
    Ok((MF,R_t,mf)),     Ok((DMF,R_t,dmf)),     Ok((CF,R_t,cf)),       Err(RIE),            Ok((MT,R_t,mt)),     Ok((DMT,R_t,dmt)),   Ok((CT,R_t,ct)),     Err(RIE),
    Ok((BC,I_t,bc)),     Err(RIE),              Err(RIE),              Err(RIE),            Err(RIE),            Err(RIE),            Err(RIE),            Err(RIE),
    Ok((COPz,cop,copz)), Ok((COPz,cop,copz)),   Ok((COPz,cop,copz)),   Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)),
    Ok((COPz,cop,copz)), Ok((COPz,cop,copz)),   Ok((COPz,cop,copz)),   Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)), Ok((COPz,cop,copz)),
],
0b0000_0011_1110_0000_0000_0000_0000_0000,
21);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static coprt_lookup: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;32],usize,usize) = ([
	Ok((BCF,I_t,bcf)),  Ok((BCT,I_t,bct)),  Ok((BCFL,I_t,bcfl)), Ok((BCTL,I_t,bctl)), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),           Err(RIE),           Err(RIE),            Err(RIE),            Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),           Err(RIE),           Err(RIE),            Err(RIE),            Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),           Err(RIE),           Err(RIE),            Err(RIE),            Err(RIE), Err(RIE), Err(RIE), Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static copzero_lookup: ([Result<(instr::opcode,InstructionFormat,fn(&mut dyn Cpu, Instruction)),DisasemblerError>;64],usize,usize) = ([
    Err(InvOp),          Ok((TLBR,cop,tlbr)), Ok((TLBWI,cop,tlbwi)), Err(InvOp), Err(InvOp), Err(InvOp), Ok((TLBWR,cop,tlbwr)), Err(InvOp),
    Ok((TLBP,cop,tlbp)), Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Err(RIE),            Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Ok((ERET,cop,eret)), Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Err(InvOp),          Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Err(InvOp),          Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Err(InvOp),          Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
    Err(InvOp),          Err(InvOp),          Err(InvOp),            Err(InvOp), Err(InvOp), Err(InvOp), Err(InvOp),            Err(InvOp),
],
0b0000_0000_0000_0000_0000_0000_0011_1111,
0);

//pub fn decode(&mut self, raw: u32) -> &Instruction<'disas, dyn Cpu + 'disas> {
pub fn decode(raw: u32, delay_slot: bool) -> Instruction {
    let bytes: [u8; 4] = raw.to_be_bytes();

    /*let entry = self.ITC.entry(raw);
    if let std::collections::hash_map::Entry::Occupied(v) = entry {
        return v.get().clone();
    }*/

    //let opcode_bits = (raw & 0xFC00_0000) >> 26;
    //let special_index = raw & 0x0000_003F;
    //wtf was this for
    //let regimm_index = (raw & 0x001F_0000) >> 16;
    //let copzrs_index = (raw & 0x03E0_0000) >> 21;
    //let copzrt_index = regimm_index;
    //let cp0_index = special_index;

    //parse out all the fields
    let r_op_rs = ((raw & 0x03E0_0000) >> 21) as u8;
    let r_op_rt = ((raw & 0x001F_0000) >> 16) as u8;
    let r_op_rd = ((raw & 0x0000_F800) >> 11) as u8;
    let r_op_shamt = ((raw & 0x0000_07C0) >> 6) as u8;
    //let r_sub_op = (raw & 0x0000_003F) as u8;

    let i_op_rs = ((raw & 0x03E0_0000) >> 21) as u8;
    let i_op_rt = ((raw & 0x001F_0000) >> 16) as u8;
    let i_op_imm = (raw & 0x0000_FFFF) as u16;

    let j_op_imm = raw & 0x007FF_FFFF;

    let mut op: &Result<
        (
            instr::opcode,
            InstructionFormat,
            fn(&mut dyn Cpu, Instruction),
        ),
        DisasemblerError,
    > = &Err(Lookup64(&opcode_main));
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

    let function = op.as_ref().unwrap().2;

    //let local: fn(&mut dyn Cpu, Instruction) = ;
    /*let ret = entry.or_insert(Arc::new(Instruction {
        bytes,
        opcode: op.as_ref().unwrap().0,
        sources,
        dest,
        operation: function,
        //machine_code: Vec::new(),
        delay_slot,
    }));

    return ret.clone();*/
    Instruction {
        bytes,
        opcode: op.as_ref().unwrap().0,
        sources,
        dest,
        operation: function,
        //machine_code: Vec::new(),
        delay_slot,
    }
}

/*pub fn get_basic_block_at_addr(&mut self, addr: usize) -> Result<BasicBlock, &str> {
    let existing = self.Blocks.entry(addr);
    match existing {
        std::collections::hash_map::Entry::Occupied(v) => return Ok(v.get().clone()),
        std::collections::hash_map::Entry::Vacant(_) => {
            self.find_basic_block(addr);
            return Ok(self.Blocks.get(&addr).unwrap().clone());
        }
    }
}*/
//}
