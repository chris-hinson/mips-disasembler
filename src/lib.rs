#![feature(hash_extract_if)]

mod instr;

use std::collections::HashMap;
use std::rc::Rc;

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

use crate::instr::dest;

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
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
#[allow(non_upper_case_globals)]
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
#[allow(non_upper_case_globals)]
static regimm_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize) = ([
    Ok((BLTZ,I_t)),   Ok((BGEZ,I_t)),   Ok((BLTZI,I_t)),   Ok((BGEZL,I_t)),   Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Ok((TGEI,I_t)),   Ok((TGEIU,I_t)),  Ok((TLTI,I_t)),    Ok((TLTIU,I_t)),   Ok((TEQI,I_t)), Err(RIE), Ok((TNEI,I_t)), Err(RIE),
    Ok((BLTZAL,I_t)), Ok((BGEZAL,I_t)), Ok((BLTZALL,I_t)), Ok((BGEZALL,I_t)), Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
    Err(RIE),         Err(RIE),         Err(RIE),          Err(RIE),          Err(RIE),       Err(RIE), Err(RIE),       Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static coprs_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize)= ([
    Ok((MF,R_t)),   Ok((DMF,R_t)),    Ok((CF,R_t)),     Err(RIE),       Ok((MT,R_t)),   Ok((DMT,R_t)),    Ok((CT,R_t)),     Err(RIE),
    Ok((BC,I_t)),   Err(RIE),         Err(RIE),         Err(RIE),       Err(RIE),       Err(RIE),         Err(RIE),         Err(RIE),
    Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)), Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)),
    Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)), Ok((COPz,cop)), Ok((COPz,cop)),   Ok((COPz,cop)),   Ok((COPz,cop)),
],
0b0000_0011_1110_0000_0000_0000_0000_0000,
21);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
static coprt_lookup: ([Result<(instr::opcode,InstructionFormat),DisasemblerError>;32],usize,usize) = ([
	Ok((BCF,I_t)),  Ok((BCT,I_t)),  Ok((BCFL,I_t)), Ok((BCTL,I_t)), Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
	Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE),       Err(RIE), Err(RIE), Err(RIE), Err(RIE),
],
0b0000_0000_0001_1111_0000_0000_0000_0000,
16);

#[rustfmt::skip]
#[allow(non_upper_case_globals)]
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

/*8pub struct MyCpu {}
impl Cpu for MyCpu {
    fn get_reg(&mut self, reg: GPR) -> Result<u64, std::io::Error> {
        unimplemented!()
    }
    fn set_reg(&mut self, reg: GPR) -> Result<u64, std::io::Error> {
        unimplemented!()
    }
}
impl std::io::Read for MyCpu {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        unimplemented!()
    }
}
impl std::io::Write for MyCpu {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        unimplemented!()
    }
    fn flush(&mut self) -> std::io::Result<()> {
        unimplemented!()
    }
    }*/

#[allow(non_snake_case)]
pub struct Disasembler {
    //hashset of thus far decoded instructions
    //this is the "global" instruction translation cache
    //we hash the raw bits of an instruction and equate that with a fully formed Instruction struct
    //TODO: hashing is overkill here. use a 2 or 3 tier linear cache that populates pages as we hit them (sparse array)
    pub ITC: HashMap<u32, Rc<Instruction>>,

    //NOTE: this may eventually be superceded by a fastmem implementation
    //for now, we map address ranges to the basic block struct representing the instructions that lie there
    pub Blocks: HashMap<std::ops::Range<usize>, BasicBlock>,

    //the disasembler needs some way to get bytes out of rom.
    //since we are explicitly dealing with instructions, handle turning it into a u32 before it gets here
    pub Reader: Box<dyn FnMut(&usize) -> u32>,
}

impl Disasembler {
    pub fn test_fn_addi(_cpu: &mut (dyn Cpu)) {}

    //finding a basic block differs in interpreter and JIT mode
    //in interpreter mode, a basic block is defined by as many instructions as you can go without hitting a control flow op
    //in a jit block, the same rule holds BUT we may need to stop the block early if we run out of host registers (especially prevalent on x86)
    //NOTE: maybe we just always find it in the "interpreter" way since thats the more textbook definition of a basic block
    //and then in jit mode, if we run out of registers WHEN EMITTING we can split the basic block and error handle in the emitter

    pub fn find_basic_block(&mut self, mut addr: usize) {
        //first, is this address already in an existing basicblock?
        let existing = self
            .Blocks
            .clone()
            .extract_if(|k, _v| k.contains(&addr))
            .collect::<Vec<_>>();
        if existing.len() > 0 {
            //we requested a basic block starting at an adress within an existing basic block.
            //for now dont do anything. we may desire additional behavior here later
            return;
        }

        //TODO: we should probably search backwards a bit and check if we're starting a basic block at a proper address. but for now just assume we called it with a good address

        let mut cur_block = BasicBlock {
            _valid: true,
            base: addr,
            instrs: Vec::new(),
        };

        //grab some bytes and turn them into an instruction
        let mut cur_bytes = (self.Reader)(&addr);
        let mut cur_instr = self.decode(cur_bytes);

        //did we just decode a block ending instruction?
        while !INSTR_WHICH_END_BASIC_BLOCK.contains(&cur_instr.opcode) {
            cur_block.instrs.push(cur_instr);

            cur_bytes = (self.Reader)(&addr);
            cur_instr = self.decode(cur_bytes);
            addr += 4;
        }
        //push the last instr we decoded before we figure out our block was over
        cur_block.instrs.push(cur_instr);

        //ADD THE DELAY SLOT INSTRUCTION
        cur_bytes = (self.Reader)(&addr);
        cur_instr = self.decode(cur_bytes);
        cur_block.instrs.push(cur_instr);

        //HANDLE DELAY SLOT FUCKERY

        self.Blocks.insert(cur_block.base..addr, cur_block);
    }

    /*pub fn ITC_fetch_or_decode_and_insert(
        &mut self,
        bytes: u32,
    ) -> &Instruction<'disas, dyn Cpu + 'disas> {
        let itc_binding = self.ITC.entry(bytes);
        let v = itc_binding.or_insert(Self::decode(bytes));
        v
        */

    //pub fn decode(&mut self, raw: u32) -> &Instruction<'disas, dyn Cpu + 'disas> {
    pub fn decode(&mut self, raw: u32) -> Rc<Instruction> {
        let bytes: [u8; 4] = raw.to_be_bytes();

        let entry = self.ITC.entry(raw);
        if let std::collections::hash_map::Entry::Occupied(v) = entry {
            return v.get().clone();
        }

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

        let local: fn(&mut dyn Cpu) = Self::test_fn_addi;
        let ret = entry.or_insert(Rc::new(Instruction {
            bytes,
            opcode: op.as_ref().unwrap().0,
            sources,
            dest,
            operation: local,
            machine_code: Vec::new(),
        }));

        return ret.clone();
    }
}

//a basic block is a set of instructions.
//these instructions may either be interpreted or emitted into a host buffer to be executed by the jit engine
//this means that we must represent two possible
#[derive(Clone)]
pub struct BasicBlock {
    //is this block currently valid?
    _valid: bool,

    //base address of the block
    base: usize,

    //each instruction in a basic block is actually a pointer to that opcode in the global instruction translation cache (ITC)
    pub instrs: Vec<Rc<Instruction>>,
}
