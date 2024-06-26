use core::fmt;
use std::{fmt::write, hash::Hash};

//use proc_bitfield::bitfield;

pub static INSTR_WHICH_END_BASIC_BLOCK: [opcode; 26] = [
    opcode::J,
    opcode::JR,
    opcode::JAL,
    opcode::JALR,
    opcode::BC,
    opcode::BCF,
    opcode::BCFL,
    opcode::BCT,
    opcode::BCTL,
    opcode::BEQ,
    opcode::BEQL,
    opcode::BGEZ,
    opcode::BGEZAL,
    opcode::BGEZALL,
    opcode::BGEZL,
    opcode::BGTZ,
    opcode::BGTZL,
    opcode::BLEZ,
    opcode::BLEZL,
    opcode::BLTZ,
    opcode::BLTZAL,
    opcode::BLTZALL,
    opcode::BLTZI,
    opcode::BNE,
    opcode::BNEL,
    opcode::ERET,
];
/*pub struct disasembler {
    operating_mode: OperatingMode,
    sixty_four_bit: bool,
    cop1_enable: bool,
}
*/
pub enum OperatingMode {
    User,
    Supervisor,
    Kernel,
}
pub enum Xlen {
    X32,
    X64,
}

//hm.
pub trait Cpu {
    //fn get_xlen(&self) -> Xlen;
    //fn get_operating_mode(&self) -> OperatingMode;
    fn _64bit_enabled(&self) -> bool;

    fn get_reg(&self, reg: GPR) -> Result<u64, std::io::Error>;
    fn set_reg(&mut self, reg: GPR, val: u64) -> Result<u64, std::io::Error>;
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, new_pc: u64);

    fn get_cop_reg(&mut self, cop_indx: u8, reg_indx: u8) -> Result<u64, std::io::Error>;
    fn set_cop_reg(&mut self, cop_indx: u8, reg_indx: u8, val: u64) -> Result<u64, std::io::Error>;
    fn throw_exception(&mut self, err: Exception, delay_slot: bool) {}

    fn read(&self, addr: usize, len: usize) -> std::io::Result<Vec<u8>>;
    fn write(&mut self, addr: usize, bytes: &[u8]) -> std::io::Result<usize>;
}
pub enum OpcodeExecutionError {
    IoError,
    ArithmeticOverFlow,
}
pub enum OpcodeFetchError {
    AddressAlignmentException
}
impl From<OpcodeExecutionError> for Exception{
    fn from(value: OpcodeExecutionError) -> Self {
        Exception::Execution(value)
    }
}
impl From<OpcodeFetchError> for Exception{
    fn from(value: OpcodeFetchError) -> Self {
        Exception::Fetch(value)
    }
}
pub enum Exception{
    Execution(OpcodeExecutionError),
    Fetch(OpcodeFetchError)
}
impl From<std::io::Error> for OpcodeExecutionError {
    fn from(_value: std::io::Error) -> Self {
        return OpcodeExecutionError::IoError;
    }
}

//this struct defines a single MIPS op.
//it contains the original bytes,
//an easily parseable enum for what op it is
//and a string mnemonic
#[derive(Clone, Copy)]
pub struct Instruction {
    pub bytes: [u8; 4],
    pub opcode: opcode,
    pub sources: [Option<source>; 3],
    pub dest: Option<dest>,
    //pub operation: Box<dyn FnMut(&'a T) + 'a>,
    pub operation: fn(&mut dyn Cpu, Instruction),
    //pub machine_code: Vec<u8>,
    pub delay_slot: bool,
}
impl Hash for Instruction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bytes.hash(state);
        self.opcode.hash(state);
        self.sources.hash(state);
        self.dest.hash(state);
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{:?}:\n\t[{:02x},{:02x},{:02x},{:02x}]\n\tsources:{:?}\n\tdest:{:?}\n\tis delay slot?:{}\n",
            self.opcode,
            self.bytes[0],
            self.bytes[1],
            self.bytes[2],
            self.bytes[3],
            self.sources,
            self.dest,
            self.delay_slot
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
pub enum InstructionFormat {
    I_t,
    J_t,
    R_t,
    cop,
}

#[derive(PartialEq, Debug)]
pub enum DisasemblerError<'a> {
    //reserved instruction excpetion
    RIE,
    InvOp,
    Lookup64(
        &'a (
            [Result<
                (opcode, InstructionFormat, fn(&mut dyn Cpu, Instruction)),
                DisasemblerError<'a>,
            >; 64],
            usize,
            usize,
        ),
    ),
    Lookup32(
        &'a (
            [Result<
                (opcode, InstructionFormat, fn(&mut dyn Cpu, Instruction)),
                DisasemblerError<'a>,
            >; 32],
            usize,
            usize,
        ),
    ),
}

#[derive(Debug, Clone, Copy, Hash)]
#[allow(non_camel_case_types)]
pub enum source {
    GPR(GPR),
    CR(cop0reg),
    FPR(usize),
    IMM(u64),
}
impl std::fmt::Display for source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            source::GPR(v) => write!(f, "{:?}", v),
            source::CR(v) => write!(f, "{:?}", v),
            source::FPR(v) => write!(f, "{:?}", v),
            source::IMM(i) => write!(f, "{:#x}", i),
        }
    }
}
impl From<source> for GPR {
    fn from(value: source) -> Self {
        match value {
            source::GPR(v) => v,
            source::CR(_v) => panic!("bad conversion"),
            source::FPR(_v) => panic!("bad conversion"),
            source::IMM(_i) => panic!("bad conversion"),
        }
    }
}
impl From<source> for u64 {
    fn from(value: source) -> Self {
        match value {
            source::GPR(_v) => {
                panic!("bad conversion")
            }
            source::CR(_v) => {
                panic!("bad conversion")
            }
            source::FPR(_v) => {
                panic!("bad conversion")
            }
            source::IMM(i) => i,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash)]
#[allow(non_camel_case_types)]
pub enum dest {
    GPR(GPR),
    CR(cop0reg),
    FPR(usize),
}

impl From<dest> for GPR {
    fn from(value: dest) -> Self {
        match value {
            dest::GPR(v) => v,
            _ => {
                panic!("cannot convert non-gpr variant to GPR")
            }
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum opcode {
    J,
    JAL,
    BEQ,
    BNE,
    BLEZ,
    BGTZ,
    ADDI,
    ADDIU,
    SLTI,
    SLTIU,
    ANDI,
    ORI,
    XORI,
    LUI,
    BEQL,
    BNEL,
    BLEZL,
    BGTZL,
    DADDI,
    DADDIU,
    LDL,
    LDR,
    LB,
    LH,
    LWL,
    LW,
    LBU,
    LHU,
    LWR,
    LWU,
    SB,
    SH,
    SWL,
    SW,
    SDL,
    SDR,
    SWR,
    CACHE,
    LL,
    LWC1,
    LWC2,
    LLD,
    LDC1,
    LDC2,
    LD,
    SC,
    SWC1,
    SWC2,
    SCD,
    SDC1,
    SDC2,
    SD,
    SLL,
    SRL,
    SRA,
    SLLV,
    SRLV,
    SRAV,
    JR,
    JALR,
    SYSCALL,
    BREAK,
    SYNC,
    MFHI,
    MTHI,
    MFLO,
    MTLO,
    DSLLV,
    DSRLV,
    DSRAV,
    MULT,
    MULTU,
    DIV,
    DIVU,
    DMULT,
    DMULTU,
    DDIV,
    DDIVU,
    ADD,
    ADDU,
    SUB,
    SUBU,
    AND,
    OR,
    XOR,
    NOR,
    SLT,
    SLTU,
    DADD,
    DADDU,
    DSUB,
    DSUBU,
    TGE,
    TGEU,
    TLT,
    TLTU,
    TEQ,
    TNE,
    DSLL,
    DSRL,
    DSRA,
    DSLL32,
    DSRL32,
    DSRA32,
    BLTZ,
    BGEZ,
    BLTZI,
    BGEZL,
    TGEI,
    TGEIU,
    TLTI,
    TLTIU,
    TEQI,
    TNEI,
    BLTZAL,
    BGEZAL,
    BLTZALL,
    BGEZALL,
    MF,
    DMF,
    CF,
    MT,
    DMT,
    CT,
    BC,
    COPz,
    BCF,
    BCT,
    BCFL,
    BCTL,
    TLBR,
    TLBWI,
    TLBWR,
    TLBP,
    ERET,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum GPR {
    zero,
    at,
    v0,
    v1,
    a0,
    a1,
    a2,
    a3,
    t0,
    t1,
    t2,
    t3,
    t4,
    t5,
    t6,
    t7,
    s0,
    s1,
    s2,
    s3,
    s4,
    s5,
    s6,
    s7,
    t8,
    t9,
    k0,
    k1,
    gp,
    sp,
    fp,
    ra,
}
impl std::fmt::Display for GPR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match *self {
            GPR::zero => "zero",
            GPR::at => "at",
            GPR::v0 => "v0",
            GPR::v1 => "v1",
            GPR::a0 => "a0",
            GPR::a1 => "a1",
            GPR::a2 => "a2",
            GPR::a3 => "a3",
            GPR::t0 => "t0",
            GPR::t1 => "t1",
            GPR::t2 => "t2",
            GPR::t3 => "t3",
            GPR::t4 => "t4",
            GPR::t5 => "t5",
            GPR::t6 => "t6",
            GPR::t7 => "t7",
            GPR::s0 => "s0",
            GPR::s1 => "s1",
            GPR::s2 => "s2",
            GPR::s3 => "s3",
            GPR::s4 => "s4",
            GPR::s5 => "s5",
            GPR::s6 => "s6",
            GPR::s7 => "s7",
            GPR::t8 => "t8",
            GPR::t9 => "t9",
            GPR::k0 => "k0",
            GPR::k1 => "k1",
            GPR::gp => "gp",
            GPR::sp => "sp",
            GPR::fp => "fp",
            GPR::ra => "ra",
        };
        write!(f, "{}", str)
    }
}
impl From<GPR> for u8 {
    fn from(val: GPR) -> Self {
        return match val {
            GPR::zero => 0,
            GPR::at => 1,
            GPR::v0 => 2,
            GPR::v1 => 3,
            GPR::a0 => 4,
            GPR::a1 => 5,
            GPR::a2 => 6,
            GPR::a3 => 7,
            GPR::t0 => 8,
            GPR::t1 => 9,
            GPR::t2 => 10,
            GPR::t3 => 11,
            GPR::t4 => 12,
            GPR::t5 => 13,
            GPR::t6 => 14,
            GPR::t7 => 15,
            GPR::s0 => 16,
            GPR::s1 => 17,
            GPR::s2 => 18,
            GPR::s3 => 19,
            GPR::s4 => 20,
            GPR::s5 => 21,
            GPR::s6 => 22,
            GPR::s7 => 23,
            GPR::t8 => 24,
            GPR::t9 => 25,
            GPR::k0 => 26,
            GPR::k1 => 27,
            GPR::gp => 28,
            GPR::sp => 29,
            GPR::fp => 30,
            GPR::ra => 31,
        };
    }
}
impl From<u8> for GPR {
    fn from(val: u8) -> Self {
        return match val {
            0 => GPR::zero,
            1 => GPR::at,
            2 => GPR::v0,
            3 => GPR::v1,
            4 => GPR::a0,
            5 => GPR::a1,
            6 => GPR::a2,
            7 => GPR::a3,
            8 => GPR::t0,
            9 => GPR::t1,
            10 => GPR::t2,
            11 => GPR::t3,
            12 => GPR::t4,
            13 => GPR::t5,
            14 => GPR::t6,
            15 => GPR::t7,
            16 => GPR::s0,
            17 => GPR::s1,
            18 => GPR::s2,
            19 => GPR::s3,
            20 => GPR::s4,
            21 => GPR::s5,
            22 => GPR::s6,
            23 => GPR::s7,
            24 => GPR::t8,
            25 => GPR::t9,
            26 => GPR::k0,
            27 => GPR::k1,
            28 => GPR::gp,
            29 => GPR::sp,
            30 => GPR::fp,
            31 => GPR::ra,
            _ => panic!("bad value"),
        };
    }
}

//in depth defs on page 146
//CP0 regs
/*
0 Index Programmable pointer into TLB array
1 Random Pseudorandom pointer into TLB array (read only)
2 EntryLo0 Low half of TLB entry for even virtual address (VPN)
3 EntryLo1 Low half of TLB entry for odd virtual address (VPN)
4 Context Pointer to kernel virtual page table entry (PTE) in 32-bit mode
5 PageMask Page size specification
6 Wired Number of wired TLB entries
7 — Reserved for future use
8 BadVAddr Display of virtual address that occurred an error last
9 Count Timer Count
10 EntryHi High half of TLB entry (including ASID)
11 Compare Timer Compare Value
12 Status Operation status setting
13 Cause Display of cause of last exception
14 EPC Exception Program Counter
15 PRId Processor Revision Identifier
16 Config Memory system mode setting
17 LLAddr Load Linked instruction address display
18 WatchLo Memory reference trap address low bits
19 WatchHi Memory reference trap address high bits
20 XContext Pointer to Kernel virtual PTE table in 64-bit mode
21–25 — Reserved for future use
26 Parity Error* Cache parity bits
27 Cache Error* Cache Error and Status register
28 TagLo Cache Tag register low
29 TagHi Cache Tag register high
30 ErrorEPC Error Exception Program Counter
31 — Reserved for future use
*/
#[derive(Debug, Clone, Copy, Hash)]
#[allow(non_snake_case, non_camel_case_types)]
pub enum cop0reg {
    Index,    //32 bit
    Random,   //32 bit
    EntryLo0, //64 bit (32 bit access sign extends)
    EntryLo1, //64 bit (32 bit access sign extends)
    Context,  //64 bit (32 bit sign access sign extends?)
    PageMask, //64 bit (32 bit access sign extends)
    Wired,    //32 bit
    //7 — Reserved for future use
    BadVAddr, //64 (32 ?)
    Count,    //32 bit
    EntryHi,  //64 bit (32 bit access sign extends)
    Compare,  //32 bit
    Status,   //32 bit NOTE: this is actually a bitfield
    Cause,    //32 bit NOTE: this is actually a bitfield
    EPC,      //64 (32?)
    PRId,     //32 NOTE: bitfield
    Config,   //32 NOTE: bitfield
    LLAddr,   //32
    WatchLo,  //32
    WatchHi,  //32
    XContext, //64 NOTE: bitfield
    //21–25 — Reserved for future use
    Parity, //32: bitfield
    Cache,  //32
    TagLo,  //32 bitfield
    TagHi,  //32 bitfield
    ErrorEPC, //64 (32?)
            //31 — Reserved for future use
}

/*
//make this indexable by an enum of all the registers it contains. impl Index and IndexMut traits
#[derive(Default)]
#[allow(non_snake_case, non_camel_case_types)]
pub struct cop0 {
    pub Index: u32,    //32 bit
    pub Random: u32,   //32 bit
    pub EntryLo0: u64, //64 bit (32 bit access sign extends)
    pub EntryLo1: u64, //64 bit (32 bit access sign extends)
    pub Context: u64,  //64 bit (32 bit sign access sign extends?)
    pub PageMask: u64, //64 bit (32 bit access sign extends)
    pub Wired: u32,    //32 bit
    //7 — Reserved for future use
    pub BadVAddr: u64,          //64 (32 ?)
    pub Count: u32,             //32 bit
    pub EntryHi: u64,           //64 bit (32 bit access sign extends)
    pub Compare: u32,           //32 bit
    pub Status: status_reg,     //32 bit NOTE: this is actually a bitfield
    pub Cause: cause_reg,       //32 bit NOTE: this is actually a bitfield
    pub EPC: u64,               //64 (32?)
    pub PRId: PRId_reg,         //32 NOTE: bitfield
    pub Config: config_reg,     //32 NOTE: bitfield
    pub LLAddr: u32,            //32
    pub WatchLo: u32,           //32
    pub WatchHi: u32,           //32
    pub XContext: XContext_reg, //64 NOTE: bitfield
    //21–25 — Reserved for future use
    //this reg is only here for VR4200 compat and we never use it. so no nice bitfield for it
    pub Parity: u32,      //32: bitfield
    pub Cache: u32,       //32
    pub TagLo: TagLo_reg, //32 bitfield
    //this is just always 0??
    pub TagHi: u32,    //32 bitfield
    pub ErrorEPC: u64, //64 (32?)
}

//status reg
#[allow(non_snake_case)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case,non_camel_case_types)]
    pub struct status_reg(pub u32): Debug, FromRaw, IntoRaw, DerefRaw{
        pub CU: u8 @ 28..=31,
        pub RP: bool @ 27,
        pub FR: bool @ 26,
        pub RE: bool @ 25,
        //pub DS: DS @ 16..=24,
        //these fields are part of the sub bitfield SD (self-diagnostic)
        pub ITS: bool @ 24,
        //hardwired 0 23
        pub BEV: bool @22,
        pub TS: bool @ 21,
        pub SR: bool @ 20,
        //hardwired 0 19
        pub CH: bool @ 18,
        pub CE: bool @ 17,
        pub DE: bool @ 16,
        //end of sub-bitfield
        pub IM: u8 @ 8..=15,
        pub KX: bool @ 7,
        pub SX: bool @ 6,
        pub UX: bool @ 5,
        pub KSU: u8 @ 3..=4,
        pub ERL: bool @ 2,
        pub EXL: bool @ 1,
        pub IE: bool @ 0,
    }
}

//cause register
#[allow(non_snake_case)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case,non_camel_case_types)]
    pub struct cause_reg(pub u32): Debug, FromRaw, IntoRaw, DerefRaw{
        pub BD: bool @ 31,
        //bit 30 is 0
        pub CE: u8 @ 28 ..= 29,
        //bits 26 ..= 27 are 0
        pub IP: u8 @ 8 ..= 15,
        //bit 7 is 0,
        pub ExcCode: u8 @ 2 ..= 6,
        //bit 0 and 1 are 0
    }
}

//PRId register
#[allow(non_snake_case)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case,non_camel_case_types)]
    pub struct PRId_reg(pub u32): Debug, FromRaw, IntoRaw, DerefRaw{
        //upper 16 are zeroed
        pub Imp: u8 @ 8 ..= 15,
        pub Rev: u8 @ 0 ..= 7,

    }
}
//Config register
#[allow(non_snake_case, non_camel_case_types)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case)]
    pub struct config_reg(pub u32): Debug, FromRaw, IntoRaw, DerefRaw{
        //bit 31 is 0
        pub EC: u8 @ 28 ..= 30,
        pub EP: u8 @ 24 ..= 27,
        //16..= 23 are set explicitly to the pattern "00000110"
        pub BE: bool @ 15,
        //4..= 14 are set eplicitly to the pattern "11001000110"
        pub CU: bool @ 3,
        pub K0: u8 @ 0..=2
    }
}

//this reg might be important later.
//XContext register
#[allow(non_snake_case, non_camel_case_types)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case)]
    pub struct XContext_reg(pub u64): Debug, FromRaw, IntoRaw, DerefRaw{
        pub PTEBase: u32 @ 33 ..= 63,
        pub R: u8 @ 31 ..= 32,
        pub BadVPN2: u32 @ 4..= 30,
        //0..=3 are 0
    }
}

//TagLo register
#[allow(non_snake_case, non_camel_case_types)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case)]
    pub struct TagLo_reg(pub u64): Debug, FromRaw, IntoRaw, DerefRaw{
        //28 ..= 31 are 0
        pub PTagLo: u32 @ 8 ..= 27,
        pub PState: u8 @ 6..= 7,
        //0..=5 are 0

    }
}

//TagHi register
//all 0s all the time??

//this controls our fp modes and assoc shit
#[allow(non_snake_case)]
bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq, Default)]
    #[allow(non_snake_case,non_camel_case_types)]
    pub struct FP_control_reg(pub u64): Debug, FromRaw, IntoRaw, DerefRaw{
        //25 ..= 31
        pub FS: bool @ 24,
        pub C: bool @ 23,
        //18..=22 are 0
        pub Cause: u8 @ 12 ..= 17,
        pub Enables: u8 @ 7 ..= 11,
        pub Flags: u8 @ 2 ..= 6,
        pub RM: u8 @ 0 ..= 1
    }
}*/
