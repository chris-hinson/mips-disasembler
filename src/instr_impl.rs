#![allow(unused_variables)] // at the top of the file

//running notes:

/*
LW and SW implemented for good cases, but both have a huge variety of possible exceptions. these need to be implemented at some point

*/

use crate::{
    instr::{dest, source, Instruction, OpcodeExecutionError::*, OpcodeFetchError},
    Cpu,
};

pub fn add(cpu: &mut dyn Cpu, inst: Instruction) {
    panic!("dont execute me until chris implements exception handling");
    match inst.dest.unwrap() {
        dest::GPR(rd) => {
            let a = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
            let b = cpu.get_reg(inst.sources[1].unwrap().into()).unwrap();

            let _ = cpu.set_reg(rd, a + b).unwrap();
        }
        dest::CR(_r) => {
            panic!("cannot ADD coprocessors in this manner.")
        }
        dest::FPR(_r) => {
            panic!("cannot ADD coprocessors in this manner.")
        }
    }
}
pub fn addi(cpu: &mut dyn Cpu, inst: Instruction) {
    match inst.dest.unwrap() {
        dest::GPR(rd) => {
            let a = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
            let b = if !cpu._64bit_enabled() {
                let imm: u64 = inst.sources[1].unwrap().into();
                let imm = imm as i16 as i32 as u64;
                imm
            } else {
                let imm: u64 = inst.sources[1].unwrap().into();
                let imm = imm as i16 as i64 as u64;
                imm
            };

            let result = a.checked_add(b);
            if result.is_some() {
                cpu.set_reg(rd, a + b).unwrap();
            } else {
                //throw an arithmetic exception error
                cpu.throw_exception(ArithmeticOverFlow.into(), inst.delay_slot)
            }
        }
        _ => unreachable!("uh oh. something's probably wrong in decode. badly formed instruction"),
    }
}

pub fn j(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode j not implemented")
}
pub fn jal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode jal not implemented")
}
pub fn beq(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode beq not implemented")
}
pub fn bne(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut offset: u64 = inst.sources[1].unwrap().into();
    let delay_slot = cpu.get_pc() + 4;
    let target = delay_slot as i64 + offset as i64;

    let take = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap()
        != cpu.get_reg(inst.dest.unwrap().into()).unwrap();

    if take {
        cpu.set_pc(target as u64);
    }
}
pub fn blez(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode blez not implemented")
}
pub fn bgtz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgtz not implemented")
}
pub fn addiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode addiu not implemented")
}
pub fn slti(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode slti not implemented")
}
pub fn sltiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sltiu not implemented")
}
pub fn andi(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut imm: u64 = (inst.sources[1].unwrap().into());
    imm = (imm as u16) as u64;
    let reg_val = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
    let result = reg_val & imm;
    cpu.set_reg(inst.dest.unwrap().into(), result);
}
pub fn ori(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut imm: u64 = (inst.sources[1].unwrap().into());
    imm = (imm as u16) as u64;
    let reg_val = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
    let result = reg_val | imm;
    cpu.set_reg(inst.dest.unwrap().into(), result);
}
pub fn xori(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode xori not implemented")
}
pub fn lui(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut imm: u64 = (inst.sources[1].unwrap().into());
    imm = (imm as u16 as u64) << 16;
    if cpu._64bit_enabled() {
        imm = imm as i32 as i64 as u64;
    }
    cpu.set_reg(inst.dest.unwrap().into(), imm);
}
pub fn beql(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode beql not implemented")
}
pub fn bnel(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bnel not implemented")
}
pub fn blezl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode blezl not implemented")
}
pub fn bgtzl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgtzl not implemented")
}
pub fn daddi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode daddi not implemented")
}
pub fn daddiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode daddiu not implemented")
}
pub fn ldl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ldl not implemented")
}
pub fn ldr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ldr not implemented")
}
pub fn lb(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lb not implemented")
}
pub fn lh(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lh not implemented")
}
pub fn lwl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lwl not implemented")
}
pub fn lw(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut offset: u64 = inst.sources[1].unwrap().into();
    if !cpu._64bit_enabled() {
        offset = offset as u16 as i16 as i32 as u64
    } else {
        offset = offset as u16 as i16 as i64 as u64
    }
    let base = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
    let addr = (base as i64 + offset as i64) as u64;

    //let bytes: &[u8] = &(cpu.get_reg(inst.dest.unwrap().into()).unwrap() as u32).to_le_bytes();

    let res = u32::from_be_bytes(
        cpu.read(addr as usize, 4).unwrap()[0..4]
            .try_into()
            .expect("bruh"),
    );
    cpu.set_reg(inst.dest.unwrap().into(), res as u64);
}
pub fn lbu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lbu not implemented")
}
pub fn lhu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lhu not implemented")
}
pub fn lwr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lwr not implemented")
}
pub fn lwu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lwu not implemented")
}
pub fn sb(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sb not implemented")
}
pub fn sh(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sh not implemented")
}
pub fn swl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode swl not implemented")
}
pub fn sw(cpu: &mut dyn Cpu, inst: Instruction) {
    let mut offset: u64 = inst.sources[1].unwrap().into();
    if !cpu._64bit_enabled() {
        offset = offset as u16 as i16 as i32 as u64
    } else {
        offset = offset as u16 as i16 as i64 as u64
    }
    let base = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
    let addr = (base as i64 + offset as i64) as u64;

    let bytes: &[u8] = &(cpu.get_reg(inst.dest.unwrap().into()).unwrap() as u32).to_le_bytes();

    cpu.write(addr as usize, bytes);
}
pub fn sdl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sdl not implemented")
}
pub fn sdr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sdr not implemented")
}
pub fn swr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode swr not implemented")
}
pub fn cache(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode cache not implemented")
}
pub fn ll(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ll not implemented")
}
pub fn lwc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lwc1 not implemented")
}
pub fn lwc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lwc2 not implemented")
}
pub fn lld(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode lld not implemented")
}
pub fn ldc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ldc1 not implemented")
}
pub fn ldc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ldc2 not implemented")
}
pub fn ld(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ld not implemented")
}
pub fn sc(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sc not implemented")
}
pub fn swc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode swc1 not implemented")
}
pub fn swc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode swc2 not implemented")
}
pub fn scd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode scd not implemented")
}
pub fn sdc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sdc1 not implemented")
}
pub fn sdc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sdc2 not implemented")
}
pub fn sd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sd not implemented")
}
pub fn sll(cpu: &mut dyn Cpu, inst: Instruction) {
    let original_value = cpu.get_reg(inst.sources[1].unwrap().into()).unwrap();
    let shamt: u64 = inst.sources[2].unwrap().into();
    let new_value: u64 = original_value << shamt;
    cpu.set_reg(inst.dest.unwrap().into(), new_value as u64).unwrap();
}
pub fn srl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode srl not implemented")
}
pub fn sra(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sra not implemented")
}
pub fn sllv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sllv not implemented")
}
pub fn srlv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode srlv not implemented")
}
pub fn srav(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode srav not implemented")
}
pub fn jr(cpu: &mut dyn Cpu, inst: Instruction) {
    let addr = cpu.get_reg(inst.sources[0].unwrap().into()).unwrap();
    if (addr & 0b11) !=0{
        //throw address exception as if it occured on the fetch stage
        cpu.throw_exception(OpcodeFetchError::AddressAlignmentException.into(), inst.delay_slot);
    }

    cpu.set_pc(addr);
}
pub fn jalr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode jalr not implemented")
}
pub fn syscall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode syscall not implemented")
}
pub fn break_(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode break_ not implemented")
}
pub fn sync(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sync not implemented")
}
pub fn mfhi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mfhi not implemented")
}
pub fn mthi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mthi not implemented")
}
pub fn mflo(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mflo not implemented")
}
pub fn mtlo(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mtlo not implemented")
}
pub fn dsllv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsllv not implemented")
}
pub fn dsrlv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsrlv not implemented")
}
pub fn dsrav(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsrav not implemented")
}
pub fn mult(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mult not implemented")
}
pub fn multu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode multu not implemented")
}
pub fn div(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode div not implemented")
}
pub fn divu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode divu not implemented")
}
pub fn dmult(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dmult not implemented")
}
pub fn dmultu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dmultu not implemented")
}
pub fn ddiv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ddiv not implemented")
}
pub fn ddivu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ddivu not implemented")
}
pub fn addu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode addu not implemented")
}
pub fn sub(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sub not implemented")
}
pub fn subu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode subu not implemented")
}
pub fn and(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode and not implemented")
}
pub fn or(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode or not implemented")
}
pub fn xor(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode xor not implemented")
}
pub fn nor(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode nor not implemented")
}
pub fn slt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode slt not implemented")
}
pub fn sltu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode sltu not implemented")
}
pub fn dadd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dadd not implemented")
}
pub fn daddu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode daddu not implemented")
}
pub fn dsub(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsub not implemented")
}
pub fn dsubu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsubu not implemented")
}
pub fn tge(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tge not implemented")
}
pub fn tgeu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tgeu not implemented")
}
pub fn tlt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlt not implemented")
}
pub fn tltu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tltu not implemented")
}
pub fn teq(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode teq not implemented")
}
pub fn tne(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tne not implemented")
}
pub fn dsll(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsll not implemented")
}
pub fn dsrl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsrl not implemented")
}
pub fn dsra(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsra not implemented")
}
pub fn dsll32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsll32 not implemented")
}
pub fn dsrl32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsrl32 not implemented")
}
pub fn dsra32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dsra32 not implemented")
}
pub fn bltz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bltz not implemented")
}
pub fn bgez(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgez not implemented")
}
pub fn bltzi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bltzi not implemented")
}
pub fn bgezl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgezl not implemented")
}
pub fn tgei(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tgei not implemented")
}
pub fn tgeiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tgeiu not implemented")
}
pub fn tlti(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlti not implemented")
}
pub fn tltiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tltiu not implemented")
}
pub fn teqi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode teqi not implemented")
}
pub fn tnei(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tnei not implemented")
}
pub fn bltzal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bltzal not implemented")
}
pub fn bgezal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgezal not implemented")
}
pub fn bltzall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bltzall not implemented")
}
pub fn bgezall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bgezall not implemented")
}
pub fn mf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mf not implemented")
}
pub fn dmf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dmf not implemented")
}
pub fn cf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode cf not implemented")
}
pub fn mt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode mt not implemented")
}
pub fn dmt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode dmt not implemented")
}
pub fn ct(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode ct not implemented")
}
pub fn bc(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bc not implemented")
}
pub fn copz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode copz not implemented")
}
pub fn bcf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bcf not implemented")
}
pub fn bct(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bct not implemented")
}
pub fn bcfl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bcfl not implemented")
}
pub fn bctl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode bctl not implemented")
}
pub fn tlbr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlbr not implemented")
}
pub fn tlbwi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlbwi not implemented")
}
pub fn tlbwr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlbwr not implemented")
}
pub fn tlbp(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode tlbp not implemented")
}
pub fn eret(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode eret not implemented")
}
