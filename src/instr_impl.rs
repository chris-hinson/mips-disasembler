use crate::{
    instr::{dest, source, Instruction, OpcodeExecutionError::*},
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
                cpu.throw_exception(ArithmeticOverFlow, inst.delay_slot)
            }
        }
        _ => unreachable!("uh oh. something's probably wrong in decode. badly formed instruction"),
    }
}

pub fn j(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn jal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn beq(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bne(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn blez(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgtz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn addiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn slti(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sltiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn andi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ori(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn xori(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
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
    unimplemented!("opcode not implemented")
}
pub fn bnel(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn blezl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgtzl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn daddi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn daddiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ldl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ldr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lb(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lh(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lwl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lw(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lbu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lhu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lwr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lwu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sb(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sh(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn swl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sw(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sdl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sdr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn swr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn cache(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ll(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lwc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lwc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn lld(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ldc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ldc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ld(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sc(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn swc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn swc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn scd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sdc1(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sdc2(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sll(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn srl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sra(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sllv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn srlv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn srav(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn jr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn jalr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn syscall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn break_(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sync(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mfhi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mthi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mflo(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mtlo(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsllv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsrlv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsrav(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mult(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn multu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn div(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn divu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dmult(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dmultu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ddiv(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ddivu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn addu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sub(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn subu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn and(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn or(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn xor(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn nor(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn slt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn sltu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dadd(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn daddu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsub(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsubu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tge(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tgeu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tltu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn teq(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tne(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsll(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsrl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsra(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsll32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsrl32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dsra32(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bltz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgez(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bltzi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgezl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tgei(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tgeiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlti(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tltiu(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn teqi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tnei(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bltzal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgezal(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bltzall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bgezall(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dmf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn cf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn mt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn dmt(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn ct(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bc(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn copz(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bcf(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bct(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bcfl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn bctl(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlbr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlbwi(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlbwr(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn tlbp(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
pub fn eret(cpu: &mut dyn Cpu, inst: Instruction) {
    unimplemented!("opcode not implemented")
}
