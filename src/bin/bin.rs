/*#![feature(hash_extract_if)]

use disas;
use disas::instr::{OperatingMode, Xlen};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::Read;

use disas::instr::Cpu;

pub struct MyCpu {}
impl Cpu for MyCpu {
    fn get_reg(&mut self, reg: disas::instr::GPR) -> Result<u64, std::io::Error> {
        unimplemented!()
    }
    fn set_reg(&mut self, reg: disas::instr::GPR, val: u64) -> Result<u64, std::io::Error> {
        unimplemented!()
    }

    fn get_cop_reg(&mut self, cop_indx: u8, reg_indx: u8) -> Result<u64, std::io::Error> {
        unimplemented!()
    }
    fn set_cop_reg(&mut self, cop_indx: u8, reg_indx: u8, val: u64) -> Result<u64, std::io::Error> {
        unimplemented!()
    }
    /*fn get_xlen(&self) -> Xlen {
        disas::instr::Xlen::X32
    }
    fn get_operating_mode(&self) -> OperatingMode {
        disas::instr::OperatingMode::User
        }*/
    fn _64bit_enabled(&self) -> bool {
        true
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
}

thread_local! {
pub static GLOBAL_BYTES: RefCell<Vec<u32>> = RefCell::new(Vec::new());
}
pub fn main() {
    let mut f = File::open("./addiu_simpleboot.z64").expect("no file found");
    let metadata = fs::metadata("./addiu_simpleboot.z64").expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    f.read(&mut buffer).expect("buffer overflow");

    buffer.drain(0..0x1000);

    //let _address = 0x1000;

    for element in 0..(buffer.len() / 4) {
        let bytes = u32::from_be_bytes([
            buffer[element * 4 + 0],
            buffer[element * 4 + 1],
            buffer[element * 4 + 2],
            buffer[element * 4 + 3],
        ]);
        GLOBAL_BYTES.with(|foo| foo.borrow_mut().push(bytes));
    }

    let mut d = disas::Disasembler {
        ITC: HashMap::new(),
        Blocks: HashMap::new(),
        Reader: Box::new(|addr: &usize| {
            return GLOBAL_BYTES.with(|foo| foo.borrow_mut()[addr / 4]);
        }),
    };

    d.find_basic_block(0);

    let mut c = MyCpu {};

    println!("{}\n\n", d.ITC.keys().len());
    //for block in d.Blocks.extract_if(|k, v| k.contains(&4)) {
    for block in d.Blocks.into_values() {
        println!("block contains {} instructions", block.instrs.len());
        for instr in block.instrs {
            println!("{}", instr);
            (instr.operation)(&mut c, *instr);
        }
    }
}
*/
