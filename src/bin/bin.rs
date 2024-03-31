#![feature(hash_extract_if)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::{
    fs::{self, File},
    io::Read,
};

use disas;

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

    println!("{}\n\n", d.ITC.keys().len());
    //for block in d.Blocks.extract_if(|k, v| k.contains(&4)) {
    for block in d.Blocks.into_values() {
        println!("block contains {} instructions", block.instrs.len());
        for instr in block.instrs {
            println!("{}", instr)
        }
    }
}
