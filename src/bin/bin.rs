use std::{
    fs::{self, File},
    io::Read,
};

use disas;

pub fn main() {
    let mut f = File::open("./addiu_simpleboot.z64").expect("no file found");
    let metadata = fs::metadata("./addiu_simpleboot.z64").expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    f.read(&mut buffer).expect("buffer overflow");

    buffer.drain(0..0x1000);

    let mut address = 0x1000;

    for element in 0..(buffer.len() / 4) {
        let bytes = u32::from_be_bytes([
            buffer[element * 4 + 0],
            buffer[element * 4 + 1],
            buffer[element * 4 + 2],
            buffer[element * 4 + 3],
        ]);

        let opcode = disas::decode(bytes);
        println!("{:#x}", address);
        println!("{opcode}");
        address += 4;
    }
}
