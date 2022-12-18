mod cartridge;
mod cpu;
mod memory;

use std::{
    fs::{self, File},
    io::Read,
    path::Path,
};

use cartridge::Rom;
use cpu::Cpu;
use memory::Memory;

pub struct Emulator {
    cpu: Cpu,
    memory: Memory,
}

impl Emulator {
    pub fn new() -> Emulator {
        Emulator {
            cpu: Cpu::new(),
            memory: Memory::new(),
        }
    }

    pub fn run(&mut self, rom_path: &Path) {
        let buffer = Emulator::read_rom_file(rom_path).expect("Cannot read rom file");
        let result_rom = Rom::new(&buffer);
        match result_rom {
            Ok(rom) => self.memory.insert_cartridge(rom),
            Err(error) => panic!("{}", error),
        }

        self.cpu.execute_all(&mut self.memory);
    }

    fn read_rom_file(rom_path: &Path) -> Option<Vec<u8>> {
        let mut file = File::open(rom_path).expect("No ROM file found");
        let file_metadata = fs::metadata(&rom_path).expect("Unable to read ROM metadata");
        let mut buffer = vec![0; file_metadata.len() as usize];
        file.read(&mut buffer).expect("Buffer overflow");
        Some(buffer)
    }
}
