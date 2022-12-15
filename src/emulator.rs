mod cartridge;
mod cpu;
mod memory;
// mod bus;

use std::{path::Path, fs::{File, self}, io::Read};

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
        let mut file = File::open(rom_path).expect("No ROM file found");
        let file_metadata = fs::metadata(&rom_path).expect("Unable to read ROM metadata");
        let mut buffer = vec![0; file_metadata.len() as usize];
        file.read(&mut buffer).expect("Buffer overflow");

        let result_rom = Rom::new(&buffer);
        match result_rom {
            Ok(rom) => self.memory.insert_cartridge(rom),
            Err(error) => panic!("{}", error),
        }

        self.cpu.run_with_debug_info(&mut self.memory);
    }
}
