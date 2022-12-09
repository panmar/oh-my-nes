mod cpu;
mod memory;

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

    pub fn run(&mut self, rom: &Rom) {}
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}
