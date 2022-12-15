mod cartridge;
mod cpu;
mod memory;
// mod bus;

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

    pub fn run(&mut self, rom: &Rom) {}
}
