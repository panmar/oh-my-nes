//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 |    PRG-ROM    |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       |     SRAM      |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       |               |
// | $2000-$2007   |       | I/O Registers |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       |               |
// |_ _ _ _ _ _ _ _| $0200 |      RAM      |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|

use super::cartridge::Rom;

const MEMORY_SIZE: usize = 0xFFFF + 1;
const STACK_BEGIN: usize = 0x0100;
const STACK_END: usize = 0x01FF;

const RAM_BEGIN: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const PRG_ROM_BEGIN: usize = 0x8000;

pub type Address = u16;

pub struct Memory {
    data: [u8; MEMORY_SIZE],
    watches: Vec<Watch>,
}

struct Watch {
    address: Address,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            data: [0; MEMORY_SIZE],
            watches: Vec::new(),
        }
    }

    pub fn insert_cartridge(&mut self, rom: Rom) {
        // PRG Rom Size might be 16 kB or 32 kB.
        // Because [0x8000 … 0x10000] mapped region is 32 kB of addressable space,
        // the upper 16 kB needs to be mapped to the lower 16 kB (if a game has only 16 kB of PRG ROM)
        // println!("PGR_ROM size = {:#0X}", rom.prg_rom.len());
        if rom.prg_rom.len() != 0x4000 && rom.prg_rom.len() != 0x8000 {
            panic!("Incorrect PRG_ROM size");
        }

        if rom.prg_rom.len() <= 0x4000 {
            self.data[PRG_ROM_BEGIN..PRG_ROM_BEGIN + rom.prg_rom.len()]
                .copy_from_slice(&rom.prg_rom);
            self.data[0xC000..0xC000 + rom.prg_rom.len()].copy_from_slice(&rom.prg_rom);
        } else {
            self.data[PRG_ROM_BEGIN..PRG_ROM_BEGIN + rom.prg_rom.len()]
                .copy_from_slice(&rom.prg_rom);
        }
    }

    pub fn read_u8(&self, address: Address) -> u8 {
        let effective_address = Memory::compute_effective_address(address) as usize;
        self.data[effective_address]
    }

    pub fn read_u8_for_writing(&mut self, address: Address) -> &mut u8 {
        let effective_address = Memory::compute_effective_address(address) as usize;
        &mut self.data[effective_address]
    }

    pub fn read_u16(&self, address: Address) -> u16 {
        u16::from_le_bytes([self.read_u8(address), self.read_u8(address + 1)])
    }

    pub fn write_u8(&mut self, address: Address, value: u8) {
        let effective_address = Memory::compute_effective_address(address) as usize;

        for watch in &self.watches {
            if watch.address == effective_address as Address {
                println!("Watch address {:04X} is being written", watch.address);
            }
        }

        self.data[effective_address] = value;
    }

    pub fn write_u16(&mut self, address: Address, value: u16) {
        let bytes = value.to_le_bytes();
        self.write_u8(address, bytes[0]);
        self.write_u8(address + 1, bytes[1]);
    }

    // TODO(panmar): Should we distinguish between read and write?
    // (so write does not mutate readonly cartridge memory)
    fn compute_effective_address(address: Address) -> Address {
        let effective_address = match address {
            0x0800..=0x1FFF => address & 0b00_0111_1111_1111,
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => address & 0b0010_0000_0000_0111,
            _ => address,
        };
        return effective_address;
    }
}

impl Memory {
    pub fn add_watch(&mut self, address: Address) {
        self.watches.push(Watch { address });
    }
}

pub struct Stack<'a> {
    memory: &'a mut Memory,
    stack_pointer: &'a mut u8,
}

impl Memory {
    pub fn stack<'a>(&'a mut self, stack_pointer: &'a mut u8) -> Stack<'a> {
        Stack {
            memory: self,
            stack_pointer,
        }
    }
}

impl Stack<'_> {
    pub fn read_u8(&self) -> u8 {
        let address = STACK_BEGIN as Address + self.stack_pointer.wrapping_add(1) as Address;
        return self.memory.read_u8(address);
    }

    pub fn write_u8(&mut self, address: Address, value: u8) {
        self.memory
            .write_u8(STACK_BEGIN as Address + address, value);
    }

    pub fn push_u8(&mut self, value: u8) {
        self.memory.write_u8(
            STACK_BEGIN as Address + *self.stack_pointer as Address,
            value,
        );
        *self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    pub fn push_u16(&mut self, value: u16) {
        let data_bytes = value.to_be_bytes();
        for byte in data_bytes {
            self.push_u8(byte);
        }
    }

    pub fn pop_u8(&mut self) -> u8 {
        *self.stack_pointer = self.stack_pointer.wrapping_add(1);
        return self
            .memory
            .read_u8(STACK_BEGIN as Address + *self.stack_pointer as Address);
    }

    pub fn pop_u16(&mut self) -> u16 {
        let lo = self.pop_u8();
        let hi = self.pop_u8();
        return u16::from_be_bytes([hi, lo]);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const STACK_POINTER_ADDRESS: u8 = 0xff;

    #[test]
    fn should_push_and_pop_from_stack() {
        // given
        let mut memory = Memory::new();
        let mut stack_pointer = STACK_POINTER_ADDRESS;
        let mut stack = memory.stack(&mut stack_pointer);

        // when
        stack.push_u8(0xab);
        stack.push_u8(0xff);
        stack.push_u16(0xb71f);
        stack.push_u8(0x00);
        stack.push_u16(0xffff);

        // then
        assert_eq!(0xffff, stack.pop_u16());
        assert_eq!(0x00, stack.pop_u8());
        assert_eq!(0xb71f, stack.pop_u16());
        assert_eq!(0xff, stack.pop_u8());
        assert_eq!(0xab, stack.pop_u8());
    }

    #[test]
    fn should_pop_from_empty_stack_wrapping_around() {
        // given
        let mut memory = Memory::new();
        memory.data[STACK_BEGIN] = 0x42;
        let mut stack_pointer = STACK_POINTER_ADDRESS;
        let mut stack = memory.stack(&mut stack_pointer);

        // when
        let value = stack.pop_u8();

        // then
        assert_eq!(value, 0x42);
    }

    #[test]
    fn should_push_to_full_stack_wrapping_around() {
        // given
        let mut memory = Memory::new();
        let mut stack_pointer = 0x00u8;
        let mut stack = memory.stack(&mut stack_pointer);

        // when
        stack.push_u8(0x42);
        stack.push_u8(0x42);

        // then
        assert_eq!(memory.data[STACK_BEGIN], 0x42);
        assert_eq!(memory.data[STACK_END], 0x42);
    }
}
