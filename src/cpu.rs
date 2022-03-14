use super::memory::Memory;
use bitflags::bitflags;

struct Cpu {
    accumulator: u8,
    x_index: u8,
    y_index: u8,

    flags: Flags,

    stack_pointer: u8,
    program_counter: u16,
}

bitflags! {
    struct Flags: u8 {
        const NEGATIVE = 0b10000000;
        const OVERFLOW = 0b01000000;
        const RESERVED = 0b00100000;
        const BREAK = 0b00010000;
        const DECIMAL = 0b00001000;
        const INTERRUPT_DISABLE = 0b00000100;
        const ZERO = 0b00000010;
        const CARRY = 0b00000001;
    }
}

struct Instruction {
    mnemonic: &'static str,
    opcode: u8,
    mode: AddressingMode,
    bytesize: u8,
    cycles: u8,
    handler: fn(cpu: &mut Cpu, memory: &mut Memory, param_address: Option<u16>),
}

#[derive(Clone, Copy)]
enum AddressingMode {
    Implicit,
    Immidiate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            accumulator: 0,
            x_index: 0,
            y_index: 0,
            flags: Flags::empty(),
            stack_pointer: 0,
            program_counter: 0,
        }
    }

    fn decode_and_execute(&mut self, memory: &mut Memory) {
        let opcode = memory.fetch_u8(self.program_counter);
        let instruction = &INSTRUCTIONS[opcode as usize];
        let param_address = self.fetch_instruction_param_address(memory, instruction.mode);

        self.program_counter += instruction.bytesize as u16;
        (instruction.handler)(self, memory, param_address);
    }

    fn fetch_instruction_param_address(&self, memory: &Memory, mode: AddressingMode) -> Option<u16> {
        let operand_address = self.program_counter + 1;
        use AddressingMode::*;
        match mode {
            Implicit => None,
            Immidiate => {
                return Some(operand_address);
            }
            ZeroPage => {
                return Some(memory.fetch_u8(operand_address) as u16);
            }
            ZeroPageX => {
                return Some(
                    memory
                        .fetch_u8(operand_address)
                        .wrapping_add(self.x_index) as u16,
                );
            }
            ZeroPageY => {
                return Some(
                    memory
                        .fetch_u8(operand_address)
                        .wrapping_add(self.y_index) as u16,
                );
            }
            Relative => {
                return Some(memory.fetch_u8(operand_address) as u16);
            }
            Absolute => {
                return Some(memory.fetch_u16(operand_address));
            }
            AbsoluteX => {
                let base = memory.fetch_u16(operand_address);
                let offset = self.x_index as u16;
                return Some(base + offset);
            }
            AbsoluteY => {
                let base = memory.fetch_u16(operand_address);
                let offset = self.y_index as u16;
                return Some(base + offset);
            }
            Indirect => {
                let address_indirect = memory.fetch_u16(operand_address);
                let address = memory.fetch_u16(address_indirect);
                return Some(address);
            }
            IndirectX => {
                let address_indirect_base = memory.fetch_u16(operand_address);
                let address_indirect = address_indirect_base.wrapping_add(self.x_index as u16);
                let address = memory.fetch_u16(address_indirect);
                return Some(address);
            }
            IndirectY => {
                let address_indirect_base = memory.fetch_u16(operand_address);
                let address_indirect = address_indirect_base.wrapping_add(self.y_index as u16);
                let address = memory.fetch_u16(address_indirect);
                return Some(address);
            }
        }
    }
}

#[repr(u8)]
enum InstructionOpcode {
    BRK_IMPLICIT = 0x00,
}

#[rustfmt::skip]
const INSTRUCTIONS: [Instruction; 2] = [
    Instruction { mnemonic: "BRK", opcode: 0x00, mode: AddressingMode::Implicit, bytesize: 1, cycles: 7, handler: Cpu::brk },
    Instruction { mnemonic: "ORA($NN,X)", opcode: 0x01, mode: AddressingMode::IndirectX, bytesize: 2, cycles: 6, handler: Cpu::ora },
];

impl Cpu {
    fn brk(&mut self, memory: &mut Memory, _param_address: Option<u16>) {
        memory.stack_push_u16(&mut self.stack_pointer, self.program_counter);
        memory.stack_push_u8(&mut self.stack_pointer, self.flags.bits());
    }

    fn ora(&mut self, memory: &mut Memory, param_address: Option<u16>) {
        let address = param_address.unwrap();
        self.accumulator |= memory.fetch_u8(address);
        self.flags.set(
            Flags::ZERO,
            if self.accumulator == 0 { true } else { false },
        );
        self.flags.set(
            Flags::NEGATIVE,
            if self.accumulator.leading_ones() > 0 {
                true
            } else {
                false
            },
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_execute_brk() {
        // given
        let mut cpu = Cpu::new();
        let mut memory = Memory::new();

        // when
        cpu.brk(&mut memory, None);

        // then
        assert_eq!(
            memory.stack_pop_u8(&mut cpu.stack_pointer),
            cpu.flags.bits()
        );
        assert_eq!(
            memory.stack_pop_u16(&mut cpu.stack_pointer),
            cpu.program_counter
        );
    }
}
