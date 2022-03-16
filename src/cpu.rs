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
    handler: fn(cpu: &mut Cpu, memory: &mut Memory, operand_address: Option<u16>),
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
            stack_pointer: 0xFF,
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

    fn fetch_instruction_param_address(
        &self,
        memory: &Memory,
        mode: AddressingMode,
    ) -> Option<u16> {
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
                return Some(memory.fetch_u8(operand_address).wrapping_add(self.x_index) as u16);
            }
            ZeroPageY => {
                return Some(memory.fetch_u8(operand_address).wrapping_add(self.y_index) as u16);
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

impl Cpu {
    fn set_accumulator(&mut self, value: u8) {
        self.accumulator = value;
        self.flags.set(Flags::ZERO, self.accumulator == 0);
        self.flags
            .set(Flags::NEGATIVE, self.accumulator & 0b1000_0000 != 0);
    }
}

#[rustfmt::skip]
const INSTRUCTIONS: [Instruction; 2] = [
    Instruction { mnemonic: "BRK", opcode: 0x00, mode: AddressingMode::Implicit, bytesize: 1, cycles: 7, handler: Cpu::brk },
    Instruction { mnemonic: "ORA($NN,X)", opcode: 0x01, mode: AddressingMode::IndirectX, bytesize: 2, cycles: 6, handler: Cpu::ora },
];

impl Cpu {
    fn adc(&mut self, memory: &mut Memory, operand_address: Option<u16>) {
        let operand = memory.fetch_u8(operand_address.unwrap());

        let (result, carry_1) = self.accumulator.overflowing_add(operand);
        let (result, carry_2) = result.overflowing_add(self.flags.contains(Flags::CARRY) as u8);

        self.flags.set(Flags::CARRY, carry_1 || carry_2);

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.flags.set(
            Flags::OVERFLOW,
            (self.accumulator ^ result) & (operand ^ result) & 0x80 != 0,
        );

        self.set_accumulator(result);
    }

    fn brk(&mut self, memory: &mut Memory, _operand_address: Option<u16>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter);
        memory
            .stack(&mut self.stack_pointer)
            .push_u8(self.flags.bits());
    }

    fn ora(&mut self, memory: &mut Memory, operand_address: Option<u16>) {
        let operand = memory.fetch_u8(operand_address.unwrap());
        self.set_accumulator(self.accumulator | operand);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn set_operand(memory: &mut Memory, value: u8) -> Option<u16> {
        let address = 0x0042;
        memory.set_u8(address, value);
        return Some(address);
    }

    #[test]
    fn should_execute_adc() {
        fn test_adc(
            accumulator: u8,
            opperand: u8,
            carry_flag: bool,
            expected_accumulator: u8,
            expected_flags: Flags,
        ) {
            let affected_flags = Flags::CARRY | Flags::ZERO | Flags::OVERFLOW | Flags::NEGATIVE;
            let expected_unaffected_flags = !affected_flags;
            let expected_unset_flags = affected_flags.difference(expected_flags);

            // given
            let mut cpu = Cpu::new();
            cpu.accumulator = accumulator;
            let mut memory = Memory::new();
            let operand_address = set_operand(&mut memory, opperand);
            cpu.flags.set(Flags::CARRY, carry_flag);
            let prev_cpu_flags = cpu.flags;

            // when
            cpu.adc(&mut memory, operand_address);

            // then
            assert_eq!(cpu.accumulator, expected_accumulator);
            assert!(cpu.flags.contains(expected_flags));
            assert!(!cpu.flags.intersects(expected_unset_flags));

            assert_eq!(
                cpu.flags.intersection(expected_unaffected_flags),
                prev_cpu_flags.intersection(expected_unaffected_flags),
            );
        }

        test_adc(0x00, 0x00, false, 0x00, Flags::ZERO);
        test_adc(0x77, 0x03, false, 0x7A, Flags::empty());
        test_adc(0x40, 0x40, false, 0x80, Flags::NEGATIVE | Flags::OVERFLOW);
        test_adc(0xFF, 0xFF, true, 0xFF, Flags::NEGATIVE | Flags::CARRY);
    }

    #[test]
    fn should_execute_brk() {
        // given
        let mut cpu = Cpu::new();
        cpu.program_counter = 0x1234;
        cpu.flags.bits = 0b10111010;
        let mut memory = Memory::new();

        // when
        cpu.brk(&mut memory, None);

        // then
        assert_eq!(
            memory.stack(&mut cpu.stack_pointer).pop_u8(),
            cpu.flags.bits()
        );
        assert_eq!(
            memory.stack(&mut cpu.stack_pointer).pop_u16(),
            cpu.program_counter
        );
    }
}
