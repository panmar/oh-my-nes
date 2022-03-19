use crate::memory::Address;

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
        const BREAK1 = 0b00100000;
        const BREAK2 = 0b00010000;
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
    handler: fn(cpu: &mut Cpu, memory: &mut Memory, arg_ptr: Option<Address>),
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
    ) -> Option<Address> {
        let arg_ptr = self.program_counter + 1;
        use AddressingMode::*;
        match mode {
            Implicit => None,
            Immidiate => {
                return Some(arg_ptr);
            }
            ZeroPage => {
                return Some(memory.fetch_u8(arg_ptr) as u16);
            }
            ZeroPageX => {
                return Some(memory.fetch_u8(arg_ptr).wrapping_add(self.x_index) as u16);
            }
            ZeroPageY => {
                return Some(memory.fetch_u8(arg_ptr).wrapping_add(self.y_index) as u16);
            }
            Relative => {
                return Some(memory.fetch_u8(arg_ptr) as u16);
            }
            Absolute => {
                return Some(memory.fetch_u16(arg_ptr));
            }
            AbsoluteX => {
                let base = memory.fetch_u16(arg_ptr);
                let offset = self.x_index as u16;
                return Some(base + offset);
            }
            AbsoluteY => {
                let base = memory.fetch_u16(arg_ptr);
                let offset = self.y_index as u16;
                return Some(base + offset);
            }
            Indirect => {
                let address_indirect = memory.fetch_u16(arg_ptr);
                let address = memory.fetch_u16(address_indirect);
                return Some(address);
            }
            IndirectX => {
                let address_indirect_base = memory.fetch_u16(arg_ptr);
                let address_indirect = address_indirect_base.wrapping_add(self.x_index as u16);
                let address = memory.fetch_u16(address_indirect);
                return Some(address);
            }
            IndirectY => {
                let address_indirect_base = memory.fetch_u16(arg_ptr);
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

    fn compare(&mut self, lhs: u8, rhs: u8) {
        self.flags.set(Flags::CARRY, lhs >= rhs);
        self.flags.set(Flags::ZERO, lhs == rhs);
        let result = lhs.wrapping_sub(rhs);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }
}

#[rustfmt::skip]
const INSTRUCTIONS: [Instruction; 2] = [
    Instruction { mnemonic: "BRK", opcode: 0x00, mode: AddressingMode::Implicit, bytesize: 1, cycles: 7, handler: Cpu::brk },
    Instruction { mnemonic: "ORA($NN,X)", opcode: 0x01, mode: AddressingMode::IndirectX, bytesize: 2, cycles: 6, handler: Cpu::ora },
];

impl Cpu {
    fn adc(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());

        let (result, carry_1) = self.accumulator.overflowing_add(arg);
        let (result, carry_2) = result.overflowing_add(self.flags.contains(Flags::CARRY) as u8);

        self.flags.set(Flags::CARRY, carry_1 || carry_2);

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.flags.set(
            Flags::OVERFLOW,
            (self.accumulator ^ result) & (arg ^ result) & 0x80 != 0,
        );

        self.set_accumulator(result);
    }

    fn and(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.set_accumulator(self.accumulator & arg);
    }

    fn asl(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        match arg_ptr {
            Some(address) => {
                let arg = memory.fetch_u8(address);
                let (result, carry) = arg.overflowing_shl(1);
                memory.set_u8(address, result);
                self.flags.set(Flags::ZERO, result == 0);
                self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
                self.flags.set(Flags::CARRY, carry);
            }
            None => {
                let (result, carry) = self.accumulator.overflowing_shl(1);
                self.set_accumulator(result);
                self.flags.set(Flags::CARRY, carry);
            }
        }
    }

    fn bcc(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if !self.flags.contains(Flags::CARRY) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn bcs(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if self.flags.contains(Flags::CARRY) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn beq(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if self.flags.contains(Flags::ZERO) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn bit(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        let result = self.accumulator & arg;
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
        self.flags.set(Flags::OVERFLOW, result & 0b0100_0000 != 0);
    }

    fn bmi(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if self.flags.contains(Flags::NEGATIVE) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn bne(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if !self.flags.contains(Flags::ZERO) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn bpl(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if !self.flags.contains(Flags::NEGATIVE) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn brk(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter);
        self.flags.set(Flags::BREAK2, true);
        memory
            .stack(&mut self.stack_pointer)
            .push_u8(self.flags.bits());
        self.flags.set(Flags::INTERRUPT_DISABLE, true);

        self.program_counter = memory.fetch_u16(0xFFFE);
    }

    fn bvc(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if !self.flags.contains(Flags::OVERFLOW) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn bvs(&mut self, _memory: &mut Memory, arg_ptr: Option<Address>) {
        if self.flags.contains(Flags::OVERFLOW) {
            self.program_counter += arg_ptr.unwrap();
        }
    }

    fn clc(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::CARRY, false);
    }

    fn cld(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::DECIMAL, false);
    }

    fn cli(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::INTERRUPT_DISABLE, false);
    }

    fn clv(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::CARRY, false);
    }

    fn cmp(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.compare(self.accumulator, arg);
    }

    fn cpx(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.compare(self.x_index, arg);
    }

    fn cpy(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.compare(self.y_index, arg);
    }

    fn dec(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let address = arg_ptr.unwrap();
        let arg = memory.fetch_u8(address);
        let result = arg.wrapping_sub(1);
        memory.set_u8(address, result);
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn dex(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.x_index = self.x_index.wrapping_sub(1);
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn dey(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.y_index = self.y_index.wrapping_sub(1);
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn eor(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.accumulator = self.accumulator ^ arg;
        self.flags.set(Flags::ZERO, self.accumulator == 0);
        self.flags
            .set(Flags::NEGATIVE, self.accumulator & 0b1000_0000 != 0);
    }

    fn inc(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let address = arg_ptr.unwrap();
        let arg = memory.fetch_u8(address);
        let result = arg.wrapping_add(1);
        memory.set_u8(address, result);
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn inx(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.x_index = self.x_index.wrapping_add(1);
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn iny(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.y_index = self.y_index.wrapping_add(1);
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn jmp(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.program_counter = arg as u16;
    }

    fn jrs(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());

        // TODO(panmar): Do we change stack pointer here?
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter - 1);
        self.program_counter = arg as u16;
    }

    fn lda(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.set_accumulator(arg);
    }

    fn ldx(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.x_index = arg;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn ldy(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.y_index = arg;
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn lhr(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        match arg_ptr {
            Some(address) => {
                let arg = memory.fetch_u8(address);
                self.flags.set(Flags::CARRY, arg & 1 != 0);
                let (result, _) = arg.overflowing_shr(1);
                memory.set_u8(address, result);
                self.flags.set(Flags::ZERO, result == 0);
                self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
            }
            None => {
                self.flags.set(Flags::CARRY, self.accumulator & 1 != 0);
                let (result, _) = self.accumulator.overflowing_shr(1);
                self.set_accumulator(result);
            }
        }
    }

    fn nop(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {}

    fn ora(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let arg = memory.fetch_u8(arg_ptr.unwrap());
        self.set_accumulator(self.accumulator | arg);
    }

    fn pha(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u8(self.accumulator);
    }

    fn php(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        let mut flags = self.flags;
        flags.insert(Flags::BREAK1 | Flags::BREAK2);
        memory.stack(&mut self.stack_pointer).push_u8(flags.bits());
    }

    fn pla(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        let new_accumulator = memory.stack(&mut self.stack_pointer).pop_u8();
        self.set_accumulator(new_accumulator);
    }

    fn plp(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
        self.flags.remove(Flags::BREAK1 | Flags::BREAK2);
    }

    fn rol(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        match arg_ptr {
            Some(address) => {
                let arg = memory.fetch_u8(address);
                let new_carry = (arg & 0b1000_0000) != 0;
                let mut result = arg.rotate_left(1);
                result &= 0b1111_1110;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b0000_0001;
                }
                memory.set_u8(address, result);
                self.flags.set(Flags::ZERO, result == 0);
                self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
                self.flags.set(Flags::CARRY, new_carry);
            }
            None => {
                let new_carry = (self.accumulator & 0b1000_0000) != 0;
                let mut result = self.accumulator.rotate_left(1);
                result &= 0b1111_1110;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b0000_0001;
                }
                self.set_accumulator(result);
                self.flags.set(Flags::CARRY, new_carry);
            }
        }
    }

    fn ror(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        match arg_ptr {
            Some(address) => {
                let arg = memory.fetch_u8(address);
                let new_carry = (arg & 0b0000_0001) != 0;
                let mut result = arg.rotate_right(1);
                result &= 0b0111_1111;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b1000_0000;
                }
                memory.set_u8(address, result);
                self.flags.set(Flags::ZERO, result == 0);
                self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
                self.flags.set(Flags::CARRY, new_carry);
            }
            None => {
                let new_carry = (self.accumulator & 0b0000_0001) != 0;
                let mut result = self.accumulator.rotate_right(1);
                result &= 0b0111_1111;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b1000_0000;
                }
                self.set_accumulator(result);
                self.flags.set(Flags::CARRY, new_carry);
            }
        }
    }

    fn rti(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16();
    }

    fn rts(&mut self, memory: &mut Memory, _arg_ptr: Option<Address>) {
        // TODO(panmar): Do we need to increments by 1?
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16() + 1;
    }

    fn sbc(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        let mut arg = memory.fetch_u8(arg_ptr.unwrap());
        // A - B = A + (-B) = A + (!B + 1)
        arg = !arg.wrapping_add(1);

        let (result, carry_1) = self.accumulator.overflowing_add(arg);
        // TODO(panmar): Do we need to negate the carry?
        let (result, carry_2) = result.overflowing_add(!self.flags.contains(Flags::CARRY) as u8);

        self.flags.set(Flags::CARRY, !(carry_1 || carry_2));

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.flags.set(
            Flags::OVERFLOW,
            (self.accumulator ^ result) & (arg ^ result) & 0x80 != 0,
        );

        self.set_accumulator(result);
    }

    fn sec(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::CARRY, true);
    }

    fn sed(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::DECIMAL, true);
    }

    fn sei(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.flags.set(Flags::INTERRUPT_DISABLE, true);
    }

    fn sta(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        memory.set_u8(arg_ptr.unwrap(), self.accumulator);
    }

    fn stx(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        memory.set_u8(arg_ptr.unwrap(), self.x_index);
    }

    fn sty(&mut self, memory: &mut Memory, arg_ptr: Option<Address>) {
        memory.set_u8(arg_ptr.unwrap(), self.y_index);
    }

    fn tax(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.x_index = self.accumulator;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn tay(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.y_index = self.accumulator;
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn tsx(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.x_index = self.stack_pointer;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn txa(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.set_accumulator(self.x_index);
    }

    fn txs(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.stack_pointer = self.x_index;
    }

    fn tya(&mut self, _memory: &mut Memory, _arg_ptr: Option<Address>) {
        self.set_accumulator(self.y_index);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn set_operand(memory: &mut Memory, value: u8) -> Option<Address> {
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
            let arg_ptr = set_operand(&mut memory, opperand);
            cpu.flags.set(Flags::CARRY, carry_flag);
            let prev_cpu_flags = cpu.flags;

            // when
            cpu.adc(&mut memory, arg_ptr);

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
