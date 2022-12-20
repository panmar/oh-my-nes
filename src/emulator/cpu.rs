// TODO: Remove me
#![allow(dead_code)]

mod test;

use super::memory::Address;
use super::memory::Memory;
use bitflags::bitflags;

#[derive(Debug)]
pub struct Cpu {
    accumulator: u8,
    x_index: u8,
    y_index: u8,
    flags: Flags,
    stack_pointer: u8,
    program_counter: u16,
}

bitflags! {
    struct Flags: u8 {
        const CARRY = 0b00000001;
        const ZERO = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL = 0b00001000;
        const BREAK1 = 0b00010000;
        const BREAK2 = 0b00100000;
        const OVERFLOW = 0b01000000;
        const NEGATIVE = 0b10000000;
    }
}

#[derive(Clone, Copy)]
enum AddressingMode {
    Accumulator,
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

struct Instruction {
    mnemonic: &'static str,
    opcode: u8,
    mode: AddressingMode,
    bytesize: u8,
    cycles: u8,
    handler: fn(cpu: &mut Cpu, memory: &mut Memory, arg_address: Option<Address>),
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            accumulator: 0,
            x_index: 0,
            y_index: 0,
            flags: Flags::INTERRUPT_DISABLE | Flags::BREAK2,
            stack_pointer: 0xFD,
            program_counter: 0x8000,
        }
    }

    pub fn execute_all(&mut self, memory: &mut Memory) {
        let mut count = 0u32;
        loop {
            count += 1;
            self.decode_and_execute(memory);
            if count == 400 {
                break;
            }
        }
    }

    pub fn decode_and_execute(&mut self, memory: &mut Memory) {
        print!("{:04X}  ", self.program_counter);

        let opcode = memory.read_u8(self.program_counter);
        let instruction = &INSTRUCTIONS[opcode as usize];

        let m1 = memory.read_u8(self.program_counter);
        let m2 = memory.read_u8(self.program_counter + 1);
        let m3 = memory.read_u8(self.program_counter + 2);

        let instruction_in_memory = match instruction.bytesize {
            1 => format!("{:02X}", m1),
            2 => format!("{:02X} {:02X}", m1, m2),
            3 => format!("{:02X} {:02X} {:02X}", m1, m2, m3),
            _ => "".to_string(),
        };
        print!("{:10}", instruction_in_memory);

        let mnemonic_address = match instruction.mnemonic {
            x if x.contains("$NNNN") => format!("{:02X}{:02X}", m3, m2),
            x if x.contains("$NN") => format!("{:02X}", m2),
            _ => "".to_string(),
        };

        let mut mnemonic: String = instruction.mnemonic.clone().to_string();
        if instruction.mnemonic.contains("NNNN") {
            mnemonic = instruction.mnemonic.replace("NNNN", &mnemonic_address);
        } else if instruction.mnemonic.contains("NN") {
            let absolute_address = format!(
                "{:04X}",
                self.program_counter as i32 + instruction.bytesize as i32 + m2 as i32
            );
            if instruction.mnemonic.contains("BCS")
                || instruction.mnemonic.contains("BCS")
                || instruction.mnemonic.contains("BCC")
                || instruction.mnemonic.contains("BCC")
                || instruction.mnemonic.contains("BEQ")
                || instruction.mnemonic.contains("BNE")
                || instruction.mnemonic.contains("BVS")
                || instruction.mnemonic.contains("BVC")
                || instruction.mnemonic.contains("BPL")
                || instruction.mnemonic.contains("BMI")
            {
                mnemonic = instruction.mnemonic.replace("NN", &absolute_address);
            } else {
                mnemonic = instruction.mnemonic.replace("NN", &mnemonic_address);
            }
        }

        let mnemonic = match mnemonic {
            m if m.contains("STA") => format!("{} = {:02X}", m, self.accumulator),
            m if m.contains("STX") => format!("{} = {:02X}", m, self.x_index),
            m if m.contains("STY") => format!("{} = {:02X}", m, self.y_index),
            m if m.contains("BIT") => format!("{} = {:02X}", m, self.accumulator),
            _ => mnemonic,
        };

        print!("{:32}", mnemonic);

        println!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.accumulator, self.x_index, self.y_index, self.flags, self.stack_pointer
        );

        let param_address = self.fetch_instruction_param_address(memory, instruction.mode);
        self.program_counter += instruction.bytesize as u16;
        (instruction.handler)(self, memory, param_address);
    }

    fn fetch_instruction_param_address(
        &self,
        memory: &Memory,
        mode: AddressingMode,
    ) -> Option<Address> {
        let arg_address = self.program_counter + 1;
        use AddressingMode::*;
        match mode {
            Accumulator => None,
            Implicit => None,
            Immidiate => {
                return Some(arg_address);
            }
            ZeroPage => {
                return Some(memory.read_u8(arg_address) as u16);
            }
            ZeroPageX => {
                return Some(memory.read_u8(arg_address).wrapping_add(self.x_index) as u16);
            }
            ZeroPageY => {
                return Some(memory.read_u8(arg_address).wrapping_add(self.y_index) as u16);
            }
            Relative => {
                return Some(memory.read_u8(arg_address) as u16);
            }
            Absolute => {
                return Some(memory.read_u16(arg_address));
            }
            AbsoluteX => {
                let base = memory.read_u16(arg_address);
                let offset = self.x_index as u16;
                return Some(base + offset);
            }
            AbsoluteY => {
                let base = memory.read_u16(arg_address);
                let offset = self.y_index as u16;
                return Some(base + offset);
            }
            Indirect => {
                let address_indirect = memory.read_u16(arg_address);
                let address = memory.read_u16(address_indirect);
                return Some(address);
            }
            IndirectX => {
                let address_indirect_base = memory.read_u16(arg_address);
                let address_indirect = address_indirect_base.wrapping_add(self.x_index as u16);
                let address = memory.read_u16(address_indirect);
                return Some(address);
            }
            IndirectY => {
                let address_indirect_base = memory.read_u16(arg_address);
                let address_indirect = address_indirect_base.wrapping_add(self.y_index as u16);
                let address = memory.read_u16(address_indirect);
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
            .set(Flags::NEGATIVE, (self.accumulator & 0b1000_0000) != 0);
    }

    fn add_with_carry(lhs: u8, rhs: u8, carry: bool) -> (u8, bool, bool) {
        let (result, carry_1) = lhs.overflowing_add(rhs);
        let (result, carry_2) = result.overflowing_add(carry as u8);

        let new_carry = carry_1 || carry_2;

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        let overflow = (lhs ^ result) & (rhs ^ result) & 0x80 != 0;

        (result, new_carry, overflow)
    }

    fn compare(&mut self, lhs: u8, rhs: u8) {
        self.flags.set(Flags::CARRY, lhs >= rhs);
        self.flags.set(Flags::ZERO, lhs == rhs);
        let result = lhs.wrapping_sub(rhs);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn jump(&mut self, relative_address: i8) {
        let jump_address = self.program_counter as i32 + relative_address as i32;
        self.program_counter = jump_address as u16;
    }
}

impl Cpu {
    fn adc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        let (accum, carry, overflow) =
            Cpu::add_with_carry(self.accumulator, arg, self.flags.contains(Flags::CARRY));
        self.set_accumulator(accum);
        self.flags.set(Flags::CARRY, carry);
        self.flags.set(Flags::OVERFLOW, overflow);
    }

    fn and(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.set_accumulator(self.accumulator & arg);
    }

    fn asl(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        fn asl_internal(flags: &mut Flags, arg_ref: &mut u8) {
            flags.set(Flags::CARRY, *arg_ref & 0b1000_0000 != 0);
            *arg_ref <<= 1;
            flags.set(Flags::ZERO, *arg_ref == 0);
            flags.set(Flags::NEGATIVE, *arg_ref & 0b1000_0000 != 0);
        }

        let arg_ref = match arg_address {
            Some(address) => memory.read_u8_for_writing(address),
            None => &mut self.accumulator,
        };

        asl_internal(&mut self.flags, arg_ref);
    }

    fn bcc(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::CARRY) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn bcs(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::CARRY) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn beq(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::ZERO) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn bit(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        let result = self.accumulator & arg;
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, (arg & 0b1000_0000) != 0);
        self.flags.set(Flags::OVERFLOW, (arg & 0b0100_0000) != 0);
    }

    fn bmi(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::NEGATIVE) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn bne(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::ZERO) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn bpl(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::NEGATIVE) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn brk(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter);
        self.flags.set(Flags::BREAK2, true);
        memory
            .stack(&mut self.stack_pointer)
            .push_u8(self.flags.bits());
        self.flags.set(Flags::INTERRUPT_DISABLE, true);

        self.program_counter = memory.read_u16(0xFFFE);
    }

    fn bvc(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::OVERFLOW) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn bvs(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::OVERFLOW) {
            self.jump(arg_address.unwrap() as i8);
        }
    }

    fn clc(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::CARRY, false);
    }

    fn cld(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::DECIMAL, false);
    }

    fn cli(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::INTERRUPT_DISABLE, false);
    }

    fn clv(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::OVERFLOW, false);
    }

    fn cmp(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.compare(self.accumulator, arg);
    }

    fn cpx(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.compare(self.x_index, arg);
    }

    fn cpy(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.compare(self.y_index, arg);
    }

    fn dec(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let address = arg_address.unwrap();
        let arg = memory.read_u8(address);
        let result = arg.wrapping_sub(1);
        memory.write_u8(address, result);
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn dex(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.x_index = self.x_index.wrapping_sub(1);
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn dey(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.y_index = self.y_index.wrapping_sub(1);
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn eor(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.accumulator = self.accumulator ^ arg;
        self.flags.set(Flags::ZERO, self.accumulator == 0);
        self.flags
            .set(Flags::NEGATIVE, self.accumulator & 0b1000_0000 != 0);
    }

    fn inc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let address = arg_address.unwrap();
        let arg = memory.read_u8(address);
        let result = arg.wrapping_add(1);
        memory.write_u8(address, result);
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn inx(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.x_index = self.x_index.wrapping_add(1);
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn iny(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.y_index = self.y_index.wrapping_add(1);
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn jmp(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        self.program_counter = arg_address.unwrap();
    }

    fn jsr(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter);
        self.program_counter = arg_address.unwrap();
    }

    fn lda(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.set_accumulator(arg);
    }

    fn ldx(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.x_index = arg;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn ldy(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.y_index = arg;
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn lsr(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        fn lsr_internal(flags: &mut Flags, arg_ref: &mut u8) {
            flags.set(Flags::CARRY, *arg_ref & 0b0000_0001 != 0);
            *arg_ref >>= 1;
            flags.set(Flags::ZERO, *arg_ref == 0);
            flags.set(Flags::NEGATIVE, *arg_ref & 0b1000_0000 != 0);
        }

        let arg_ref = match arg_address {
            Some(address) => memory.read_u8_for_writing(address),
            None => &mut self.accumulator,
        };

        lsr_internal(&mut self.flags, arg_ref);
    }

    fn nop(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {}

    fn ora(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.set_accumulator(self.accumulator | arg);
    }

    fn pha(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        memory
            .stack(&mut self.stack_pointer)
            .push_u8(self.accumulator);
    }

    fn php(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let flags = self.flags;
        memory.stack(&mut self.stack_pointer).push_u8(flags.bits());
    }

    fn pla(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_accumulator = memory.stack(&mut self.stack_pointer).pop_u8();
        self.set_accumulator(new_accumulator);
    }

    fn plp(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
    }

    fn rol(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        fn rol_internal(flags: &mut Flags, arg_ref: &mut u8) {
            let old_carry = flags.contains(Flags::CARRY) as u8;
            flags.set(Flags::CARRY, *arg_ref & 0b1000_0000 != 0);
            *arg_ref <<= 1;
            *arg_ref |= old_carry;
            flags.set(Flags::ZERO, *arg_ref == 0);
            flags.set(Flags::NEGATIVE, *arg_ref & 0b1000_0000 != 0);
        }

        let arg_ref = match arg_address {
            Some(address) => memory.read_u8_for_writing(address),
            None => &mut self.accumulator,
        };

        rol_internal(&mut self.flags, arg_ref);
    }

    fn ror(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        fn ror_internal(flags: &mut Flags, arg_ref: &mut u8) {
            let old_carry = (flags.contains(Flags::CARRY) as u8) << 7;
            flags.set(Flags::CARRY, *arg_ref & 0b0000_0001 != 0);
            *arg_ref >>= 1;
            *arg_ref |= old_carry;
            flags.set(Flags::ZERO, *arg_ref == 0);
            flags.set(Flags::NEGATIVE, *arg_ref & 0b1000_0000 != 0);
        }

        let arg_ref = match arg_address {
            Some(address) => memory.read_u8_for_writing(address),
            None => &mut self.accumulator,
        };

        ror_internal(&mut self.flags, arg_ref);
    }

    fn rti(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16();
    }

    fn rts(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16();
    }

    fn sbc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let mut arg = memory.read_u8(arg_address.unwrap());
        // A - B = A + (-B) = A + (!B + 1)
        arg = arg.wrapping_neg().wrapping_sub(1);
        let (accum, carry, overflow) =
            Cpu::add_with_carry(self.accumulator, arg, self.flags.contains(Flags::CARRY));
        self.set_accumulator(accum);
        self.flags.set(Flags::CARRY, carry);
        self.flags.set(Flags::OVERFLOW, overflow);
    }

    fn sec(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::CARRY, true);
    }

    fn sed(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::DECIMAL, true);
    }

    fn sei(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.flags.set(Flags::INTERRUPT_DISABLE, true);
    }

    fn sta(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        memory.write_u8(arg_address.unwrap(), self.accumulator);
    }

    fn stx(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        memory.write_u8(arg_address.unwrap(), self.x_index);
    }

    fn sty(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        memory.write_u8(arg_address.unwrap(), self.y_index);
    }

    fn tax(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.x_index = self.accumulator;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn tay(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.y_index = self.accumulator;
        self.flags.set(Flags::ZERO, self.y_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.y_index & 0b1000_0000 != 0);
    }

    fn tsx(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.x_index = self.stack_pointer;
        self.flags.set(Flags::ZERO, self.x_index == 0);
        self.flags
            .set(Flags::NEGATIVE, self.x_index & 0b1000_0000 != 0);
    }

    fn txa(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.set_accumulator(self.x_index);
    }

    fn txs(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.stack_pointer = self.x_index;
    }

    fn tya(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        self.set_accumulator(self.y_index);
    }

    fn ude(&mut self, _memory: &mut Memory, _arg_address: Option<Address>) {
        panic!("Undefined instruction");
    }
}

macro_rules! instruction {
    ($mnemonic: literal, $opcode: literal, $mode: expr, $bytesize: literal, $cycles: literal, $handler: expr) => {
        Instruction {
            mnemonic: $mnemonic,
            opcode: $opcode,
            mode: $mode,
            bytesize: $bytesize,
            cycles: $cycles,
            handler: $handler,
        }
    };
}

macro_rules! undefined_instruction {
    ($opcode: literal) => {
        instruction!("UDE", $opcode, AddressingMode::Implicit, 0, 0, Cpu::ude)
    };
}

#[rustfmt::skip]
const INSTRUCTIONS: [Instruction; 256] = [
    instruction!("BRK", 0x00, AddressingMode::Implicit, 1, 7, Cpu::brk),
    instruction!("ORA($NN,X)", 0x01, AddressingMode::IndirectX, 2, 6, Cpu::ora),
    undefined_instruction!(0x02),
    undefined_instruction!(0x03),
    undefined_instruction!(0x04),
    instruction!("ORA $NN", 0x05, AddressingMode::ZeroPage, 2, 0, Cpu::ora),
    instruction!("ASL $NN", 0x06, AddressingMode::ZeroPage, 2, 0, Cpu::asl),
    undefined_instruction!(0x07),  
    instruction!("PHP", 0x08, AddressingMode::Implicit, 1, 0, Cpu::php),
    instruction!("ORA #$NN", 0x09, AddressingMode::Immidiate, 2, 0, Cpu::ora),
    instruction!("ASL A", 0x0A, AddressingMode::Accumulator, 1, 0, Cpu::asl),
    undefined_instruction!(0x0B),
    undefined_instruction!(0x0C),
    instruction!("ORA $NNNN", 0x0D, AddressingMode::Absolute, 3, 0, Cpu::ora),
    instruction!("ASL $NNNN", 0x0E, AddressingMode::Absolute, 3, 0, Cpu::asl),
    undefined_instruction!(0x0F),

    instruction!("BPL $NN", 0x10, AddressingMode::Relative, 2, 0, Cpu::bpl),
    instruction!("ORA ($NN),Y", 0x11, AddressingMode::IndirectY, 2, 0, Cpu::ora),
    undefined_instruction!(0x12),
    undefined_instruction!(0x13),
    undefined_instruction!(0x14),
    instruction!("ORA $NN,X", 0x15, AddressingMode::ZeroPageX, 2, 0, Cpu::ora),
    instruction!("ASL $NN,X", 0x16, AddressingMode::ZeroPageX, 2, 0, Cpu::asl),
    undefined_instruction!(0x17),
    instruction!("CLC", 0x18, AddressingMode::Implicit, 1, 0, Cpu::clc),
    instruction!("ORA $NNNN,Y", 0x19, AddressingMode::AbsoluteY, 3, 0, Cpu::ora),
    undefined_instruction!(0x1A),
    undefined_instruction!(0x1B),
    undefined_instruction!(0x1C),
    instruction!("ORA $NNNN,X", 0x1D, AddressingMode::AbsoluteX, 3, 0, Cpu::ora),
    instruction!("ASL $NNNN,X", 0x1E, AddressingMode::AbsoluteX, 3, 0, Cpu::asl),
    undefined_instruction!(0x1F),

    instruction!("JSR $NNNN", 0x20, AddressingMode::Absolute, 3, 0, Cpu::jsr),
    instruction!("AND ($NN,X)", 0x21, AddressingMode::IndirectX, 2, 0, Cpu::and),
    undefined_instruction!(0x22),
    undefined_instruction!(0x23),
    instruction!("BIT $NN", 0x24, AddressingMode::ZeroPage, 2, 0, Cpu::bit),
    instruction!("AND $NN", 0x25, AddressingMode::ZeroPage, 2, 0, Cpu::and),
    instruction!("ROL $NN", 0x26, AddressingMode::ZeroPage, 2, 0, Cpu::rol),
    undefined_instruction!(0x27),
    instruction!("PLP", 0x28, AddressingMode::Implicit, 1, 0, Cpu::plp),
    instruction!("AND #$NN", 0x29, AddressingMode::Immidiate, 2, 0, Cpu::and),
    instruction!("ROL A", 0x2A, AddressingMode::Accumulator, 1, 0, Cpu::rol),
    undefined_instruction!(0x2B),
    instruction!("BIT $NNNN", 0x2C, AddressingMode::Absolute, 3, 0, Cpu::bit),
    instruction!("AND $NNNN", 0x2D, AddressingMode::Absolute, 3, 0, Cpu::and),
    instruction!("ROL $NNNN", 0x2E, AddressingMode::Absolute, 3, 0, Cpu::rol),
    undefined_instruction!(0x2F),

    instruction!("BMI $NN", 0x30, AddressingMode::Relative, 2, 0, Cpu::bmi),
    instruction!("AND ($NN),Y", 0x31, AddressingMode::IndirectY, 2, 0, Cpu::and),
    undefined_instruction!(0x32),
    undefined_instruction!(0x33),
    undefined_instruction!(0x34),
    instruction!("AND $NN,X", 0x35, AddressingMode::ZeroPageX, 2, 0, Cpu::and),
    instruction!("ROL $NN,X", 0x36, AddressingMode::ZeroPageX, 2, 0, Cpu::rol),
    undefined_instruction!(0x37),
    instruction!("SEC", 0x38, AddressingMode::Implicit, 1, 0, Cpu::sec),
    instruction!("AND $NNNN,Y", 0x39, AddressingMode::AbsoluteY, 3, 0, Cpu::and),
    undefined_instruction!(0x3A),
    undefined_instruction!(0x3B),
    undefined_instruction!(0x3C),
    instruction!("AND $NNNN,X", 0x3D, AddressingMode::AbsoluteX, 3, 0, Cpu::and),
    instruction!("ROL $NNNN,X", 0x3E, AddressingMode::AbsoluteX, 3, 0, Cpu::rol),
    undefined_instruction!(0x3F),

    instruction!("RTI", 0x40, AddressingMode::Implicit, 1, 0, Cpu::rti),
    instruction!("EOR ($NN,X)", 0x41, AddressingMode::IndirectX, 2, 0, Cpu::eor),
    undefined_instruction!(0x42),
    undefined_instruction!(0x43),
    undefined_instruction!(0x44),
    instruction!("EOR $NN", 0x45, AddressingMode::ZeroPage, 2, 0, Cpu::eor),
    instruction!("LSR $NN", 0x46, AddressingMode::ZeroPage, 2, 0, Cpu::lsr),
    undefined_instruction!(0x47),
    instruction!("PHA", 0x48, AddressingMode::Implicit, 1, 0, Cpu::pha),
    instruction!("EOR #$NN", 0x49, AddressingMode::Immidiate, 2, 0, Cpu::eor),
    instruction!("LSR A", 0x4A, AddressingMode::Accumulator, 1, 0, Cpu::lsr),
    undefined_instruction!(0x4B),
    instruction!("JMP $NNNN", 0x4C, AddressingMode::Absolute, 3, 0, Cpu::jmp),
    instruction!("EOR $NNNN", 0x4D, AddressingMode::Absolute, 3, 0, Cpu::eor),
    instruction!("LSR $NNNN", 0x4E, AddressingMode::Absolute, 3, 0, Cpu::lsr),
    undefined_instruction!(0x4F),

    instruction!("BVC $NN", 0x50, AddressingMode::Relative, 2, 0, Cpu::bvc),
    instruction!("EOR ($NN),Y", 0x51, AddressingMode::IndirectY, 2, 0, Cpu::eor),
    undefined_instruction!(0x52),
    undefined_instruction!(0x53),
    undefined_instruction!(0x54),
    instruction!("EOR $NN,X", 0x55, AddressingMode::ZeroPageX, 2, 0, Cpu::eor),
    instruction!("LSR $NN,X", 0x56, AddressingMode::ZeroPageX, 2, 0, Cpu::lsr),
    undefined_instruction!(0x57),
    instruction!("CLI", 0x58, AddressingMode::Implicit, 1, 0, Cpu::cli),
    instruction!("EOR $NNNN,Y", 0x59, AddressingMode::AbsoluteY, 3, 0, Cpu::eor),
    undefined_instruction!(0x5A),
    undefined_instruction!(0x5B),
    undefined_instruction!(0x5C),
    instruction!("EOR $NNNN,X", 0x5D, AddressingMode::AbsoluteX, 3, 0, Cpu::eor),
    instruction!("LSR $NNNN,X", 0x5E, AddressingMode::AbsoluteX, 3, 0, Cpu::lsr),
    undefined_instruction!(0x5F),

    instruction!("RTS", 0x60, AddressingMode::Implicit, 1, 0, Cpu::rts),
    instruction!("ADC ($NN,X)", 0x61, AddressingMode::IndirectX, 2, 0, Cpu::adc),
    undefined_instruction!(0x62),
    undefined_instruction!(0x63),
    undefined_instruction!(0x64),
    instruction!("ADC $NN", 0x65, AddressingMode::ZeroPage, 2, 0, Cpu::adc),
    instruction!("ROR $NN", 0x66, AddressingMode::ZeroPage, 2, 0, Cpu::ror),
    undefined_instruction!(0x67),
    instruction!("PLA", 0x68, AddressingMode::ZeroPage, 1, 0, Cpu::pla),
    instruction!("ADC #$NN", 0x69, AddressingMode::Immidiate, 2, 0, Cpu::adc),
    instruction!("ROR", 0x6A, AddressingMode::Accumulator, 1, 0, Cpu::ror),
    undefined_instruction!(0x6B),
    instruction!("JMP $NN", 0x6C, AddressingMode::Indirect, 1, 0, Cpu::jmp),
    instruction!("ADC $NNNN", 0x6D, AddressingMode::Absolute, 1, 0, Cpu::adc),
    instruction!("ROR $NNNN,X", 0x6E, AddressingMode::AbsoluteX, 1, 0, Cpu::ror),
    undefined_instruction!(0x6F),

    instruction!("BVS $NN", 0x70, AddressingMode::Relative, 2, 0, Cpu::bvs),
    instruction!("ADC ($NN),Y", 0x71, AddressingMode::IndirectY, 2, 0, Cpu::adc),
    undefined_instruction!(0x72),
    undefined_instruction!(0x73),
    undefined_instruction!(0x74),
    instruction!("ADC $NN,X", 0x75, AddressingMode::ZeroPage, 2, 0, Cpu::adc),
    instruction!("ROR $NN,X", 0x76, AddressingMode::ZeroPage, 2, 0, Cpu::ror),
    undefined_instruction!(0x77),
    instruction!("SEI", 0x78, AddressingMode::Implicit, 1, 0, Cpu::sei),
    instruction!("ADC $NNNN,Y", 0x79, AddressingMode::AbsoluteY, 3, 0, Cpu::adc),
    undefined_instruction!(0x7A),
    undefined_instruction!(0x7B),
    undefined_instruction!(0x7C),
    instruction!("ADC $NNNN,X", 0x7D, AddressingMode::AbsoluteX, 3, 0, Cpu::adc),
    instruction!("ROR $NNNN", 0x7E, AddressingMode::Absolute, 3, 0, Cpu::ror),
    undefined_instruction!(0x7F),

    undefined_instruction!(0x80),
    instruction!("STA ($NN,X)", 0x81, AddressingMode::Indirect, 2, 0, Cpu::sta),
    undefined_instruction!(0x82),
    undefined_instruction!(0x83),
    instruction!("STY $NN", 0x84, AddressingMode::ZeroPage, 2, 0, Cpu::sty),
    instruction!("STA $NN", 0x85, AddressingMode::ZeroPage, 2, 0, Cpu::sta),
    instruction!("STX $NN", 0x86, AddressingMode::ZeroPage, 2, 0, Cpu::stx),
    undefined_instruction!(0x87),
    instruction!("DEY", 0x88, AddressingMode::Implicit, 1, 0, Cpu::dey),
    undefined_instruction!(0x89),
    instruction!("TXA", 0x8A, AddressingMode::Implicit, 1, 0, Cpu::txa),
    undefined_instruction!(0x8B),
    instruction!("STY $NNNN", 0x8C, AddressingMode::Absolute, 3, 0, Cpu::sty),
    instruction!("STA $NNNN", 0x8D, AddressingMode::Absolute, 3, 0, Cpu::sta),
    instruction!("STX $NNNN", 0x8E, AddressingMode::Absolute, 3, 0, Cpu::stx),
    undefined_instruction!(0x8F),

    instruction!("BCC $NN", 0x90, AddressingMode::Relative, 2, 0, Cpu::bcc),
    instruction!("STA ($NN),Y", 0x91, AddressingMode::IndirectY, 2, 0, Cpu::sta),
    undefined_instruction!(0x92),
    undefined_instruction!(0x93),
    instruction!("STY $NN,X", 0x94, AddressingMode::ZeroPageX, 2, 0, Cpu::sty),
    instruction!("STA $NN,X", 0x95, AddressingMode::ZeroPageX, 2, 0, Cpu::sta),
    instruction!("STX $NN,Y", 0x96, AddressingMode::ZeroPageX, 2, 0, Cpu::stx),
    undefined_instruction!(0x97),
    instruction!("TYA", 0x98, AddressingMode::Implicit, 1, 0, Cpu::tya),
    instruction!("STA $NNNN,Y", 0x99, AddressingMode::AbsoluteY, 3, 0, Cpu::sta),
    instruction!("TXS", 0x9A, AddressingMode::Implicit, 1, 0, Cpu::txs),
    undefined_instruction!(0x9B),
    undefined_instruction!(0x9C),
    instruction!("STA $NNNN,X", 0x9D, AddressingMode::AbsoluteX, 3, 0, Cpu::sta),
    undefined_instruction!(0x9E),
    undefined_instruction!(0x9F),

    instruction!("LDY #$NN", 0xA0, AddressingMode::Immidiate, 2, 0, Cpu::ldy),
    instruction!("LDA ($NN,X)", 0xA1, AddressingMode::IndirectX, 2, 0, Cpu::lda),
    instruction!("LDX #$NN", 0xA2, AddressingMode::Immidiate, 2, 0, Cpu::ldx),
    undefined_instruction!(0xA3),
    instruction!("LDY $NN", 0xA4, AddressingMode::ZeroPage, 2, 0, Cpu::ldy),
    instruction!("LDA $NN", 0xA5, AddressingMode::ZeroPage, 2, 0, Cpu::lda),
    instruction!("LDX $NN", 0xA6, AddressingMode::ZeroPage, 2, 0, Cpu::ldx),
    undefined_instruction!(0xA7),
    instruction!("TAY", 0xA8, AddressingMode::Implicit, 1, 0, Cpu::tay),
    instruction!("LDA #$NN", 0xA9, AddressingMode::Immidiate, 2, 0, Cpu::lda),
    instruction!("TAX", 0xAA, AddressingMode::Implicit, 1, 0, Cpu::tax),
    undefined_instruction!(0xAB),
    instruction!("LDY $NNNN", 0xAC, AddressingMode::Absolute, 3, 0, Cpu::ldy),
    instruction!("LDA $NNNN", 0xAD, AddressingMode::Absolute, 3, 0, Cpu::lda),
    instruction!("LDX $NNNN", 0xAE, AddressingMode::Absolute, 3, 0, Cpu::ldx),
    undefined_instruction!(0xAF),

    instruction!("BCS $NN", 0xB0, AddressingMode::Relative, 2, 0, Cpu::bcs),
    instruction!("LDA ($NN),Y", 0xB1, AddressingMode::IndirectY, 2, 0, Cpu::lda),
    undefined_instruction!(0xB2),
    undefined_instruction!(0xB3),
    instruction!("LDY $NN,X", 0xB4, AddressingMode::ZeroPageX, 2, 0, Cpu::ldy),
    instruction!("LDA $NN,X", 0xB5, AddressingMode::ZeroPageX, 2, 0, Cpu::lda),
    instruction!("LDX $NN,Y", 0xB6, AddressingMode::ZeroPageX, 2, 0, Cpu::ldx),
    undefined_instruction!(0xB7),
    instruction!("CLV", 0xB8, AddressingMode::Implicit, 1, 0, Cpu::clv),
    instruction!("LDA", 0xB9, AddressingMode::AbsoluteY, 1, 0, Cpu::lda),
    instruction!("TSX", 0xBA, AddressingMode::Implicit, 1, 0, Cpu::tsx),
    undefined_instruction!(0xBB),
    instruction!("LDY $NNNN,X", 0xBC, AddressingMode::AbsoluteX, 3, 0, Cpu::ldy),
    instruction!("LDA $NNNN,X", 0xBD, AddressingMode::AbsoluteX, 3, 0, Cpu::lda),
    instruction!("LDX $NNNN,Y", 0xBE, AddressingMode::AbsoluteY, 3, 0, Cpu::ldx),
    undefined_instruction!(0xBF),

    instruction!("CPY #$NN", 0xC0, AddressingMode::Immidiate, 2, 0, Cpu::cpy),
    instruction!("CMP ($NN,X)", 0xC1, AddressingMode::IndirectX, 2, 0, Cpu::cmp),
    undefined_instruction!(0xC2),
    undefined_instruction!(0xC3),
    instruction!("CPY $NN", 0xC4, AddressingMode::ZeroPage, 2, 0, Cpu::cpy),
    instruction!("CMP $NN", 0xC5, AddressingMode::ZeroPage, 2, 0, Cpu::cmp),
    instruction!("DEC $NN", 0xC6, AddressingMode::ZeroPage, 2, 0, Cpu::dec),
    undefined_instruction!(0xC7),
    instruction!("INY", 0xC8, AddressingMode::Implicit, 1, 0, Cpu::iny),
    instruction!("CMP #$NN", 0xC9, AddressingMode::Immidiate, 2, 0, Cpu::cmp),
    instruction!("DEX", 0xCA, AddressingMode::Implicit, 1, 0, Cpu::dex),
    undefined_instruction!(0xCB),
    instruction!("CPY $NNNN", 0xCC, AddressingMode::Absolute, 3, 0, Cpu::cpy),
    instruction!("CMP $NNNN", 0xCD, AddressingMode::Absolute, 3, 0, Cpu::cmp),
    instruction!("DEC $NNNN", 0xCE, AddressingMode::Absolute, 3, 0, Cpu::dec),
    undefined_instruction!(0xCF),

    instruction!("BNE $NN", 0xD0, AddressingMode::Relative, 2, 0, Cpu::bne),
    instruction!("CMP ($NN),Y", 0xD1, AddressingMode::IndirectY, 2, 0, Cpu::cmp),
    undefined_instruction!(0xD2),
    undefined_instruction!(0xD3),
    undefined_instruction!(0xD4),
    instruction!("CMP $NN,X", 0xD5, AddressingMode::ZeroPageX, 2, 0, Cpu::cmp),
    instruction!("DEC $NN,X", 0xD6, AddressingMode::ZeroPageX, 2, 0, Cpu::dec),
    undefined_instruction!(0xD7),
    instruction!("CLD", 0xD8, AddressingMode::Implicit, 1, 0, Cpu::cld),
    instruction!("CMP $NNNN,Y", 0xD9, AddressingMode::AbsoluteY, 3, 0, Cpu::cmp),
    undefined_instruction!(0xDA),
    undefined_instruction!(0xDB),
    undefined_instruction!(0xDC),
    instruction!("CMP $NNNN,X", 0xDD, AddressingMode::AbsoluteX, 3, 0, Cpu::cmp),
    instruction!("DEC $NNNN,X", 0xDE, AddressingMode::AbsoluteX, 3, 0, Cpu::dec),
    undefined_instruction!(0xDF),

    instruction!("CPX #$NN", 0xE0, AddressingMode::Immidiate, 2, 0, Cpu::cpx),
    instruction!("SBC ($NN,X)", 0xE1, AddressingMode::IndirectX, 2, 0, Cpu::sbc),
    undefined_instruction!(0xE2),
    undefined_instruction!(0xE3),
    instruction!("CPX $NN", 0xE4, AddressingMode::ZeroPage, 1, 0, Cpu::cpx),
    instruction!("SBC $NN", 0xE5, AddressingMode::ZeroPage, 1, 0, Cpu::sbc),
    instruction!("INC $NN", 0xE6, AddressingMode::ZeroPage, 1, 0, Cpu::inc),
    undefined_instruction!(0xE7),
    instruction!("INX", 0xE8, AddressingMode::Implicit, 1, 0, Cpu::inx),
    instruction!("SBC #$NN", 0xE9, AddressingMode::Immidiate, 2, 0, Cpu::sbc),
    instruction!("NOP", 0xEA, AddressingMode::Implicit, 1, 0, Cpu::nop),
    undefined_instruction!(0xEB),
    instruction!("CPX $NNNN", 0xEC, AddressingMode::Absolute, 3, 0, Cpu::cpx),
    instruction!("SBC $NNNN", 0xED, AddressingMode::Absolute, 3, 0, Cpu::sbc),
    instruction!("INC $NNNN", 0xEE, AddressingMode::Absolute, 3, 0, Cpu::inc),
    undefined_instruction!(0xEF),

    instruction!("BEQ $NN", 0xF0, AddressingMode::Relative, 2, 0, Cpu::beq),
    instruction!("SBC ($NN),Y", 0xF1, AddressingMode::IndirectY, 2, 0, Cpu::sbc),
    undefined_instruction!(0xF2),
    undefined_instruction!(0xF3),
    undefined_instruction!(0xF4),
    instruction!("SBC $NN,X", 0xF5, AddressingMode::ZeroPageX, 2, 0, Cpu::sbc),
    instruction!("INC $NN,X", 0xF6, AddressingMode::ZeroPageX, 2, 0, Cpu::inc),
    undefined_instruction!(0xF7),
    instruction!("SED", 0xE8, AddressingMode::Implicit, 1, 0, Cpu::sed),
    instruction!("SBC $NNNN,Y", 0xF9, AddressingMode::AbsoluteY, 3, 0, Cpu::sbc),
    undefined_instruction!(0xFA),
    undefined_instruction!(0xFB),
    undefined_instruction!(0xFC),
    instruction!("SBC $NNNN,X", 0xFD, AddressingMode::AbsoluteX, 3, 0, Cpu::sbc),
    instruction!("INC $NNNN,X", 0xFE, AddressingMode::AbsoluteX, 3, 0, Cpu::inc),
    undefined_instruction!(0xFF),
];
