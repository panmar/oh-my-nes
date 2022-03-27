// TODO: Remove me
#![allow(dead_code)]
use crate::memory::Address;

use super::memory::Memory;
use bitflags::bitflags;

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
    handler: fn(cpu: &mut Cpu, memory: &mut Memory, arg_address: Option<Address>),
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

impl Cpu {
    pub fn new() -> Cpu {
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
        let opcode = memory.read_u8(self.program_counter);
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
            .set(Flags::NEGATIVE, self.accumulator & 0b1000_0000 != 0);
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

const UNDEFINED_INSTRUCTION: Instruction = Instruction {
    mnemonic: "UDE",
    opcode: 0x00,
    mode: AddressingMode::Implicit,
    bytesize: 1,
    cycles: 0,
    handler: Cpu::ude,
};

macro_rules! undefined_instruction {
    ($opcode: literal) => {
        Instruction { mnemonic: "UDE", opcode: $opcode, mode: AddressingMode::Implicit, bytesize: 1, cycles: 0, handler: Cpu::ude }
    }
}

#[rustfmt::skip]
const INSTRUCTIONS: [Instruction; 256] = [
    Instruction { mnemonic: "BRK", opcode: 0x00, mode: AddressingMode::Implicit, bytesize: 1, cycles: 7, handler: Cpu::brk },
    Instruction { mnemonic: "ORA($NN,X)", opcode: 0x01, mode: AddressingMode::IndirectX, bytesize: 2, cycles: 6, handler: Cpu::ora },
    undefined_instruction!(0x02),
    undefined_instruction!(0x03),
    undefined_instruction!(0x04),
    Instruction { mnemonic: "ORA $NN", opcode: 0x05, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::ora },
    Instruction { mnemonic: "ASL $NN", opcode: 0x06, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::asl },
    undefined_instruction!(0x07),
    Instruction { mnemonic: "PHP", opcode: 0x08, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::php },
    Instruction { mnemonic: "ORA #$NN", opcode: 0x09, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::ora },
    Instruction { mnemonic: "ASL A", opcode: 0x0A, mode: AddressingMode::Accumulator, bytesize: 0, cycles: 0, handler: Cpu::asl },
    undefined_instruction!(0x0B),
    undefined_instruction!(0x0C),
    Instruction { mnemonic: "ORA $NNNN", opcode: 0x0D, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::ora },
    Instruction { mnemonic: "ASL $NNNN", opcode: 0x0E, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::asl },
    undefined_instruction!(0x0F),

    Instruction { mnemonic: "BPL $NN", opcode: 0x10, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bpl },
    Instruction { mnemonic: "ORA ($NN),Y", opcode: 0x11, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::ora },
    undefined_instruction!(0x12),
    undefined_instruction!(0x13),
    undefined_instruction!(0x14),
    Instruction { mnemonic: "ORA $NN,X", opcode: 0x15, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::ora },
    Instruction { mnemonic: "ASL $NN,X", opcode: 0x16, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::asl },
    undefined_instruction!(0x17),
    Instruction { mnemonic: "CLC", opcode: 0x18, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::clc },
    Instruction { mnemonic: "ORA $NNNN,Y", opcode: 0x19, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::ora },
    undefined_instruction!(0x1A),
    undefined_instruction!(0x1B),
    undefined_instruction!(0x1C),
    Instruction { mnemonic: "ORA $NNNN,X", opcode: 0x1D, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::ora },
    Instruction { mnemonic: "ASL $NNNN,X", opcode: 0x1E, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::asl },
    undefined_instruction!(0x1F),

    Instruction { mnemonic: "JSR $NNNN", opcode: 0x20, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::jsr },
    Instruction { mnemonic: "AND ($NN,X)", opcode: 0x21, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::and },
    undefined_instruction!(0x22),
    undefined_instruction!(0x23),
    Instruction { mnemonic: "BIT $NN", opcode: 0x24, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::bit },
    Instruction { mnemonic: "AND $NN", opcode: 0x25, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::and },
    Instruction { mnemonic: "ROL $NN", opcode: 0x26, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::rol },
    undefined_instruction!(0x27),
    Instruction { mnemonic: "PLP", opcode: 0x28, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::plp },
    Instruction { mnemonic: "AND #$NN", opcode: 0x29, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::and },
    Instruction { mnemonic: "ROL A", opcode: 0x2A, mode: AddressingMode::Accumulator, bytesize: 0, cycles: 0, handler: Cpu::rol },
    undefined_instruction!(0x2B),
    Instruction { mnemonic: "BIT $NNNN", opcode: 0x2C, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::bit },
    Instruction { mnemonic: "AND $NNNN", opcode: 0x2D, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::and },
    Instruction { mnemonic: "ROL $NNNN", opcode: 0x2E, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::rol },
    undefined_instruction!(0x2F),

    Instruction { mnemonic: "BMI $NN", opcode: 0x30, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bmi },
    Instruction { mnemonic: "AND ($NN),Y", opcode: 0x31, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::and },
    undefined_instruction!(0x32),
    undefined_instruction!(0x33),
    undefined_instruction!(0x34),
    Instruction { mnemonic: "AND $NN,X", opcode: 0x35, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::and },
    Instruction { mnemonic: "ROL $NN,X", opcode: 0x36, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::rol },
    undefined_instruction!(0x37),
    Instruction { mnemonic: "SEC", opcode: 0x38, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::sec },
    Instruction { mnemonic: "AND $NNNN,Y", opcode: 0x39, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::and },
    undefined_instruction!(0x3A),
    undefined_instruction!(0x3B),
    undefined_instruction!(0x3C),
    Instruction { mnemonic: "AND $NNNN,X", opcode: 0x3D, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::and },
    Instruction { mnemonic: "ROL $NNNN,X", opcode: 0x3E, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::rol },
    undefined_instruction!(0x3F),

    Instruction { mnemonic: "RTI", opcode: 0x40, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::rti },
    Instruction { mnemonic: "EOR ($NN,X)", opcode: 0x41, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::eor },
    undefined_instruction!(0x42),
    undefined_instruction!(0x43),
    undefined_instruction!(0x44),
    Instruction { mnemonic: "EOR $NN", opcode: 0x45, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::eor },
    Instruction { mnemonic: "LSR $NN", opcode: 0x46, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::lsr },
    undefined_instruction!(0x47),
    Instruction { mnemonic: "PHA", opcode: 0x48, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::pha },
    Instruction { mnemonic: "EOR #$NN", opcode: 0x49, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::eor },
    Instruction { mnemonic: "LSR A", opcode: 0x4A, mode: AddressingMode::Accumulator, bytesize: 0, cycles: 0, handler: Cpu::lsr },
    undefined_instruction!(0x4B),
    Instruction { mnemonic: "JMP $NNNN", opcode: 0x4C, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::jmp },
    Instruction { mnemonic: "EOR $NNNN", opcode: 0x4D, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::eor },
    Instruction { mnemonic: "LSR $NNNN", opcode: 0x4E, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::lsr },
    undefined_instruction!(0x4F),

    Instruction { mnemonic: "BVC $NN", opcode: 0x50, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bvc },
    Instruction { mnemonic: "EOR ($NN),Y", opcode: 0x51, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::eor },
    undefined_instruction!(0x52),
    undefined_instruction!(0x53),
    undefined_instruction!(0x54),
    Instruction { mnemonic: "EOR $NN,X", opcode: 0x55, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::eor },
    Instruction { mnemonic: "LSR $NN,X", opcode: 0x56, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::lsr },
    undefined_instruction!(0x57),
    Instruction { mnemonic: "CLI", opcode: 0x58, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::cli },
    Instruction { mnemonic: "EOR $NNNN,Y", opcode: 0x59, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::eor },
    undefined_instruction!(0x5A),
    undefined_instruction!(0x5B),
    undefined_instruction!(0x5C),
    Instruction { mnemonic: "EOR $NNNN,X", opcode: 0x5D, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::eor },
    Instruction { mnemonic: "LSR $NNNN,X", opcode: 0x5E, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::lsr },
    undefined_instruction!(0x5F),

    Instruction { mnemonic: "RTS", opcode: 0x60, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::rts },
    Instruction { mnemonic: "ADC ($NN,X)", opcode: 0x61, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::adc },
    undefined_instruction!(0x62),
    undefined_instruction!(0x63),
    undefined_instruction!(0x64),
    Instruction { mnemonic: "ADC $NN", opcode: 0x65, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::adc },
    Instruction { mnemonic: "ROR $NN", opcode: 0x66, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::ror },
    undefined_instruction!(0x67),
    Instruction { mnemonic: "PLA", opcode: 0x68, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::pla },
    Instruction { mnemonic: "ADC #$NN", opcode: 0x69, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::adc },
    Instruction { mnemonic: "ROR", opcode: 0x6A, mode: AddressingMode::Accumulator, bytesize: 0, cycles: 0, handler: Cpu::ror },
    undefined_instruction!(0x6B),
    Instruction { mnemonic: "JMP $NN", opcode: 0x6C, mode: AddressingMode::Indirect, bytesize: 0, cycles: 0, handler: Cpu::jmp },
    Instruction { mnemonic: "ADC $NNNN", opcode: 0x6D, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::adc },
    Instruction { mnemonic: "ROR $NNNN,X", opcode: 0x6E, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::ror },
    undefined_instruction!(0x6F),

    Instruction { mnemonic: "BVS $NN", opcode: 0x70, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bvs },
    Instruction { mnemonic: "ADC ($NN),Y", opcode: 0x71, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::adc },
    undefined_instruction!(0x72),
    undefined_instruction!(0x73),
    undefined_instruction!(0x74),
    Instruction { mnemonic: "ADC $NN,X", opcode: 0x75, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::adc },
    Instruction { mnemonic: "ROR $NN,X", opcode: 0x76, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::ror },
    undefined_instruction!(0x77),
    Instruction { mnemonic: "SEI", opcode: 0x78, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::sei },
    Instruction { mnemonic: "ADC $NNNN,Y", opcode: 0x79, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::adc },
    undefined_instruction!(0x7A),
    undefined_instruction!(0x7B),
    undefined_instruction!(0x7C),
    Instruction { mnemonic: "ADC $NNNN,X", opcode: 0x7D, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::adc },
    Instruction { mnemonic: "ROR $NNNN", opcode: 0x7E, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::ror },
    undefined_instruction!(0x7F),

    undefined_instruction!(0x80),
    Instruction { mnemonic: "STA ($NN,X)", opcode: 0x81, mode: AddressingMode::Indirect, bytesize: 0, cycles: 0, handler: Cpu::sta },
    undefined_instruction!(0x82),
    undefined_instruction!(0x83),
    Instruction { mnemonic: "STY $NN", opcode: 0x84, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::sty },
    Instruction { mnemonic: "STA $NN", opcode: 0x85, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::sta },
    Instruction { mnemonic: "STX $NN", opcode: 0x86, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::stx },
    undefined_instruction!(0x87),
    Instruction { mnemonic: "DEY", opcode: 0x88, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::dey },
    undefined_instruction!(0x89),
    Instruction { mnemonic: "TXA", opcode: 0x8A, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::txa },
    undefined_instruction!(0x8B),
    Instruction { mnemonic: "STY $NNNN", opcode: 0x8C, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::sty },
    Instruction { mnemonic: "STA $NNNN", opcode: 0x8D, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::sta },
    Instruction { mnemonic: "STX $NNNN", opcode: 0x8E, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::stx },
    undefined_instruction!(0x8F),

    Instruction { mnemonic: "BCC $NN", opcode: 0x90, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bcc },
    Instruction { mnemonic: "STA ($NN),Y", opcode: 0x91, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::sta },
    undefined_instruction!(0x92),
    undefined_instruction!(0x93),
    Instruction { mnemonic: "STY $NN,X", opcode: 0x94, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::sty },
    Instruction { mnemonic: "STA $NN,X", opcode: 0x95, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::sta },
    Instruction { mnemonic: "STX $NN,Y", opcode: 0x96, mode: AddressingMode::ZeroPageY, bytesize: 0, cycles: 0, handler: Cpu::stx },
    undefined_instruction!(0x97),
    Instruction { mnemonic: "TYA", opcode: 0x98, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::tya },
    Instruction { mnemonic: "STA $NNNN,Y", opcode: 0x99, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::sta },
    Instruction { mnemonic: "TXS", opcode: 0x9A, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::txs },
    undefined_instruction!(0x9B),
    undefined_instruction!(0x9C),
    Instruction { mnemonic: "STA $NNNN,X", opcode: 0x9D, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::sta },
    undefined_instruction!(0x9E),
    undefined_instruction!(0x9F),

    Instruction { mnemonic: "LDY #$NN", opcode: 0xA0, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::ldy },
    Instruction { mnemonic: "LDA ($NN,X)", opcode: 0xA1, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "LDX #$NN", opcode: 0xA2, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::ldx },
    undefined_instruction!(0xA3),
    Instruction { mnemonic: "LDY $NN", opcode: 0xA4, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::ldy },
    Instruction { mnemonic: "LDA $NN", opcode: 0xA5, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "LDX $NN", opcode: 0xA6, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::ldx },
    undefined_instruction!(0xA7),
    Instruction { mnemonic: "TAY", opcode: 0xA8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::tay },
    Instruction { mnemonic: "LDA #$NN", opcode: 0xA9, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "TAX", opcode: 0xAA, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::tax },
    undefined_instruction!(0xAB),
    Instruction { mnemonic: "LDY $NNNN", opcode: 0xAC, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::ldy },
    Instruction { mnemonic: "LDA $NNNN", opcode: 0xAD, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "LDX $NNNN", opcode: 0xAE, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::ldx },
    undefined_instruction!(0xAF),

    Instruction { mnemonic: "BCS $NN", opcode: 0xB0, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bcs },
    Instruction { mnemonic: "LDA ($NN),Y", opcode: 0xB1, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::lda },
    undefined_instruction!(0xB2),
    undefined_instruction!(0xB3),
    Instruction { mnemonic: "LDY $NN,X", opcode: 0xB4, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::ldy },
    Instruction { mnemonic: "LDA $NN,X", opcode: 0xB5, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "LDX $NN,Y", opcode: 0xB6, mode: AddressingMode::ZeroPageY, bytesize: 0, cycles: 0, handler: Cpu::ldx },
    undefined_instruction!(0xB7),
    Instruction { mnemonic: "CLV", opcode: 0xB8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::clv },
    Instruction { mnemonic: "LDA", opcode: 0xB9, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "TSX", opcode: 0xBA, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::tsx },
    undefined_instruction!(0xBB),
    Instruction { mnemonic: "LDY $NNNN,X", opcode: 0xBC, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::ldy },
    Instruction { mnemonic: "LDA $NNNN,X", opcode: 0xBD, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::lda },
    Instruction { mnemonic: "LDX $NNNN,Y", opcode: 0xBE, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::ldx },
    undefined_instruction!(0xBF),

    Instruction { mnemonic: "CPY #$NN", opcode: 0xC0, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::cpy },
    Instruction { mnemonic: "CMP ($NN,X)", opcode: 0xC1, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    undefined_instruction!(0xC2),
    undefined_instruction!(0xC3),
    Instruction { mnemonic: "CPY $NN", opcode: 0xC4, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::cpy },
    Instruction { mnemonic: "CMP $NN", opcode: 0xC5, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    Instruction { mnemonic: "DEC $NN", opcode: 0xC6, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::dec },
    undefined_instruction!(0xC7),
    Instruction { mnemonic: "INY", opcode: 0xC8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::iny },
    Instruction { mnemonic: "CMP #$NN", opcode: 0xC9, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    Instruction { mnemonic: "DEX", opcode: 0xCA, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::dex },
    undefined_instruction!(0xCB),
    Instruction { mnemonic: "CPY $NNNN", opcode: 0xCC, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::cpy },
    Instruction { mnemonic: "CMP $NNNN", opcode: 0xCD, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    Instruction { mnemonic: "DEC $NNNN", opcode: 0xCE, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::dec },
    undefined_instruction!(0xCF),

    Instruction { mnemonic: "BNE $NN", opcode: 0xD0, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::bne },
    Instruction { mnemonic: "CMP ($NN),Y", opcode: 0xD1, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    undefined_instruction!(0xD2),
    undefined_instruction!(0xD3),
    undefined_instruction!(0xD4),
    Instruction { mnemonic: "CMP $NN,X", opcode: 0xD5, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    Instruction { mnemonic: "DEC $NN,X", opcode: 0xD6, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::dec },
    undefined_instruction!(0xD7),
    Instruction { mnemonic: "CLD", opcode: 0xD8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::cld },
    Instruction { mnemonic: "CMP $NNNN,Y", opcode: 0xD9, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    undefined_instruction!(0xDA),
    undefined_instruction!(0xDB),
    undefined_instruction!(0xDC),
    Instruction { mnemonic: "CMP $NNNN,X", opcode: 0xDD, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::cmp },
    Instruction { mnemonic: "DEC $NNNN,X", opcode: 0xDE, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::dec },
    undefined_instruction!(0xDF),

    Instruction { mnemonic: "CPX #$NN", opcode: 0xE0, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::cpx },
    Instruction { mnemonic: "SBC ($NN,X)", opcode: 0xE1, mode: AddressingMode::IndirectX, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    undefined_instruction!(0xE2),
    undefined_instruction!(0xE3),
    Instruction { mnemonic: "CPX $NN", opcode: 0xE4, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::cpx },
    Instruction { mnemonic: "SBC $NN", opcode: 0xE5, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    Instruction { mnemonic: "INC $NN", opcode: 0xE6, mode: AddressingMode::ZeroPage, bytesize: 0, cycles: 0, handler: Cpu::inc },
    undefined_instruction!(0xE7),
    Instruction { mnemonic: "INX", opcode: 0xE8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::inx },
    Instruction { mnemonic: "SBC #$NN", opcode: 0xE9, mode: AddressingMode::Immidiate, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    Instruction { mnemonic: "NOP", opcode: 0xEA, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::nop },
    undefined_instruction!(0xEB),
    Instruction { mnemonic: "CPX $NNNN", opcode: 0xEC, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::cpx },
    Instruction { mnemonic: "SBC $NNNN", opcode: 0xED, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    Instruction { mnemonic: "INC $NNNN", opcode: 0xEE, mode: AddressingMode::Absolute, bytesize: 0, cycles: 0, handler: Cpu::inc },
    undefined_instruction!(0xEF),

    Instruction { mnemonic: "BEQ $NN", opcode: 0xF0, mode: AddressingMode::Relative, bytesize: 0, cycles: 0, handler: Cpu::beq },
    Instruction { mnemonic: "SBC ($NN),Y", opcode: 0xF1, mode: AddressingMode::IndirectY, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    undefined_instruction!(0xF2),
    undefined_instruction!(0xF3),
    undefined_instruction!(0xF4),
    Instruction { mnemonic: "SBC $NN,X", opcode: 0xF5, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    Instruction { mnemonic: "INC $NN,X", opcode: 0xF6, mode: AddressingMode::ZeroPageX, bytesize: 0, cycles: 0, handler: Cpu::inc },
    undefined_instruction!(0xF7),
    Instruction { mnemonic: "SED", opcode: 0xF8, mode: AddressingMode::Implicit, bytesize: 0, cycles: 0, handler: Cpu::sed },
    Instruction { mnemonic: "SBC $NNNN,Y", opcode: 0xF9, mode: AddressingMode::AbsoluteY, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    undefined_instruction!(0xFA),
    undefined_instruction!(0xFB),
    undefined_instruction!(0xFC),
    Instruction { mnemonic: "SBC $NNNN,X", opcode: 0xFD, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::sbc },
    Instruction { mnemonic: "INC $NNNN,X", opcode: 0xFE, mode: AddressingMode::AbsoluteX, bytesize: 0, cycles: 0, handler: Cpu::inc },
    undefined_instruction!(0xFF),
];

impl Cpu {
    fn adc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());

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

    fn and(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.set_accumulator(self.accumulator & arg);
    }

    fn asl(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        match arg_address {
            Some(address) => {
                let arg = memory.read_u8(address);
                let (result, carry) = arg.overflowing_shl(1);
                memory.write_u8(address, result);
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

    fn bcc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::CARRY) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn bcs(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::CARRY) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn beq(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::ZERO) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn bit(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        let result = self.accumulator & arg;
        self.flags.set(Flags::ZERO, result == 0);
        self.flags.set(Flags::NEGATIVE, result & 0b1000_0000 != 0);
        self.flags.set(Flags::OVERFLOW, result & 0b0100_0000 != 0);
    }

    fn bmi(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::NEGATIVE) {
            self.program_counter += arg_address.unwrap();
        }
    }

    fn bne(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::ZERO) {
            self.program_counter += arg_address.unwrap();
        }
    }

    fn bpl(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::NEGATIVE) {
            self.program_counter += arg_address.unwrap();
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
            self.program_counter += arg_address.unwrap();
        }
    }

    fn bvs(&mut self, _memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::OVERFLOW) {
            self.program_counter += arg_address.unwrap();
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
        self.flags.set(Flags::CARRY, false);
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

    fn jmp(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());
        self.program_counter = arg as u16;
    }

    fn jsr(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let arg = memory.read_u8(arg_address.unwrap());

        // TODO(panmar): Do we change stack pointer here?
        memory
            .stack(&mut self.stack_pointer)
            .push_u16(self.program_counter - 1);
        self.program_counter = arg as u16;
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
        match arg_address {
            Some(address) => {
                let arg = memory.read_u8(address);
                self.flags.set(Flags::CARRY, arg & 1 != 0);
                let (result, _) = arg.overflowing_shr(1);
                memory.write_u8(address, result);
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
        let mut flags = self.flags;
        flags.insert(Flags::BREAK1 | Flags::BREAK2);
        memory.stack(&mut self.stack_pointer).push_u8(flags.bits());
    }

    fn pla(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_accumulator = memory.stack(&mut self.stack_pointer).pop_u8();
        self.set_accumulator(new_accumulator);
    }

    fn plp(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
        self.flags.remove(Flags::BREAK1 | Flags::BREAK2);
    }

    fn rol(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        match arg_address {
            Some(address) => {
                let arg = memory.read_u8(address);
                let new_carry = (arg & 0b1000_0000) != 0;
                let mut result = arg.rotate_left(1);
                result &= 0b1111_1110;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b0000_0001;
                }
                memory.write_u8(address, result);
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

    fn ror(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        match arg_address {
            Some(address) => {
                let arg = memory.read_u8(address);
                let new_carry = (arg & 0b0000_0001) != 0;
                let mut result = arg.rotate_right(1);
                result &= 0b0111_1111;
                if self.flags.contains(Flags::CARRY) {
                    result |= 0b1000_0000;
                }
                memory.write_u8(address, result);
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

    fn rti(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        let new_flags = memory.stack(&mut self.stack_pointer).pop_u8();
        self.flags = Flags::from_bits(new_flags).unwrap();
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16();
    }

    fn rts(&mut self, memory: &mut Memory, _arg_address: Option<Address>) {
        // TODO(panmar): Do we need to increments by 1?
        self.program_counter = memory.stack(&mut self.stack_pointer).pop_u16() + 1;
    }

    fn sbc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        let mut arg = memory.read_u8(arg_address.unwrap());
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

#[cfg(test)]
mod test {
    use super::*;

    fn set_operand(memory: &mut Memory, value: u8) -> Option<Address> {
        let address = 0x0042;
        memory.write_u8(address, value);
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
            let arg_address = set_operand(&mut memory, opperand);
            cpu.flags.set(Flags::CARRY, carry_flag);
            let prev_cpu_flags = cpu.flags;

            // when
            cpu.adc(&mut memory, arg_address);

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
        assert_eq!(memory.stack(&mut cpu.stack_pointer).pop_u8(), 0b10111010);
        assert_eq!(
            memory.stack(&mut cpu.stack_pointer).pop_u16(),
            cpu.program_counter
        );
    }
}
