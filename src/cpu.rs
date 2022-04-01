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

    fn bmi(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::NEGATIVE) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn bne(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::ZERO) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn bpl(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::NEGATIVE) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
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

    fn bvc(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if !self.flags.contains(Flags::OVERFLOW) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
        }
    }

    fn bvs(&mut self, memory: &mut Memory, arg_address: Option<Address>) {
        if self.flags.contains(Flags::OVERFLOW) {
            let relative_address = memory.read_u8(arg_address.unwrap()) as i8;
            self.jump(relative_address);
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

    const ARGUMENT_ADDRESS: Address = 0x0242;

    fn try_set_argument(memory: &mut Memory, arg: Option<u8>) -> Option<Address> {
        match arg {
            Some(arg_value) => {
                memory.write_u8(ARGUMENT_ADDRESS, arg_value);
                return Some(ARGUMENT_ADDRESS);
            }
            None => return None,
        }
    }

    fn get_argument(memory: &Memory) -> u8 {
        return memory.read_u8(ARGUMENT_ADDRESS);
    }

    macro_rules! test_instruction {
        ($instruction: expr, $given: expr, $arg: expr, $then: expr) => {
            // given
            let mut cpu = Cpu::new();
            let mut memory = Memory::new();
            $given(&mut cpu, &mut memory);
            let arg_address = try_set_argument(&mut memory, $arg);

            // when
            $instruction(&mut cpu, &mut memory, arg_address);

            // then
            $then(&cpu, &memory);
        };
    }

    #[test]
    fn should_execute_adc() {
        test_instruction!(
            Cpu::adc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x77;
            },
            Some(0x03),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0x7A);
            }
        );
    }

    #[test]
    fn should_execute_adc_with_zeroes() {
        test_instruction!(
            Cpu::adc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x00;
            },
            Some(0x00),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0x00);
                assert!(cpu.flags.contains(Flags::ZERO));
            }
        );
    }

    #[test]
    fn should_execute_adc_with_overflow_and_negative() {
        test_instruction!(
            Cpu::adc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x40;
            },
            Some(0x40),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0x80);
                assert!(cpu.flags.contains(Flags::NEGATIVE | Flags::OVERFLOW));
            }
        );
    }

    #[test]
    fn should_execute_adc_with_carry() {
        test_instruction!(
            Cpu::adc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0xFF;
                cpu.flags.set(Flags::CARRY, true);
            },
            Some(0xFF),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0xFF);
                assert!(cpu.flags.contains(Flags::NEGATIVE | Flags::CARRY));
            }
        );
    }

    #[test]
    fn should_execute_and() {
        test_instruction!(
            Cpu::and,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b0110_0111;
            },
            Some(0b1100_0101),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b0100_0101);
            }
        );
    }

    #[test]
    fn should_execute_and_with_negative() {
        test_instruction!(
            Cpu::and,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1111_1111;
            },
            Some(0b1000_0111),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b1000_0111);
                assert!(cpu.flags.contains(Flags::NEGATIVE));
            }
        );
    }

    #[test]
    fn should_execute_and_with_zero() {
        test_instruction!(
            Cpu::and,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1010_0101;
            },
            Some(0b0101_1010),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b0000_0000);
                assert!(cpu.flags.contains(Flags::ZERO));
            }
        );
    }

    #[test]
    fn should_execute_asl() {
        test_instruction!(
            Cpu::asl,
            |_cpu: &mut Cpu, _memory: &mut Memory| {},
            Some(0b0101_0101),
            |cpu: &Cpu, memory: &Memory| {
                assert_eq!(get_argument(memory), 0b1010_1010);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_asl_with_carry_output() {
        test_instruction!(
            Cpu::asl,
            |_cpu: &mut Cpu, _memory: &mut Memory| {},
            Some(0b1000_0000),
            |cpu: &Cpu, memory: &Memory| {
                assert_eq!(get_argument(memory), 0b0000_0000);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            }
        );
    }

    #[test]
    fn should_execute_bcc_with_carry() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, true);
            },
            Some(0x42),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bcc_no_carry_positive_offset() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, false);
            },
            Some(122i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 122);
            }
        );
    }

    #[test]
    fn should_execute_bcc_no_carry_negative_offset() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, false);
            },
            Some(-71i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 71);
            }
        );
    }

    #[test]
    fn should_execute_bcs_with_carry() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcs,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 51);
            }
        );
    }

    #[test]
    fn should_execute_bcs_with_carry_negative_offset() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcs,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, true);
            },
            Some(-111i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 111);
            }
        );
    }

    #[test]
    fn should_execute_bcs_no_carry() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bcs,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::CARRY, false);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_beq_with_zero() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::beq,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::ZERO, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 51);
            }
        );
    }

    #[test]
    fn should_execute_beq_with_zero_negative_offset() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::beq,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::ZERO, true);
            },
            Some(-99i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 99);
            }
        );
    }

    #[test]
    fn should_execute_beq_no_zero() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::beq,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::ZERO, false);
            },
            Some(-99i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bit() {
        test_instruction!(
            Cpu::bit,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1011_1001;
            },
            Some(0b0111_1000),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b1011_1001);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
                assert_eq!(cpu.flags.contains(Flags::OVERFLOW), false);
            }
        );
    }

    #[test]
    fn should_execute_bit_with_negative() {
        test_instruction!(
            Cpu::bit,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1011_1001;
            },
            Some(0b1111_1000),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b1011_1001);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
                assert_eq!(cpu.flags.contains(Flags::OVERFLOW), false);
            }
        );
    }

    #[test]
    fn should_execute_bit_with_overflow() {
        test_instruction!(
            Cpu::bit,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1111_1001;
            },
            Some(0b0111_1000),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b1111_1001);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
                assert_eq!(cpu.flags.contains(Flags::OVERFLOW), true);
            }
        );
    }

    #[test]
    fn should_execute_bit_with_zero() {
        test_instruction!(
            Cpu::bit,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0b1010_0101;
            },
            Some(0b0101_1010),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.accumulator, 0b1010_0101);
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
                assert_eq!(cpu.flags.contains(Flags::OVERFLOW), false);
            }
        );
    }

    #[test]
    fn should_execute_bmi_no_negative_flag() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bmi,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::NEGATIVE, false);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bmi_with_negative_flag() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bmi,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::NEGATIVE, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 51);
            }
        );
    }

    #[test]
    fn should_execute_bmi_with_negative_flag_and_negative_offset() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bmi,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::NEGATIVE, true);
            },
            Some(-114i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 114);
            }
        );
    }

    #[test]
    fn should_execute_bne_no_zero() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bne,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::ZERO, false);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 51);
            }
        );
    }

    #[test]
    fn should_execute_bne_with_zero() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bne,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::ZERO, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bpl_no_negative() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bpl,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::NEGATIVE, false);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS + 51);
            }
        );
    }

    #[test]
    fn should_execute_bpl_with_negative() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bpl,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::NEGATIVE, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bvc_with_overflow() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bvc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::OVERFLOW, true);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bvc_no_overflow() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bvc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::OVERFLOW, false);
            },
            Some(-51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 51);
            }
        );
    }

    #[test]
    fn should_execute_bvs_without_overflow() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bvs,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::OVERFLOW, false);
            },
            Some(51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS);
            }
        );
    }

    #[test]
    fn should_execute_bvs_with_overflow() {
        const PROGRAM_COUNTER_ADDRESS: Address = 0x0407;
        test_instruction!(
            Cpu::bvs,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.program_counter = PROGRAM_COUNTER_ADDRESS;
                cpu.flags.set(Flags::OVERFLOW, true);
            },
            Some(-51i8 as u8),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.program_counter, PROGRAM_COUNTER_ADDRESS - 51);
            }
        );
    }

    #[test]
    fn should_execute_clc() {
        test_instruction!(
            Cpu::clc,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.flags.set(Flags::CARRY, true);
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
            }
        );
    }

    #[test]
    fn should_execute_cld() {
        test_instruction!(
            Cpu::cld,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.flags.set(Flags::DECIMAL, true);
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::DECIMAL), false);
            }
        );
    }

    #[test]
    fn should_execute_cli() {
        test_instruction!(
            Cpu::cli,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.flags.set(Flags::INTERRUPT_DISABLE, true);
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::INTERRUPT_DISABLE), false);
            }
        );
    }

    #[test]
    fn should_execute_clv() {
        test_instruction!(
            Cpu::clv,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.flags.set(Flags::OVERFLOW, true);
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::OVERFLOW), false);
            }
        );
    }

    #[test]
    fn should_execute_cmp_when_equal() {
        test_instruction!(
            Cpu::cmp,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x5F;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmp_when_smaller() {
        test_instruction!(
            Cpu::cmp,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x5E;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_cmp_when_smaller_not_negative() {
        test_instruction!(
            Cpu::cmp,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x00;
            },
            Some(0xFF),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmp_when_greater() {
        test_instruction!(
            Cpu::cmp,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.accumulator = 0x60;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmx_when_equal() {
        test_instruction!(
            Cpu::cpx,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x5F;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmx_when_smaller() {
        test_instruction!(
            Cpu::cpx,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x5E;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_cmx_when_smaller_no_negative() {
        test_instruction!(
            Cpu::cpx,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x00;
            },
            Some(0xFF),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmx_when_greater() {
        test_instruction!(
            Cpu::cpx,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0xFF;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_cmy_when_equal() {
        test_instruction!(
            Cpu::cpy,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x5F;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmy_when_smaller() {
        test_instruction!(
            Cpu::cpy,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x5E;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_cmy_when_smaller_no_negative() {
        test_instruction!(
            Cpu::cpy,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x00;
            },
            Some(0xFF),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_cmy_when_greater() {
        test_instruction!(
            Cpu::cpy,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0xFF;
            },
            Some(0x5F),
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::CARRY), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_dec() {
        test_instruction!(
            Cpu::dec,
            |_cpu: &mut Cpu, _memory: &mut Memory| {},
            Some(0x71),
            |cpu: &Cpu, memory: &Memory| {
                assert_eq!(get_argument(memory), 0x70);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_dec_wrapping() {
        test_instruction!(
            Cpu::dec,
            |_cpu: &mut Cpu, _memory: &mut Memory| {},
            Some(0x00),
            |cpu: &Cpu, memory: &Memory| {
                assert_eq!(get_argument(memory), 0xFF);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_dec_zero() {
        test_instruction!(
            Cpu::dec,
            |_cpu: &mut Cpu, _memory: &mut Memory| {},
            Some(0x01),
            |cpu: &Cpu, memory: &Memory| {
                assert_eq!(get_argument(memory), 0x00);
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_dex() {
        test_instruction!(
            Cpu::dex,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x61;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.x_index, 0x60);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_dex_wrapping() {
        test_instruction!(
            Cpu::dex,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x00;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.x_index, 0xFF);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_dex_zero() {
        test_instruction!(
            Cpu::dex,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.x_index = 0x01;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.x_index, 0x00);
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_dey() {
        test_instruction!(
            Cpu::dey,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x61;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.y_index, 0x60);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }

    #[test]
    fn should_execute_dey_wrapping() {
        test_instruction!(
            Cpu::dey,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x00;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.y_index, 0xFF);
                assert_eq!(cpu.flags.contains(Flags::ZERO), false);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            }
        );
    }

    #[test]
    fn should_execute_dey_zero() {
        test_instruction!(
            Cpu::dey,
            |cpu: &mut Cpu, _memory: &mut Memory| {
                cpu.y_index = 0x01;
            },
            None,
            |cpu: &Cpu, _memory: &Memory| {
                assert_eq!(cpu.y_index, 0x00);
                assert_eq!(cpu.flags.contains(Flags::ZERO), true);
                assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            }
        );
    }
}
