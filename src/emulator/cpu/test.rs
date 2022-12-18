use super::*;

const OPERAND_ADDRESS: Address = 0x0242;

enum Operand {
    Address(Address),
    Value(u8),
    None,
}

fn try_set_operand(memory: &mut Memory, operand: Operand) -> Option<Address> {
    match operand {
        Operand::Address(address) => Some(address),
        Operand::Value(value) => {
            memory.write_u8(OPERAND_ADDRESS, value);
            Some(OPERAND_ADDRESS)
        }
        Operand::None => None,
    }
}

fn get_operand_value(memory: &Memory) -> u8 {
    return memory.read_u8(OPERAND_ADDRESS);
}

macro_rules! test_instruction {
    ($instruction: expr, $given: expr, $arg: expr, $then: expr) => {
        // given
        let mut cpu = Cpu::new();
        let mut memory = Memory::new();
        $given(&mut cpu, &mut memory);
        let arg_address = try_set_operand(&mut memory, $arg);

        // when
        $instruction(&mut cpu, &mut memory, arg_address);

        // then
        $then(&mut cpu, &mut memory);
    };
}

#[test]
fn should_execute_adc() {
    test_instruction!(
        Cpu::adc,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x77;
        },
        Operand::Value(0x03),
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
        Operand::Value(0x00),
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
        Operand::Value(0x40),
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
        Operand::Value(0xFF),
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
        Operand::Value(0b1100_0101),
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
        Operand::Value(0b1000_0111),
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
        Operand::Value(0b0101_1010),
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
        Operand::Value(0b0101_0101),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0b1010_1010);
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
        Operand::Value(0b1000_0000),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0b0000_0000);
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
        Operand::Value(0x42),
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
        Operand::Address(122i8 as u16),
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
        Operand::Address(-71i8 as u16),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(-111i8 as u16),
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
        Operand::Value(51i8 as u8),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(-99i8 as u16),
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
        Operand::Address(-99i8 as u16),
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
        Operand::Value(0b0111_1000),
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
        Operand::Value(0b1111_1000),
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
        Operand::Value(0b0111_1000),
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
        Operand::Value(0b0101_1010),
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
        Operand::Value(51i8 as u8),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(-114i8 as u16),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(51i8 as u16),
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
        Operand::Value(51i8 as u8),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(-51i8 as u16),
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
        Operand::Address(51i8 as u16),
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
        Operand::Address(-51i8 as u16),
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
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
        Operand::Value(0x5F),
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
        Operand::Value(0x5F),
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
        Operand::Value(0xFF),
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
        Operand::Value(0x5F),
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
        Operand::Value(0x5F),
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
        Operand::Value(0x5F),
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
        Operand::Value(0xFF),
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
        Operand::Value(0x5F),
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
        Operand::Value(0x5F),
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
        Operand::Value(0x5F),
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
        Operand::Value(0xFF),
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
        Operand::Value(0x5F),
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
        Operand::Value(0x71),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0x70);
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
        Operand::Value(0x00),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0xFF);
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
        Operand::Value(0x01),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0x00);
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
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
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_eor() {
    test_instruction!(
        Cpu::eor,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0110_1001;
        },
        Operand::Value(0b0111_1111),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0001_0110);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_eor_when_zero() {
    test_instruction!(
        Cpu::eor,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0110_1001;
        },
        Operand::Value(0b0110_1001),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0000_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_eor_when_negative() {
    test_instruction!(
        Cpu::eor,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0000_1001;
        },
        Operand::Value(0b1110_1001),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b1110_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_inc() {
    test_instruction!(
        Cpu::inc,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0x56),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0x57);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_inc_wrapping() {
    test_instruction!(
        Cpu::inc,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0xFF),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_inc_when_negative() {
    test_instruction!(
        Cpu::inc,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0x9B),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0x9C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_inx() {
    test_instruction!(
        Cpu::inx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x61;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x62);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_inx_wrapping() {
    test_instruction!(
        Cpu::inx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0xFF;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_inx_negativeg() {
    test_instruction!(
        Cpu::inx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 127;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 128);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_iny() {
    test_instruction!(
        Cpu::iny,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x61;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0x62);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_iny_wrapping() {
    test_instruction!(
        Cpu::iny,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0xFF;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_iny_negativeg() {
    test_instruction!(
        Cpu::iny,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 127;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 128);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_jmp() {
    test_instruction!(
        Cpu::jmp,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.program_counter = 0x0200;
        },
        Operand::Address(0x0678),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.program_counter, 0x0678);
        }
    );
}

#[test]
fn should_execute_jsr() {
    test_instruction!(
        Cpu::jsr,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.program_counter = 0x02FF;
        },
        Operand::Address(0x0678),
        |cpu: &mut Cpu, memory: &mut Memory| {
            assert_eq!(cpu.program_counter, 0x0678);
            assert_eq!(memory.stack(&mut cpu.stack_pointer).pop_u16(), 0x02FF);
        }
    );
}

#[test]
fn should_execute_lda() {
    test_instruction!(
        Cpu::lda,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0x78),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 0x78);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_lda_when_zero() {
    test_instruction!(
        Cpu::lda,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x42;
        },
        Operand::Value(0x00),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_lda_when_negative() {
    test_instruction!(
        Cpu::lda,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(129),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 129);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_ldx() {
    test_instruction!(
        Cpu::ldx,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0x78),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.x_index, 0x78);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ldx_when_zero() {
    test_instruction!(
        Cpu::ldx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x42;
        },
        Operand::Value(0x00),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.x_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ldx_when_negative() {
    test_instruction!(
        Cpu::ldx,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(129),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.x_index, 129);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_ldy() {
    test_instruction!(
        Cpu::ldy,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0x78),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.y_index, 0x78);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ldy_when_zero() {
    test_instruction!(
        Cpu::ldy,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x42;
        },
        Operand::Value(0x00),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.y_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ldy_when_negative() {
    test_instruction!(
        Cpu::ldy,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(129),
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.y_index, 129);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_lsr_accumulator() {
    test_instruction!(
        Cpu::lsr,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b1110_1010;
        },
        Operand::None,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 0b0111_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_lsr_memory() {
    test_instruction!(
        Cpu::lsr,
        |_cpu: &mut Cpu, _memory: &mut Memory| {},
        Operand::Value(0b1110_1010),
        |cpu: &mut Cpu, memory: &mut Memory| {
            assert_eq!(get_operand_value(memory), 0b0111_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_lsr_with_carry() {
    test_instruction!(
        Cpu::lsr,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b1110_1011;
        },
        Operand::None,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 0b0111_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_lsr_with_zero() {
    test_instruction!(
        Cpu::lsr,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0000_0001;
        },
        Operand::None,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            assert_eq!(cpu.accumulator, 0b0000_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_ora() {
    test_instruction!(
        Cpu::ora,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0110_1001;
        },
        Operand::Value(0b0111_1110),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0111_1111);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ora_when_zero() {
    test_instruction!(
        Cpu::ora,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0000_0000;
        },
        Operand::Value(0b0000_0000),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0000_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_ora_when_negative() {
    test_instruction!(
        Cpu::ora,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0b0000_1001;
        },
        Operand::Value(0b1110_1001),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b1110_1001);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_pha() {
    test_instruction!(
        Cpu::pha,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x93;
        },
        Operand::None,
        |cpu: &mut Cpu, memory: &mut Memory| {
            assert_eq!(memory.stack(&mut cpu.stack_pointer).pop_u8(), 0x93);
        }
    );
}

#[test]
fn should_execute_php() {
    test_instruction!(
        Cpu::php,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.bits = 0b0101_1010;
        },
        Operand::None,
        |cpu: &mut Cpu, memory: &mut Memory| {
            assert_eq!(memory.stack(&mut cpu.stack_pointer).pop_u8(), 0b0101_1010);
        }
    );
}

#[test]
fn should_execute_pla() {
    test_instruction!(
        Cpu::pla,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.accumulator = 0x17;
            memory
                .stack(&mut cpu.stack_pointer)
                .push_u8(cpu.accumulator);
            cpu.accumulator = 0x42;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x17);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_pla_with_zero() {
    test_instruction!(
        Cpu::pla,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.accumulator = 0x00;
            memory
                .stack(&mut cpu.stack_pointer)
                .push_u8(cpu.accumulator);
            cpu.accumulator = 0x42;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_pla_with_negative() {
    test_instruction!(
        Cpu::pla,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.accumulator = 0x9B;
            memory
                .stack(&mut cpu.stack_pointer)
                .push_u8(cpu.accumulator);
            cpu.accumulator = 0x42;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x9B);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_plp() {
    test_instruction!(
        Cpu::plp,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.flags.bits = 0b0101_1010;
            memory.stack(&mut cpu.stack_pointer).push_u8(cpu.flags.bits);
            cpu.flags.bits = 0b1011_0111;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.flags.bits(), 0b0101_1010);
        }
    );
}

#[test]
fn should_execute_rol_accumulator() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b0010_1001;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0101_0010);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_rol_memory() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
        },
        Operand::Value(0b0010_1001),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0b0101_0010);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_rol_with_carry_input() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, true);
            cpu.accumulator = 0b0010_1001;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0101_0011);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_rol_with_carry_output() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b1010_1001;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0101_0010);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_rol_with_zero() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b1000_0000;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0000_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_rol_with_negative() {
    test_instruction!(
        Cpu::rol,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b0100_1000;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b1001_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_ror_accumulator() {
    test_instruction!(
        Cpu::ror,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b1010_1010;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0101_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_ror_memory() {
    test_instruction!(
        Cpu::ror,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
        },
        Operand::Value(0b1010_1010),
        |cpu: &Cpu, memory: &Memory| {
            assert_eq!(get_operand_value(memory), 0b0101_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_ror_with_carry_input() {
    test_instruction!(
        Cpu::ror,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, true);
            cpu.accumulator = 0b1010_1010;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b1101_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_ror_with_carry_output() {
    test_instruction!(
        Cpu::ror,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b1010_1011;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0101_0101);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_ror_with_zero() {
    test_instruction!(
        Cpu::ror,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0b0000_0001;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0b0000_0000);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_rti_after_brk() {
    test_instruction!(
        Cpu::rti,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.flags = Flags::from_bits(0b0101_1010).unwrap();
            cpu.program_counter = 0x0277;
            cpu.brk(memory, None);
            cpu.flags = Flags::from_bits(0b1100_0110).unwrap();
            cpu.program_counter = 0x0491;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.program_counter, 0x0277);
            assert_eq!(cpu.flags.bits(), 0b0101_1010);
        }
    );
}

#[test]
fn should_execute_rts_after_jrs() {
    test_instruction!(
        Cpu::rts,
        |cpu: &mut Cpu, memory: &mut Memory| {
            cpu.program_counter = 0x0277;
            cpu.jsr(memory, Some(0x0388));
            cpu.program_counter = 0x0491;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.program_counter, 0x0277);
        }
    );
}

#[test]
fn should_execute_sbc() {
    test_instruction!(
        Cpu::sbc,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0x79;
        },
        Operand::Value(0x1F),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x59);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_sbc_with_negative() {
    test_instruction!(
        Cpu::sbc,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0x1F;
        },
        Operand::Value(0x79),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0xA5);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_sbc_with_zero() {
    test_instruction!(
        Cpu::sbc,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, true);
            cpu.accumulator = 0xBF;
        },
        Operand::Value(0xBF),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_sbc_with_carry() {
    test_instruction!(
        Cpu::sbc,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
            cpu.accumulator = 0xBF;
        },
        Operand::Value(0xBF),
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0xFF);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
            assert_eq!(cpu.flags.contains(Flags::CARRY), false);
        }
    );
}

#[test]
fn should_execute_sec() {
    test_instruction!(
        Cpu::sec,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::CARRY, false);
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.flags.contains(Flags::CARRY), true);
        }
    );
}

#[test]
fn should_execute_sed() {
    test_instruction!(
        Cpu::sed,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::DECIMAL, false);
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.flags.contains(Flags::DECIMAL), true);
        }
    );
}

#[test]
fn should_execute_sei() {
    test_instruction!(
        Cpu::sei,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.flags.set(Flags::INTERRUPT_DISABLE, false);
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.flags.contains(Flags::INTERRUPT_DISABLE), true);
        }
    );
}

#[test]
fn should_execute_sta() {
    test_instruction!(
        Cpu::sta,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x8C;
        },
        Operand::Address(0x0271),
        |_cpu: &Cpu, memory: &Memory| {
            assert_eq!(memory.read_u8(0x0271), 0x8C);
        }
    );
}

#[test]
fn should_execute_stx() {
    test_instruction!(
        Cpu::stx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x8C;
        },
        Operand::Address(0x0271),
        |_cpu: &Cpu, memory: &Memory| {
            assert_eq!(memory.read_u8(0x0271), 0x8C);
        }
    );
}

#[test]
fn should_execute_sty() {
    test_instruction!(
        Cpu::sty,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x8C;
        },
        Operand::Address(0x0271),
        |_cpu: &Cpu, memory: &Memory| {
            assert_eq!(memory.read_u8(0x0271), 0x8C);
        }
    );
}

#[test]
fn should_execute_tax() {
    test_instruction!(
        Cpu::tax,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x6C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tax_with_zero() {
    test_instruction!(
        Cpu::tax,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x91;
            cpu.accumulator = 0x00;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tax_with_negative() {
    test_instruction!(
        Cpu::tax,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0xD4;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0xD4);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_tay() {
    test_instruction!(
        Cpu::tay,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0x6C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tay_with_zero() {
    test_instruction!(
        Cpu::tay,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x91;
            cpu.accumulator = 0x00;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tay_with_negative() {
    test_instruction!(
        Cpu::tay,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0xD4;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.y_index, 0xD4);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_tsx() {
    test_instruction!(
        Cpu::tsx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.stack_pointer = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x6C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tsx_with_zero() {
    test_instruction!(
        Cpu::tsx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0xF4;
            cpu.stack_pointer = 0x00;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tsx_with_negative() {
    test_instruction!(
        Cpu::tsx,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.stack_pointer = 0xC9;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.x_index, 0xC9);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_txa() {
    test_instruction!(
        Cpu::txa,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x6C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_txa_with_zero() {
    test_instruction!(
        Cpu::txa,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0x43;
            cpu.x_index = 0x00;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_txa_with_negative() {
    test_instruction!(
        Cpu::txa,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x91;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x91);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}

#[test]
fn should_execute_txs() {
    test_instruction!(
        Cpu::txs,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.x_index = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.stack_pointer, 0x6C);
        }
    );
}

#[test]
fn should_execute_tya() {
    test_instruction!(
        Cpu::tya,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x6C;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x6C);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tya_with_zero() {
    test_instruction!(
        Cpu::tya,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.accumulator = 0xB2;
            cpu.y_index = 0x00;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x00);
            assert_eq!(cpu.flags.contains(Flags::ZERO), true);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), false);
        }
    );
}

#[test]
fn should_execute_tya_with_negative() {
    test_instruction!(
        Cpu::tya,
        |cpu: &mut Cpu, _memory: &mut Memory| {
            cpu.y_index = 0x97;
        },
        Operand::None,
        |cpu: &Cpu, _memory: &Memory| {
            assert_eq!(cpu.accumulator, 0x97);
            assert_eq!(cpu.flags.contains(Flags::ZERO), false);
            assert_eq!(cpu.flags.contains(Flags::NEGATIVE), true);
        }
    );
}
