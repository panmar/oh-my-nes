const MEMORY_SIZE: usize = 0xFFFF + 1;
const STACK_BEGIN: usize = 0x0100;
const STACK_END: usize = 0x01FF;

pub type Address = u16;

pub struct Memory {
    data: [u8; MEMORY_SIZE],
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            data: [0; MEMORY_SIZE],
        }
    }

    pub fn fetch_u8(&self, address: Address) -> u8 {
        self.data[address as usize]
    }

    pub fn fetch_u16(&self, address: Address) -> u16 {
        u16::from_le_bytes([self.fetch_u8(address), self.fetch_u8(address + 1)])
    }

    pub fn set_u8(&mut self, address: Address, value: u8) {
        self.data[address as usize] = value;
    }
}

/// A stack view on the memory
pub struct Stack<'a> {
    data: &'a mut [u8],
    stack_pointer: &'a mut u8,
}

impl Memory {
    pub fn stack<'a>(&'a mut self, stack_pointer: &'a mut u8) -> Stack<'a> {
        Stack {
            data: &mut self.data[STACK_BEGIN..STACK_END + 1],
            stack_pointer,
        }
    }
}

impl Stack<'_> {
    pub fn push_u8(&mut self, value: u8) {
        self.data[*self.stack_pointer as usize] = value;
        *self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    pub fn push_u16(&mut self, value: u16) {
        let data_bytes = value.to_le_bytes();
        for byte in data_bytes {
            self.push_u8(byte);
        }
    }

    pub fn pop_u8(&mut self) -> u8 {
        *self.stack_pointer = self.stack_pointer.wrapping_add(1);
        return self.data[*self.stack_pointer as usize];
    }

    pub fn pop_u16(&mut self) -> u16 {
        let b1 = self.pop_u8();
        let b2 = self.pop_u8();
        return u16::from_le_bytes([b2, b1]);
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
