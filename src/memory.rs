const MEMORY_SIZE: usize = 0xFFFF;

pub struct Memory {
    data: [u8; MEMORY_SIZE],
}

impl Memory {
    pub fn new() -> Memory {
        Memory{ data: [0; MEMORY_SIZE]}
    }

    pub fn fetch_u8(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    pub fn fetch_u16(&self, address: u16) -> u16 {
        u16::from_le_bytes([self.fetch_u8(address), self.fetch_u8(address + 1)])
    }

    pub fn stack_push_u8(&mut self, stack_pointer: &mut u8, data: u8) {
        self.data[*stack_pointer as usize] = data;
        *stack_pointer += 1;
    }

    pub fn stack_push_u16(&mut self, stack_pointer: &mut u8, data: u16) {
        let data_bytes = data.to_le_bytes();
        self.data[*stack_pointer as usize] = data_bytes[0];
        self.data[*stack_pointer as usize + 1] = data_bytes[1];
        *stack_pointer += 2;
    }

    pub fn stack_pop_u8(&mut self, stack_pointer: &mut u8) -> u8 {
        let result = self.fetch_u8(*stack_pointer as u16);
        *stack_pointer -= 1;
        return result;
    }

    pub fn stack_pop_u16(&mut self, stack_pointer: &mut u8) -> u16 {
        let result = self.fetch_u16(*stack_pointer as u16);
        *stack_pointer -= 2;
        return result;
    }
}
