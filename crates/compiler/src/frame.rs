use monke_core::object;

use crate::code::Instructions;

#[derive(Debug)]
pub struct Frame {
    func: object::Closure,

    /// instruction pointer
    pub ip: i64,

    pub base_pointer: i64,
}

impl Frame {
    pub fn new(func: object::Closure, base_pointer: i64) -> Self {
        Frame {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> Instructions {
        Instructions::from(self.func.func.instructions.clone())
    }
}
