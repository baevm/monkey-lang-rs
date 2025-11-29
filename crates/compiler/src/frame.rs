use monke_core::object;

use crate::code::Instructions;

#[derive(Debug)]
pub struct Frame {
    func: object::CompiledFunction,

    /// instruction pointer
    pub ip: i64,
}

impl Frame {
    pub fn new(func: object::CompiledFunction) -> Self {
        Frame { func, ip: -1 }
    }

    pub fn instructions(&self) -> Instructions {
        Instructions::from(self.func.instructions.clone())
    }
}
