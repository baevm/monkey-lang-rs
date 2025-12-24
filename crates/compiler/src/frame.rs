use monke_core::object::Closure;

use crate::code::Instructions;

#[derive(Debug)]
pub struct Frame {
    func: Closure,

    /// instruction pointer
    pub ip: i64,

    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: Closure, base_pointer: usize) -> Self {
        Frame {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> Instructions {
        Instructions::from(self.func.func.instructions.clone())
    }

    pub fn current_closure(&self) -> Closure {
        self.func.clone()
    }
}
