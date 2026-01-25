use std::rc::Rc;

use monke_core::object::Closure;

#[derive(Debug)]
pub struct Frame {
    func: Rc<Closure>,

    /// instruction pointer
    pub ip: usize,

    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: Rc<Closure>, base_pointer: usize) -> Self {
        Frame {
            func,
            ip: 0,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &[u8] {
        &self.func.func.instructions
    }

    pub fn current_closure(&self) -> Rc<Closure> {
        Rc::clone(&self.func)
    }
}
