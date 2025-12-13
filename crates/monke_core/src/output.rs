use std::cell::RefCell;

thread_local! {
    static OUTPUT_HANDLER: RefCell<Box<dyn OutputHandler>> = RefCell::new(Box::new(StdoutHandler));
}

pub trait AsAny {
    fn as_any(&self) -> &dyn std::any::Any;
}

pub trait OutputHandler: AsAny {
    fn print(&mut self, message: &str);
}

pub struct StdoutHandler;

impl AsAny for StdoutHandler {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl OutputHandler for StdoutHandler {
    fn print(&mut self, message: &str) {
        println!("{}", message);
    }
}

pub struct BufferHandler {
    pub buffer: String,
}

impl BufferHandler {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }
}

impl AsAny for BufferHandler {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl OutputHandler for BufferHandler {
    fn print(&mut self, message: &str) {
        self.buffer.push_str(message);
        self.buffer.push('\n');
    }
}

pub fn set_output_handler(handler: Box<dyn OutputHandler>) {
    OUTPUT_HANDLER.with(|h| *h.borrow_mut() = handler);
}

pub fn print_output(message: &str) {
    OUTPUT_HANDLER.with(|h| h.borrow_mut().print(message));
}

pub fn get_buffer() -> String {
    OUTPUT_HANDLER.with(|h| {
        if let Some(buffer_handler) = h.borrow().as_any().downcast_ref::<BufferHandler>() {
            buffer_handler.buffer.clone()
        } else {
            String::new()
        }
    })
}
