use std::collections::HashMap;

use monke_core::object::{
    self, Array, Boolean, HashKey, HashObj, HashPair, Integer, Null, Object, StringObj,
};

use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    frame::Frame,
};

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

pub struct Vm {
    constants: Vec<Object>,

    stack: Vec<Object>,
    sp: usize, // stack pointer - always points to the next value.

    globals: Vec<Object>,

    frames: Vec<Frame>,
    frame_index: usize,
}

#[derive(Debug)]
pub enum VmError {
    StackOverflowError(StackOverflowError),
    InvalidType,
    UnknownOperator,
    UnusableAsHashKey,
    UnsupportedIndexOperator,
    CallingNonFunction,
}

#[derive(Debug)]
pub struct StackOverflowError;

impl std::fmt::Display for StackOverflowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack overflow")
    }
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        let main_func = object::CompiledFunction {
            instructions: bytecode.instructions.to_vec(),
            num_locals: 0,
        };
        let main_frame = Frame::new(main_func, 0);

        let mut frames: Vec<Frame> = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants: bytecode.constants,
            stack: vec![Object::Null(Box::new(Null {})); STACK_SIZE as usize],
            sp: 0,
            globals: vec![Object::Null(Box::new(Null {})); GLOBALS_SIZE],
            frame_index: 1,
            frames,
        }
    }

    pub fn new_with_state(bytecode: Bytecode, globals: Vec<Object>) -> Self {
        let mut vm = Vm::new(bytecode);
        vm.globals = globals;

        vm
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let mut i = 0;
        let mut ins: Instructions = Instructions::new();
        let mut opcode: Option<Opcode> = None;

        while self.current_frame().ip < ((self.current_frame().instructions().len() - 1) as i64) {
            self.update_frame_pointer(1);

            i = self.current_frame().ip;
            ins = self.current_frame().instructions();
            opcode = Opcode::from_byte(ins[i as usize]);

            let Some(opcode) = opcode else {
                continue;
            };

            match opcode {
                Opcode::OpConstant => {
                    // first byte is opcode, read 2 bytes from i+1..i+3
                    let const_index = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );
                    self.update_frame_pointer(2);

                    let constant = self.constants[const_index as usize].clone();

                    self.push(constant)?
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Object::Integer(left_int), Object::Integer(right_int)) => {
                            let left_value = left_int.value;
                            let right_value = right_int.value;

                            let result = match opcode {
                                Opcode::OpAdd => left_value + right_value,
                                Opcode::OpSub => left_value - right_value,
                                Opcode::OpMul => left_value * right_value,
                                Opcode::OpDiv => left_value / right_value,
                                _ => unreachable!(),
                            };

                            self.push(Object::Integer(Box::new(Integer { value: result })))?
                        }
                        (Object::String(left_str), Object::String(right_str)) => {
                            let mut left_value = left_str.value;
                            let right_value = right_str.value;
                            left_value.push_str(&right_value);

                            let result = Object::String(Box::new(StringObj { value: left_value }));
                            self.push(result)?
                        }
                        _ => return Err(VmError::InvalidType),
                    }
                }
                Opcode::OpPop => {
                    self.pop();
                }
                Opcode::OpTrue => self.push(Object::Boolean(Box::new(Boolean { value: true })))?,
                Opcode::OpFalse => {
                    self.push(Object::Boolean(Box::new(Boolean { value: false })))?
                }
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.execute_comparison(&opcode)?
                }
                Opcode::OpMinus => self.execute_minus_operator()?,
                Opcode::OpBang => self.execute_bang_operator()?,
                Opcode::OpJumpNotTruthy => {
                    let pos = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );

                    self.update_frame_pointer(2);

                    let condition = self.pop();

                    if !self.is_truthy(&condition) {
                        self.set_frame_pointer(pos as i64 - 1);
                    }
                }
                Opcode::OpJump => {
                    let pos = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );
                    self.set_frame_pointer(pos as i64 - 1);
                }
                Opcode::OpNull => self.push(Object::Null(Box::new(Null {})))?,
                Opcode::OpSetGlobal => {
                    let global_idx: usize = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    )
                    .try_into()
                    .unwrap();

                    self.update_frame_pointer(2);

                    self.globals[global_idx] = self.pop();
                }
                Opcode::OpGetGlobal => {
                    let global_idx: usize = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    )
                    .try_into()
                    .unwrap();

                    self.update_frame_pointer(2);

                    let obj = self.globals[global_idx].clone();
                    self.push(obj)?;
                }
                Opcode::OpArray => {
                    let num_elements: usize = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    )
                    .try_into()
                    .unwrap();

                    self.update_frame_pointer(2);

                    let array = self.build_array(self.sp - num_elements, self.sp);

                    self.sp = self.sp - num_elements;

                    self.push(array)?
                }
                Opcode::OpHash => {
                    let num_elements: usize = u16::from_be_bytes(
                        ins[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    )
                    .try_into()
                    .unwrap();

                    self.update_frame_pointer(2);

                    let hash = self.build_hash(self.sp - num_elements, self.sp)?;

                    self.sp = self.sp - num_elements;

                    self.push(hash)?
                }
                Opcode::OpIndex => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?
                }
                Opcode::OpCall => {
                    self.update_frame_pointer(1); // TODO

                    let func = match &self.stack[self.sp - 1] {
                        Object::CompiledFunction(f) => f.clone(),
                        _ => return Err(VmError::CallingNonFunction),
                    };

                    let frame = Frame::new(*func.clone(), self.sp as i64);
                    let bp = frame.base_pointer;
                    self.push_frame(frame);
                    self.sp = (bp + func.num_locals) as usize;
                }
                Opcode::OpReturnValue => {
                    let return_value = self.pop();
                    let frame = self.pop_frame().expect("frame doesnt exist");
                    self.sp = (frame.base_pointer - 1) as usize;

                    self.push(return_value)?
                }
                Opcode::OpReturn => {
                    let frame = self.pop_frame().expect("frame doesnt exist");
                    self.sp = (frame.base_pointer - 1) as usize;

                    self.push(Object::Null(Box::new(Null {})))?
                }
                Opcode::OpSetLocal => {
                    let local_index = u8::from_be_bytes([ins[(i + 1) as usize]]);
                    self.update_frame_pointer(1);

                    let base_pointer = self.current_frame_base_pointer();
                    self.stack[(base_pointer + (local_index as i64)) as usize] = self.pop();
                }
                Opcode::OpGetLocal => {
                    let local_index = u8::from_be_bytes([ins[(i + 1) as usize]]);
                    self.update_frame_pointer(1);

                    let base_pointer = self.current_frame_base_pointer();

                    let obj = self.stack[(base_pointer + (local_index as i64)) as usize].clone();
                    self.push(obj)?
                }
            }

            i += 1;
        }

        Ok(())
    }

    pub fn globals(&self) -> Vec<Object> {
        self.globals.clone()
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frame_index - 1]
    }

    fn current_frame_base_pointer(&self) -> i64 {
        self.frames[self.frame_index - 1].base_pointer
    }

    fn update_frame_pointer(&mut self, to_add: i64) {
        self.frames[self.frame_index - 1].ip += to_add
    }

    fn set_frame_pointer(&mut self, offset: i64) {
        self.frames[self.frame_index - 1].ip = offset;
    }

    fn push_frame(&mut self, frame: Frame) {
        if self.frames.len() == self.frame_index {
            self.frames.push(frame);
        } else {
            self.frames[self.frame_index] = frame;
        }

        self.frame_index += 1;
    }

    fn pop_frame(&mut self) -> Option<&Frame> {
        self.frame_index -= 1;
        self.frames.get(self.frame_index)
    }

    fn is_truthy(&self, object: &Object) -> bool {
        match object {
            Object::Boolean(bool_obj) => bool_obj.value,
            Object::Null(_) => false,
            _ => true,
        }
    }

    fn push(&mut self, object: Object) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflowError(StackOverflowError {}));
        }

        self.stack[self.sp] = object;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let object = &self.stack[self.sp - 1];
        self.sp -= 1;
        object.clone()
    }

    pub fn last_popped_stack_element(&self) -> Option<Object> {
        self.stack.get(self.sp).cloned()
    }

    fn execute_comparison(&mut self, opcode: &Opcode) -> Result<(), VmError> {
        let right = self.pop();
        let left = self.pop();

        if let Object::Integer(right_int) = &right
            && let Object::Integer(left_int) = &left
        {
            return self.execute_integer_comparison(&opcode, left_int, right_int);
        }

        if let Object::Boolean(right_bool) = right
            && let Object::Boolean(left_bool) = left
        {
            match opcode {
                Opcode::OpEqual => {
                    self.push(self.native_bool_to_boolean_obj(right_bool.value == left_bool.value))?
                }
                Opcode::OpNotEqual => {
                    self.push(self.native_bool_to_boolean_obj(right_bool.value != left_bool.value))?
                }
                Opcode::OpGreaterThan => {
                    self.push(self.native_bool_to_boolean_obj(right_bool.value > left_bool.value))?
                }
                _ => Err(VmError::UnknownOperator)?,
            }

            return Ok(());
        }

        // TODO: add comparsion between diff objects
        // match opcode {
        //     Opcode::OpEqual => self.push(self.native_bool_to_boolean_obj(right == left))?,
        //     Opcode::OpNotEqual => self.push(self.native_bool_to_boolean_obj(right != left))?,
        //     _ => return Err(VmError::UnknownOperator),
        // }

        Err(VmError::InvalidType)
    }

    fn execute_integer_comparison(
        &mut self,
        opcode: &Opcode,
        left_int: &Box<Integer>,
        right_int: &Box<Integer>,
    ) -> Result<(), VmError> {
        match opcode {
            Opcode::OpEqual => {
                self.push(self.native_bool_to_boolean_obj(right_int.value == left_int.value))
            }
            Opcode::OpNotEqual => {
                self.push(self.native_bool_to_boolean_obj(right_int.value != left_int.value))
            }
            Opcode::OpGreaterThan => {
                self.push(self.native_bool_to_boolean_obj(left_int.value > right_int.value))
            }
            _ => Err(VmError::UnknownOperator),
        }
    }

    fn native_bool_to_boolean_obj(&self, val: bool) -> Object {
        if val {
            return Object::Boolean(Box::new(Boolean { value: true }));
        }

        return Object::Boolean(Box::new(Boolean { value: false }));
    }

    fn execute_bang_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop();

        match operand {
            Object::Boolean(bool_obj) => {
                if bool_obj.value == true {
                    self.push(Object::Boolean(Box::new(Boolean { value: false })))
                } else {
                    self.push(Object::Boolean(Box::new(Boolean { value: true })))
                }
            }
            Object::Null(_) => self.push(Object::Boolean(Box::new(Boolean { value: true }))),
            _ => self.push(Object::Boolean(Box::new(Boolean { value: false }))),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop();

        let Object::Integer(int_obj) = operand else {
            return Err(VmError::InvalidType);
        };

        self.push(Object::Integer(Box::new(Integer {
            value: -int_obj.value,
        })))
    }

    fn build_array(&self, start_idx: usize, end_idx: usize) -> Object {
        let mut elements = vec![Object::Null(Box::new(Null {})); end_idx - start_idx];

        // todo: avoid cloning
        for i in start_idx..end_idx {
            elements[i - start_idx] = self.stack[i].clone();
        }

        Object::Array(Box::new(Array { elements }))
    }

    fn build_hash(&self, start_idx: usize, end_idx: usize) -> Result<Object, VmError> {
        let mut hashed_pairs: HashMap<HashKey, HashPair> = HashMap::new();

        for i in (start_idx..end_idx).step_by(2) {
            let key = &self.stack[i];
            let value = &self.stack[i + 1];

            let pair = HashPair {
                key: key.clone(),
                value: value.clone(),
            };

            let Some(hash_key) = key.get_hash() else {
                return Err(VmError::UnusableAsHashKey);
            };

            hashed_pairs.insert(hash_key, pair);
        }

        Ok(Object::HashObj(Box::new(HashObj {
            pairs: hashed_pairs,
        })))
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<(), VmError> {
        match (left, &index) {
            (Object::Array(arr_obj), Object::Integer(int_index)) => {
                self.execute_array_index(arr_obj, int_index.as_ref())
            }
            (Object::HashObj(hash_obj), _) => self.execute_hash_index(hash_obj, &index),
            _ => Err(VmError::UnsupportedIndexOperator),
        }
    }

    fn execute_array_index(
        &mut self,
        arr_obj: Box<Array>,
        int_index: &Integer,
    ) -> Result<(), VmError> {
        let i = int_index.value;

        if arr_obj.elements.len() < 1 {
            return self.push(Object::Null(Box::new(Null {})));
        }

        let max: i64 = (arr_obj.elements.len() - 1).try_into().unwrap();

        if i < 0 || i > max {
            return self.push(Object::Null(Box::new(Null {})));
        }

        self.push(arr_obj.elements[i as usize].clone())
    }

    fn execute_hash_index(
        &mut self,
        hash_obj: Box<HashObj>,
        index: &Object,
    ) -> Result<(), VmError> {
        let Some(index_hash) = index.get_hash() else {
            return Err(VmError::UnusableAsHashKey);
        };

        let Some(pair) = hash_obj.pairs.get(&index_hash) else {
            return self.push(Object::Null(Box::new(Null {})));
        };

        self.push(pair.value.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use monke_core::{
        ast,
        lexer::Lexer,
        object::{HashKey, HashableKey, Integer, Null, Object},
        parser::Parser,
    };

    use crate::{compiler::Compiler, vm::Vm};

    enum Expected {
        Integer(i64),
        Boolean(bool),
        Null(Null),
        String(String),
        Array(Vec<Expected>),
        Hash(HashMap<HashKey, Expected>),
    }

    struct VmTestCase {
        input: String,
        expected: Expected,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase {
                input: "1".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "1 + 2".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "1 - 2".to_string(),
                expected: Expected::Integer(-1),
            },
            VmTestCase {
                input: "1 * 2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "4 / 2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "50 / 2 * 2 + 10 - 5".to_string(),
                expected: Expected::Integer(55),
            },
            VmTestCase {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: Expected::Integer(32),
            },
            VmTestCase {
                input: "5 * 2 + 10".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "5 + 2 * 10".to_string(),
                expected: Expected::Integer(25),
            },
            VmTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: Expected::Integer(60),
            },
            VmTestCase {
                input: "-5".to_string(),
                expected: Expected::Integer(-5),
            },
            VmTestCase {
                input: "-10".to_string(),
                expected: Expected::Integer(-10),
            },
            VmTestCase {
                input: "-50 + 100 + -50".to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: Expected::Integer(50),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            VmTestCase {
                input: "true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 < 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "1 > 2".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 < 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 > 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 == 1".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "1 != 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 == 2".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 != 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false == false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "true != false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false != true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(1 < 2) == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(1 < 2) == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "(1 > 2) == true".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "(1 > 2) == false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!true".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!5".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!!true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!!false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!!5".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!(if (false) { 5; })".to_string(),
                expected: Expected::Boolean(true),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestCase {
                input: "if (true) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (true) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (true) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (false) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "if (1) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "if (false) { 10 }".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VmTestCase {
                input: "let one = 1; one".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "let one = 1; let two = 2; one + two".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "let one = 1; let two = one + one; one + two".to_string(),
                expected: Expected::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestCase {
                input: r#""monkey""#.to_string(),
                expected: Expected::String("monkey".to_string()),
            },
            VmTestCase {
                input: r#""mon" + "key""#.to_string(),
                expected: Expected::String("monkey".to_string()),
            },
            VmTestCase {
                input: r#""mon" + "key" + "banana""#.to_string(),
                expected: Expected::String("monkeybanana".to_string()),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestCase {
                input: "[]".to_string(),
                expected: Expected::Array(vec![]),
            },
            VmTestCase {
                input: "[1,2,3]".to_string(),
                expected: Expected::Array(vec![
                    Expected::Integer(1),
                    Expected::Integer(2),
                    Expected::Integer(3),
                ]),
            },
            VmTestCase {
                input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
                expected: Expected::Array(vec![
                    Expected::Integer(3),
                    Expected::Integer(12),
                    Expected::Integer(11),
                ]),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            VmTestCase {
                input: "{}".to_string(),
                expected: Expected::Hash(HashMap::new()),
            },
            VmTestCase {
                input: "{1: 2, 2: 3}".to_string(),
                expected: Expected::Hash(HashMap::from([
                    (create_hash_key(1), Expected::Integer(2)),
                    (create_hash_key(2), Expected::Integer(3)),
                ])),
            },
            VmTestCase {
                input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
                expected: Expected::Hash(HashMap::from([
                    (create_hash_key(2), Expected::Integer(4)),
                    (create_hash_key(6), Expected::Integer(16)),
                ])),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            VmTestCase {
                input: "[1, 2, 3][1]".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "[1, 2, 3][0 + 2]".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "[[1, 1, 1]][0][0]".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "[][0]".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "[1, 2, 3][99]".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "[1][-1]".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[1]".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[2]".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "{1: 1}[0]".to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "{}[0]".to_string(),
                expected: Expected::Null(Null {}),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_function_no_arguments() {
        let tests = vec![
            VmTestCase {
                input: "
                let fivePlusTen = function() {5 + 10;};
                fivePlusTen();
            "
                .to_string(),
                expected: Expected::Integer(15),
            },
            VmTestCase {
                input: "
                let one = function() { 1; };
                let two = function() { 2; };
                one() + two()
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
                let a = function() { 1 };
                let b = function() { a() + 1 };
                let c = function() { b() + 1 };
                c()
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_function_with_return_statement() {
        let tests = vec![
            VmTestCase {
                input: "
                let earlyExit = function() { return 99; 100; };
                earlyExit();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
            VmTestCase {
                input: "
                let earlyExit = function() { return 99; return 100; };
                earlyExit();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_function_without_return_value() {
        let tests = vec![
            VmTestCase {
                input: "
               let noReturn = function() { };
                noReturn();
            "
                .to_string(),
                expected: Expected::Null(Null {}),
            },
            VmTestCase {
                input: "
                let noReturn = function() { };
                let noReturnTwo = function() { noReturn(); };
                noReturn();
                noReturnTwo();
            "
                .to_string(),
                expected: Expected::Null(Null {}),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_first_class_functions() {
        let tests = vec![
            VmTestCase {
                input: "
                let returnsOne = function() { 1; };
                let returnsOneReturner = function() { returnsOne; };
                returnsOneReturner()();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "
               let returnsOneReturner = function() {
                let returnsOne = function() { 1; };
                returnsOne;
                };
                returnsOneReturner()();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            VmTestCase {
                input: "
               let one = function() { let one = 1; one };
                one();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "
               let oneAndTwo = function() { let one = 1; let two = 2; one + two; };
                oneAndTwo();
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
               let oneAndTwo = function() { let one = 1; let two = 2; one + two; };
                let threeAndFour = function() { let three = 3; let four = 4; three + four; };
                oneAndTwo() + threeAndFour();
            "
                .to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "
               let firstFoobar = function() { let foobar = 50; foobar; };
                let secondFoobar = function() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();
            "
                .to_string(),
                expected: Expected::Integer(150),
            },
            VmTestCase {
                input: "
               let globalSeed = 50;
                let minusOne = function() {
                let num = 1;
                globalSeed - num;
                }
                let minusTwo = function() {
                let num = 2;
                globalSeed - num;
                }
                minusOne() + minusTwo();
            "
                .to_string(),
                expected: Expected::Integer(97),
            },
        ];

        run_vm_tests(tests);
    }

    fn create_hash_key(value: i64) -> HashKey {
        let val = Integer { value };
        val.hash_key()
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let program = parse(test.input);
            let mut compiler = Compiler::new();

            if let Err(err) = compiler.compile(program) {
                panic!("compiler error: {:?}", err);
            }

            let mut vm = Vm::new(compiler.bytecode());

            if let Err(err) = vm.run() {
                panic!("vm error: {:?}", err);
            }

            let stack_element = vm.last_popped_stack_element();

            let Some(stack_element) = &stack_element else {
                panic!("stack is empty");
            };

            test_expected_object(&test.expected, &stack_element);
        }
    }

    fn test_expected_object(expected: &Expected, actual: &Object) {
        match expected {
            Expected::Integer(int) => {
                test_integer_object(&int, actual);
            }
            Expected::Boolean(boolean) => {
                test_boolean_object(boolean, actual);
            }
            Expected::Null(_) => {
                let Object::Null(_) = actual else {
                    panic!("object is not Null. got: {}", actual)
                };
            }
            Expected::String(str_val) => test_string_object(&str_val, actual),
            Expected::Array(arr) => test_array_object(arr, actual),
            Expected::Hash(hash_map) => test_hash_object(hash_map, actual),
        }
    }

    fn test_hash_object(expected: &HashMap<HashKey, Expected>, actual: &Object) {
        let Object::HashObj(actual_hash_obj) = actual else {
            panic!("object is not HashObj. got: {}", actual);
        };

        assert_eq!(
            expected.len(),
            actual_hash_obj.pairs.len(),
            "hash has wrong number of pairs. want: {}, got:{}",
            expected.len(),
            actual_hash_obj.pairs.len()
        );

        for (expected_key, expected_val) in expected {
            let pair = actual_hash_obj
                .pairs
                .iter()
                .find(|p| p.0.value == expected_key.value)
                .expect("no pair for given key in pairs");

            test_expected_object(expected_val, &pair.1.value);
        }
    }

    fn test_array_object(expected: &[Expected], actual: &Object) {
        let Object::Array(actual_arr) = actual else {
            panic!("object is not Array. got: {}", actual);
        };

        assert_eq!(
            actual_arr.elements.len(),
            expected.len(),
            "wrong num of elements, want: {}, got: {}",
            expected.len(),
            actual_arr.elements.len()
        );

        for (expected, actual) in expected.iter().zip(actual_arr.elements.iter()) {
            test_expected_object(expected, actual);
        }
    }

    fn test_boolean_object(expected: &bool, actual: &Object) {
        let Object::Boolean(bool_obj) = actual else {
            panic!("object is not Integer. got: {}", actual);
        };

        assert_eq!(
            bool_obj.value, *expected,
            "object has wrong value. got: {}, want: {}",
            bool_obj.value, expected
        );
    }

    fn test_integer_object(expected: &i64, actual: &Object) {
        let Object::Integer(int_obj) = actual else {
            panic!("object is not Integer. got: {}", actual);
        };

        assert_eq!(
            int_obj.value, *expected,
            "object has wrong value. got: {}, want: {}",
            int_obj.value, expected
        );
    }

    fn test_string_object(expected: &str, actual: &Object) {
        let Object::String(str_obj) = actual else {
            panic!("object is not String. got: {}", actual);
        };

        assert_eq!(
            str_obj.value, *expected,
            "object has wrong value. got: {}, want: {}",
            str_obj.value, expected
        );
    }

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        return parser.parse_program();
    }
}
