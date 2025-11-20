use std::ops::Sub;

use monke_core::object::{Boolean, Integer, Null, Object};

use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
};

const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // stack pointer - always points to the next value.
}

#[derive(Debug)]
pub enum VmError {
    StackOverflowError(StackOverflowError),
    InvalidType,
    UnknownOperator,
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
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Object::Null(Box::new(Null {})); STACK_SIZE as usize],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let mut i = 0;

        while i < self.instructions.len() {
            let instruction = self.instructions[i];
            let opcode = Opcode::from_byte(instruction);

            let Some(opcode) = opcode else {
                continue;
            };

            match opcode {
                Opcode::OpConstant => {
                    // first byte is opcode, read 2 bytes from i+1..i+3
                    let const_index = u16::from_be_bytes(
                        self.instructions[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );
                    i += 2;

                    let constant = self.constants[const_index as usize].clone();

                    self.push(constant)?
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    let right = self.pop();

                    let Object::Integer(right_int) = right else {
                        return Err(VmError::InvalidType);
                    };

                    let left = self.pop();

                    let Object::Integer(left_int) = left else {
                        return Err(VmError::InvalidType);
                    };

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
                        self.instructions[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );

                    i += 2;

                    let condition = self.pop();

                    if !self.is_truthy(&condition) {
                        i = pos.sub(1) as usize
                    }
                }
                Opcode::OpJump => {
                    let pos = u16::from_be_bytes(
                        self.instructions[(i as usize + 1)..(i as usize + 3)]
                            .try_into()
                            .expect("failed to convert instruction to u16"),
                    );
                    i = pos.sub(1) as usize;
                }
                Opcode::OpNull => self.push(Object::Null(Box::new(Null {})))?,
                Opcode::OpGetGlobal => todo!(),
                Opcode::OpSetGlobal => todo!(),
            }

            i += 1;
        }

        Ok(())
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
}

#[cfg(test)]
mod tests {
    use monke_core::{
        ast,
        lexer::Lexer,
        object::{Null, Object},
        parser::Parser,
    };

    use crate::{compiler::Compiler, vm::Vm};

    enum Expected {
        Integer(i64),
        Boolean(bool),
        Null(Null),
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

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        return parser.parse_program();
    }
}
