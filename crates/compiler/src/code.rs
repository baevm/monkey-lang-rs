use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn from(value: Vec<u8>) -> Self {
        Self(value)
    }

    fn fmt_instruction(&self, def: &Definition, operands: Vec<usize>) -> String {
        let operand_count = def.operand_widths.len();

        if operands.len() != operand_count {
            return format!(
                "ERROR: operand length: {} does not match defined {}",
                operands.len(),
                operand_count
            );
        }

        match operand_count {
            0 => def.name.to_string(),
            1 => format!("{} {}", def.name, operands[0]),
            2 => format!("{} {} {}", def.name, operands[0], operands[1]),
            _ => format!("ERROR: unhandled operandCount for {}", def.name),
        }
    }
}

impl std::fmt::Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        let mut i = 0;

        while i < self.0.len() {
            let opcode = Opcode::from_byte(self.0[i]);

            let Some(opcode) = opcode else {
                sb.push_str("ERROR: opcode not found");
                continue;
            };

            let def = opcode.get_definition();

            let (operands, read_bytes) = read_operands(&def, &self.0[i + 1..]);

            sb.push_str(&format!(
                "{:04} {} \n",
                i,
                self.fmt_instruction(&def, operands)
            ));
            i += 1 + read_bytes;
        }

        write!(f, "{}", sb)
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = <Vec<u8> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<u8> for Instructions {
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    OpConstant = 0,
    OpPop = 1,
    OpAdd = 2,
    OpSub = 3,
    OpMul = 4,
    OpDiv = 5,
    OpTrue = 6,
    OpFalse = 7,
    OpEqual = 8,
    OpNotEqual = 9,
    OpGreaterThan = 10,
    OpMinus = 11,
    OpBang = 12,
    OpJumpNotTruthy = 13, // jump if value on top of stack is not truthy
    OpJump = 14,          // jump at offset
    OpNull = 15,
    OpGetGlobal = 16,
    OpSetGlobal = 17,
    OpArray = 18,
    OpHash = 19,
    OpIndex = 20,
    OpCall = 21,
    OpReturnValue = 22, // function return with value
    OpReturn = 23,      // function return with no value (null)
    OpGetLocal = 24,
    OpSetLocal = 25,
    OpGetBuiltin = 26,
    OpClosure = 27,
}

impl Opcode {
    pub fn get_definition(&self) -> Definition {
        match self {
            Opcode::OpConstant => Definition {
                name: "OpConstant",
                operand_widths: &[2],
            },
            Opcode::OpPop => Definition {
                name: "OpPop",
                operand_widths: &[],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd",
                operand_widths: &[],
            },
            Opcode::OpSub => Definition {
                name: "OpSub",
                operand_widths: &[],
            },
            Opcode::OpMul => Definition {
                name: "OpMul",
                operand_widths: &[],
            },
            Opcode::OpDiv => Definition {
                name: "OpDiv",
                operand_widths: &[],
            },
            Opcode::OpTrue => Definition {
                name: "OpTrue",
                operand_widths: &[],
            },
            Opcode::OpFalse => Definition {
                name: "OpFalse",
                operand_widths: &[],
            },
            Opcode::OpEqual => Definition {
                name: "OpEqual",
                operand_widths: &[],
            },
            Opcode::OpNotEqual => Definition {
                name: "OpNotEqual",
                operand_widths: &[],
            },
            Opcode::OpGreaterThan => Definition {
                name: "OpGreaterThan",
                operand_widths: &[],
            },
            Opcode::OpMinus => Definition {
                name: "OpMinus",
                operand_widths: &[],
            },
            Opcode::OpBang => Definition {
                name: "OpBang",
                operand_widths: &[],
            },
            Opcode::OpJumpNotTruthy => Definition {
                name: "OpJumpNotTruthy",
                operand_widths: &[2],
            },
            Opcode::OpJump => Definition {
                name: "OpJump",
                operand_widths: &[2],
            },
            Opcode::OpNull => Definition {
                name: "OpNull",
                operand_widths: &[],
            },
            Opcode::OpSetGlobal => Definition {
                name: "OpSetGlobal",
                operand_widths: &[2],
            },
            Opcode::OpGetGlobal => Definition {
                name: "OpGetGlobal",
                operand_widths: &[2],
            },
            Opcode::OpArray => Definition {
                name: "OpArray",
                operand_widths: &[2],
            },
            Opcode::OpHash => Definition {
                name: "OpHash",
                operand_widths: &[2],
            },
            Opcode::OpIndex => Definition {
                name: "OpIndex",
                operand_widths: &[],
            },
            Opcode::OpCall => Definition {
                name: "OpCall",
                operand_widths: &[1],
            },
            Opcode::OpReturnValue => Definition {
                name: "OpReturnValue",
                operand_widths: &[],
            },
            Opcode::OpReturn => Definition {
                name: "OpReturn",
                operand_widths: &[],
            },
            Opcode::OpGetLocal => Definition {
                name: "OpGetLocal",
                operand_widths: &[1],
            },
            Opcode::OpSetLocal => Definition {
                name: "OpSetLocal",
                operand_widths: &[1],
            },
            Opcode::OpGetBuiltin => Definition {
                name: "OpGetBuiltin",
                operand_widths: &[1],
            },
            Opcode::OpClosure => Definition {
                name: "OpClosure",
                // first operand - constant index
                // second operand - "free variables" on stack
                operand_widths: &[2, 1],
            },
        }
    }

    pub fn from_byte(value: u8) -> Option<Opcode> {
        match value {
            0 => Some(Opcode::OpConstant),
            1 => Some(Opcode::OpPop),
            2 => Some(Opcode::OpAdd),
            3 => Some(Opcode::OpSub),
            4 => Some(Opcode::OpMul),
            5 => Some(Opcode::OpDiv),
            6 => Some(Opcode::OpTrue),
            7 => Some(Opcode::OpFalse),
            8 => Some(Opcode::OpEqual),
            9 => Some(Opcode::OpNotEqual),
            10 => Some(Opcode::OpGreaterThan),
            11 => Some(Opcode::OpMinus),
            12 => Some(Opcode::OpBang),
            13 => Some(Opcode::OpJumpNotTruthy),
            14 => Some(Opcode::OpJump),
            15 => Some(Opcode::OpNull),
            16 => Some(Opcode::OpGetGlobal),
            17 => Some(Opcode::OpSetGlobal),
            18 => Some(Opcode::OpArray),
            19 => Some(Opcode::OpHash),
            20 => Some(Opcode::OpIndex),
            21 => Some(Opcode::OpCall),
            22 => Some(Opcode::OpReturnValue),
            23 => Some(Opcode::OpReturn),
            24 => Some(Opcode::OpGetLocal),
            25 => Some(Opcode::OpSetLocal),
            26 => Some(Opcode::OpGetBuiltin),
            27 => Some(Opcode::OpClosure),
            _ => None,
        }
    }
}

pub struct Definition {
    pub name: &'static str,
    /// Number of bytes each operand takes
    pub operand_widths: &'static [usize],
}

pub fn make(opcode: Opcode, operands: &[usize]) -> Vec<u8> {
    let definition = opcode.get_definition();

    let instruction_len: usize = definition
        .operand_widths
        .iter()
        .fold(1, |acc, val| acc + *val);

    let mut instruction: Vec<u8> = vec![0; instruction_len];
    instruction[0] = opcode as u8;

    let mut offset = 1;

    for (idx, operand) in operands.iter().enumerate() {
        let width = definition.operand_widths[idx];

        match width {
            1 => {
                let bytes = operand.to_be_bytes();
                instruction[offset] = *bytes.last().unwrap();
            }
            2 => {
                let bytes = (*operand as u16).to_be_bytes();
                instruction[offset..offset + 2].copy_from_slice(&bytes);
            }
            _ => {}
        }

        offset += width;
    }

    instruction
}

pub fn read_operands(def: &Definition, instruction: &[u8]) -> (Vec<usize>, usize) {
    let mut operands: Vec<usize> = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (idx, byte_width) in def.operand_widths.iter().enumerate() {
        match byte_width {
            1 => {
                let u8num = u8::from_be_bytes([instruction[offset]]);
                operands[idx] = u8num.try_into().unwrap()
            }
            2 => {
                let u16num =
                    u16::from_be_bytes(instruction[(offset)..(offset + 2)].try_into().unwrap());
                operands[idx] = u16num.try_into().unwrap()
            }
            _ => {}
        }

        offset += byte_width;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use crate::code::{Instructions, Opcode, make, read_operands};

    #[test]
    fn make_test() {
        let tests: Vec<(Opcode, Vec<usize>, Vec<u8>)> = vec![
            (
                Opcode::OpConstant,
                vec![65534],
                vec![Opcode::OpConstant as u8, 255, 254],
            ),
            (Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
            (
                Opcode::OpGetLocal,
                vec![255],
                vec![Opcode::OpGetLocal as u8, 255],
            ),
            (
                Opcode::OpClosure,
                vec![65534, 255],
                vec![Opcode::OpClosure as u8, 255, 254, 255],
            ),
        ];

        for test in tests {
            let instruction = make(test.0, &test.1);

            assert_eq!(
                instruction.len(),
                test.2.len(),
                "instruction has wrong length. want: {}, got: {}",
                test.2.len(),
                instruction.len()
            );

            for (idx, byte) in test.2.into_iter().enumerate() {
                assert_eq!(
                    byte, instruction[idx],
                    "wrong byte at pos {}. want: {}, got: {}",
                    idx, byte, instruction[idx]
                );
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::OpAdd, &[]),
            make(Opcode::OpGetLocal, &[1]),
            make(Opcode::OpConstant, &[2]),
            make(Opcode::OpConstant, &[65535]),
            make(Opcode::OpClosure, &[65535, 255]),
        ];

        let expected = "0000 OpAdd \n0001 OpGetLocal 1 \n0003 OpConstant 2 \n0006 OpConstant 65535 \n0009 OpClosure 65535 255 \n";

        let concated: Instructions = instructions.into_iter().flat_map(|i| i).collect();

        assert_eq!(expected, concated.to_string());
    }

    #[test]
    fn test_read_operands() {
        struct TestCase {
            op: Opcode,
            operands: Vec<usize>,
            bytes_read: usize,
        }

        let tests: Vec<TestCase> = vec![
            TestCase {
                op: Opcode::OpConstant,
                operands: vec![65535],
                bytes_read: 2,
            },
            TestCase {
                op: Opcode::OpGetLocal,
                operands: vec![255],
                bytes_read: 1,
            },
            TestCase {
                op: Opcode::OpClosure,
                operands: vec![65535, 255],
                bytes_read: 3,
            },
        ];

        for test in tests {
            let instruction = make(test.op, &test.operands);

            let def = test.op.get_definition();

            let (operands_read, n) = read_operands(&def, &instruction[1..]);

            assert_eq!(
                n, test.bytes_read,
                "n wrong. want: {}, got: {}",
                test.bytes_read, n
            );

            for (i, want) in test.operands.iter().enumerate() {
                assert_eq!(
                    operands_read[i], *want,
                    "operand wrong. want: {}, got: {}",
                    want, operands_read[i]
                )
            }
        }
    }
}
