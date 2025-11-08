use std::ops::{Deref, DerefMut};

#[derive(Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn from(value: Vec<u8>) -> Self {
        Self(value)
    }

    fn fmt_instruction(&self, def: &Definition, operands: Vec<i64>) -> String {
        let operand_count = def.operand_widths.len();

        if operands.len() != operand_count {
            return format!(
                "ERROR: operand length: {} does not match defined {}",
                operands.len(),
                operand_count
            );
        }

        match operand_count {
            0 => def.name.clone(),
            1 => format!("{} {}", def.name, operands[0]),
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
            i += 1 + read_bytes as usize;
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
#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    OpConstant = 0,
    OpPop = 1,
    OpAdd = 2,
    OpSub = 3,
    OpMul = 4,
    OpDiv = 5,
    OpTrue = 6,
    OpFalse = 7,
}

impl Opcode {
    pub fn get_definition(&self) -> Definition {
        match self {
            Opcode::OpConstant => Definition {
                name: "OpConstant".to_string(),
                operand_widths: vec![2],
            },
            Opcode::OpPop => Definition {
                name: "OpPop".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpSub => Definition {
                name: "OpSub".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpMul => Definition {
                name: "OpMul".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpDiv => Definition {
                name: "OpDiv".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpTrue => Definition {
                name: "OpTrue".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpFalse => Definition {
                name: "OpFalse".to_string(),
                operand_widths: vec![],
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
            _ => None,
        }
    }
}

pub struct Definition {
    pub name: String,
    /// Number of bytes each operand takes
    pub operand_widths: Vec<i64>,
}

pub fn make(opcode: Opcode, operands: &Vec<i64>) -> Vec<u8> {
    let definition = opcode.get_definition();

    let instruction_len: usize = definition
        .operand_widths
        .iter()
        .fold(1, |acc, val| acc + *val as usize);

    let mut instruction: Vec<u8> = vec![0; instruction_len];
    instruction[0] = opcode as u8;

    let mut offset = 1;

    for (idx, operand) in operands.iter().enumerate() {
        let width = definition.operand_widths[idx] as usize;

        match width {
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

pub fn read_operands(def: &Definition, instruction: &[u8]) -> (Vec<i64>, i64) {
    let mut operands: Vec<i64> = vec![0; def.operand_widths.len()];
    let mut offset: i64 = 0;

    for (idx, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                let u16num = u16::from_be_bytes(
                    instruction[(offset as usize)..(offset as usize + 2)]
                        .try_into()
                        .unwrap(),
                );
                operands[idx] = u16num.try_into().unwrap()
            }
            _ => {}
        }

        offset += width;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use crate::code::{Instructions, Opcode, make};

    #[test]
    fn make_test() {
        let tests: Vec<(Opcode, Vec<i64>, Vec<u8>)> = vec![
            (
                Opcode::OpConstant,
                vec![65534],
                vec![Opcode::OpConstant as u8, 255, 254],
            ),
            (Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
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
            make(Opcode::OpAdd, &vec![]),
            make(Opcode::OpConstant, &vec![2]),
            make(Opcode::OpConstant, &vec![65535]),
        ];

        let expected = "0000 OpAdd \n0001 OpConstant 2 \n0004 OpConstant 65535 \n";

        let concated: Instructions = instructions.into_iter().flat_map(|i| i).collect();

        assert_eq!(expected, concated.to_string());
    }
}
