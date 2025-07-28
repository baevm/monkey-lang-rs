pub type Instructions = Vec<i8>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Opcode {
    OpConstant,
}

struct Definition {
    pub name: String,
    pub operand_widths: Vec<u64>,
}

impl Definition {
    pub fn from_opcode(opcode: &Opcode) -> Definition {
        match opcode {
            Opcode::OpConstant => Definition {
                name: "OpConstant".to_string(),
                operand_widths: vec![2],
            },
        }
    }
}

fn make(op: Opcode, operands: &[u64]) -> Vec<u8> {
    let def = Definition::from_opcode(&op);

    let mut instruction_len: usize = 1;

    for operand in &def.operand_widths {
        instruction_len += *operand as usize;
    }

    let mut instruction: Vec<u8> = vec![0; instruction_len];
    instruction[0] = op as u8;

    let mut offset = 1;

    for (idx, operand) in operands.iter().enumerate() {
        let width = def.operand_widths[idx] as usize;

        match width {
            2 => {
                let bytes = (*operand as u16).to_be_bytes();
                instruction[offset..offset + width].copy_from_slice(&bytes);
            }
            _ => {}
        }

        offset += width;
    }

    instruction
}

#[cfg(test)]
mod tests {
    use crate::code::code::{Opcode, make};

    #[test]
    fn test_make() {
        struct TestCase {
            op: Opcode,
            operands: Vec<u64>,
            expected: Vec<u8>,
        }

        let tests = vec![TestCase {
            op: Opcode::OpConstant,
            operands: vec![65534],
            expected: vec![Opcode::OpConstant as u8, 255, 254],
        }];

        for test in tests {
            let instruction = make(test.op, &test.operands);
            println!("{:?}", instruction.len());

            assert_eq!(instruction.len(), test.expected.len());

            for (actual, expected) in instruction.iter().zip(test.expected.iter()) {
                assert_eq!(actual, expected);
            }
        }
    }
}
