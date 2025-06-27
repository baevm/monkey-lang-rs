use crate::{
    ast::{Expression, Program, Statement},
    object::{Boolean, Integer, Null, Object},
};

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {}
    }

    pub fn eval(&self, program: &Program) -> Object {
        let mut result = Object::Null(Box::new(Null {}));

        for stmt in &program.body {
            result = self.eval_statement(stmt);
        }

        result
    }

    fn eval_statement(&self, stmt: &Statement) -> Object {
        match stmt {
            Statement::LetStatement(let_statement) => Object::Null(Box::new(Null {})),
            Statement::ReturnStatement(return_statement) => Object::Null(Box::new(Null {})),
            Statement::ExpressionStatement(expression_statement) => {
                if let Some(expr) = &expression_statement.expression {
                    self.eval_expression(expr)
                } else {
                    Object::Null(Box::new(Null {}))
                }
            }
        }
    }

    fn eval_expression(&self, expr: &Expression) -> Object {
        match expr {
            Expression::IntegerLiteral(integer_literal) => Object::Integer(Box::new(Integer {
                value: integer_literal.value,
            })),
            Expression::Boolean(boolean_literal) => Object::Boolean(Box::new(Boolean {
                value: boolean_literal.value,
            })),
            _ => Object::Null(Box::new(Null {})),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, object::Object, parser::Parser};

    struct TestCase<T> {
        input: String,
        expected: T,
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<TestCase<i64>> = vec![
            TestCase {
                input: "5".to_string(),
                expected: 5,
            },
            TestCase {
                input: "100500".to_string(),
                expected: 100500,
            },
        ];

        for test in tests {
            let evaled = test_eval(test.input);
            test_integer_object(evaled, test.expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<TestCase<bool>> = vec![
            TestCase {
                input: "true".to_string(),
                expected: true,
            },
            TestCase {
                input: "false".to_string(),
                expected: false,
            },
        ];

        for test in tests {
            let evaled = test_eval(test.input);
            test_boolean_object(evaled, test.expected);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let evaluator = Evaluator::new();

        evaluator.eval(&program)
    }

    fn test_integer_object(obj: Object, expected: i64) {
        let Object::Integer(int_obj) = obj else {
            panic!("object is not Integer. got: {:?}", obj)
        };

        assert_eq!(
            int_obj.value, expected,
            "Integer Object has wrong value. got:{}, want: {}",
            int_obj.value, expected
        );
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        let Object::Boolean(bool_obj) = obj else {
            panic!("object is not Boolean. got: {:?}", obj)
        };

        assert_eq!(
            bool_obj.value, expected,
            "Boolean Object has wrong value. got:{}, want: {}",
            bool_obj.value, expected
        );
    }
}
