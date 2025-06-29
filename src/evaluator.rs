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
            Expression::PrefixExpression(prefix_expr) => {
                let right = self.eval_expression(&prefix_expr.right);
                self.eval_prefix_expression(&prefix_expr.operator, right)
            }
            Expression::InfixExpression(infix_expr) => {
                let left = self.eval_expression(&infix_expr.left);
                let right = self.eval_expression(&infix_expr.right);
                self.eval_infix_expression(&infix_expr.operator, left, right)
            }
            _ => Object::Null(Box::new(Null {})),
        }
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Object::Null(Box::new(Null {})),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => {
                if boolean.value {
                    Object::Boolean(Box::new(Boolean { value: false }))
                } else {
                    Object::Boolean(Box::new(Boolean { value: true }))
                }
            }
            Object::Null(_) => Object::Boolean(Box::new(Boolean { value: true })),
            _ => Object::Boolean(Box::new(Boolean { value: false })),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        let Object::Integer(int_expr) = right else {
            return Object::Null(Box::new(Null {}));
        };

        Object::Integer(Box::new(Integer {
            value: -int_expr.value,
        }))
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(left_expr), Object::Integer(right_expr)) => {
                return self.eval_integer_infix_expression(&operator, left_expr, right_expr);
            }
            (Object::Boolean(left_expr), Object::Boolean(right_expr)) => {
                return self.eval_boolean_infix_expression(&operator, left_expr, right_expr);
            }
            (_, _) => {}
        }

        Object::Null(Box::new(Null {}))
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &str,
        left: Box<Integer>,
        right: Box<Integer>,
    ) -> Object {
        match operator {
            "+" => Object::Integer(Box::new(Integer {
                value: left.value + right.value,
            })),
            "-" => Object::Integer(Box::new(Integer {
                value: left.value - right.value,
            })),
            "*" => Object::Integer(Box::new(Integer {
                value: left.value * right.value,
            })),
            "/" => Object::Integer(Box::new(Integer {
                value: left.value / right.value,
            })),
            ">" => Object::Boolean(Box::new(Boolean {
                value: left.value > right.value,
            })),
            "<" => Object::Boolean(Box::new(Boolean {
                value: left.value < right.value,
            })),
            "==" => Object::Boolean(Box::new(Boolean {
                value: left.value == right.value,
            })),
            "!=" => Object::Boolean(Box::new(Boolean {
                value: left.value != right.value,
            })),
            _ => Object::Null(Box::new(Null {})),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &str,
        left: Box<Boolean>,
        right: Box<Boolean>,
    ) -> Object {
        match operator {
            "==" => Object::Boolean(Box::new(Boolean {
                value: left.value == right.value,
            })),
            "!=" => Object::Boolean(Box::new(Boolean {
                value: left.value != right.value,
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
            TestCase {
                input: "-5".to_string(),
                expected: -5,
            },
            TestCase {
                input: "-100500".to_string(),
                expected: -100500,
            },
            TestCase {
                input: "5 + 5 + 5".to_string(),
                expected: 15,
            },
            TestCase {
                input: "20 + 5 / 5".to_string(),
                expected: 21,
            },
            TestCase {
                input: "1 + (5 * 10)".to_string(),
                expected: 51,
            },
            TestCase {
                input: "5 * 5 * 5".to_string(),
                expected: 125,
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
            TestCase {
                input: "1 < 2".to_string(),
                expected: true,
            },
            TestCase {
                input: "1 > 2".to_string(),
                expected: false,
            },
            TestCase {
                input: "1 == 2".to_string(),
                expected: false,
            },
            TestCase {
                input: "1 != 2".to_string(),
                expected: true,
            },
            TestCase {
                input: "1 == 1".to_string(),
                expected: true,
            },
            TestCase {
                input: "true == true".to_string(),
                expected: true,
            },
            TestCase {
                input: "true != false".to_string(),
                expected: true,
            },
            TestCase {
                input: "true == false".to_string(),
                expected: false,
            },
            TestCase {
                input: "false == false".to_string(),
                expected: true,
            },
            TestCase {
                input: "(1 > 2) == true".to_string(),
                expected: false,
            },
            TestCase {
                input: "(1 < 2) == true".to_string(),
                expected: true,
            },
            TestCase {
                input: "(1 > 2) == false".to_string(),
                expected: true,
            },
            TestCase {
                input: "(1 < 2) == false".to_string(),
                expected: false,
            },
        ];

        for test in tests {
            let evaled = test_eval(test.input);
            test_boolean_object(evaled, test.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: Vec<TestCase<bool>> = vec![
            TestCase {
                input: "!true".to_string(),
                expected: false,
            },
            TestCase {
                input: "!false".to_string(),
                expected: true,
            },
            TestCase {
                input: "!5".to_string(),
                expected: false,
            },
            TestCase {
                input: "!!true".to_string(),
                expected: true,
            },
            TestCase {
                input: "!!false".to_string(),
                expected: false,
            },
            TestCase {
                input: "!!5".to_string(),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_boolean_object(evaluated, test.expected);
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
