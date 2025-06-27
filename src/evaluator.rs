use crate::{
    ast::{Expression, Program, Statement},
    object::{Integer, Null, Object},
};

pub fn eval(program: &Program) -> Object {
    let mut result = Object::Null(Box::new(Null {}));

    for stmt in &program.body {
        result = eval_statement(stmt);
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::LetStatement(let_statement) => Object::Null(Box::new(Null {})),
        Statement::ReturnStatement(return_statement) => Object::Null(Box::new(Null {})),
        Statement::ExpressionStatement(expression_statement) => {
            if let Some(expr) = &expression_statement.expression {
                eval_expression(expr)
            } else {
                Object::Null(Box::new(Null {}))
            }
        }
    }
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::IntegerLiteral(integer_literal) => Object::Integer(Box::new(Integer {
            value: integer_literal.value,
        })),
        _ => Object::Null(Box::new(Null {})),
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::eval, lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_integer_expression() {
        struct TestCase {
            input: String,
            expected: i64,
        }

        let tests: Vec<TestCase> = vec![
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

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eval(&program)
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
}
