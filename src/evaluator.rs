use crate::ast::{Identifier, IfExpression};
use crate::object::{Environment, Function, InternalError, Return};
use crate::{
    ast::{Expression, Program, Statement},
    object::{Boolean, Integer, Null, Object},
};
use std::cell::RefCell;
use std::mem::discriminant;
use std::rc::Rc;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {}
    }

    pub fn eval(&self, program: &Program, env: &mut Environment) -> Object {
        let mut result = Object::Null(Box::new(Null {}));

        for stmt in &program.body {
            result = self.eval_statement(&stmt, env);

            if let Object::Return(return_obj) = result {
                return return_obj.value;
            }

            if let Object::InternalError(_) = &result {
                return result;
            }
        }

        result
    }

    fn eval_statement(&self, stmt: &Statement, env: &mut Environment) -> Object {
        match stmt {
            Statement::LetStatement(let_stmt) => {
                if let_stmt.value.is_none() {
                    return Object::Null(Box::new(Null {}));
                }

                let evaluated = self.eval_expression(&let_stmt.value.as_ref().unwrap(), env);

                if self.is_err_obj(&evaluated) {
                    return evaluated;
                }

                env.set(&let_stmt.name.value, evaluated);

                Object::Null(Box::new(Null {}))
            }
            Statement::ReturnStatement(return_statement) => {
                if return_statement.return_value.is_none() {
                    return Object::Null(Box::new(Null {}));
                }

                let value =
                    self.eval_expression(&return_statement.return_value.as_ref().unwrap(), env);

                if self.is_err_obj(&value) {
                    return value;
                }

                Object::Return(Box::new(Return { value }))
            }
            Statement::ExpressionStatement(expression_statement) => {
                if let Some(expr) = &expression_statement.expression {
                    self.eval_expression(expr, env)
                } else {
                    Object::Null(Box::new(Null {}))
                }
            }
            Statement::BlockStatement(block_stmt) => {
                let mut result = Object::Null(Box::new(Null {}));

                for stmt in &block_stmt.statements {
                    result = self.eval_statement(&stmt, env);

                    if let Object::Return(_) = &result {
                        return result;
                    }

                    if let Object::InternalError(_) = &result {
                        return result;
                    }
                }

                result
            }
        }
    }

    fn eval_expression(&self, expr: &Expression, env: &mut Environment) -> Object {
        match expr {
            Expression::IntegerLiteral(integer_literal) => Object::Integer(Box::new(Integer {
                value: integer_literal.value,
            })),
            Expression::Boolean(boolean_literal) => Object::Boolean(Box::new(Boolean {
                value: boolean_literal.value,
            })),
            Expression::PrefixExpression(prefix_expr) => {
                let right = self.eval_expression(&prefix_expr.right, env);

                if self.is_err_obj(&right) {
                    return right;
                }

                self.eval_prefix_expression(&prefix_expr.operator, right)
            }
            Expression::InfixExpression(infix_expr) => {
                let left = self.eval_expression(&infix_expr.left, env);

                if self.is_err_obj(&left) {
                    return left;
                }

                let right = self.eval_expression(&infix_expr.right, env);

                if self.is_err_obj(&right) {
                    return right;
                }

                self.eval_infix_expression(&infix_expr.operator, left, right)
            }
            Expression::IfExpression(if_expr) => self.eval_if_expression(&if_expr, env),
            Expression::Identifier(ident) => self.eval_identifier(ident, env),
            Expression::FunctionLiteral(func) => Object::Function(Box::new(Function {
                parameters: func.parameters.clone(),
                body: func.body.clone(),
                env: env.clone(),
            })),
            Expression::CallExpression(call_expr) => {
                let func = self.eval_expression(&call_expr.function, env);

                if self.is_err_obj(&func) {
                    return func;
                }

                let args = self.eval_expressions(&call_expr.arguments, env);

                if args.len() == 1 && self.is_err_obj(&args[0]) {
                    return args[0].clone();
                }

                self.apply_function(func, args)
            }

            _ => Object::Null(Box::new(Null {})),
        }
    }

    fn eval_expressions(
        &self,
        expressions: &Vec<Expression>,
        env: &mut Environment,
    ) -> Vec<Object> {
        let mut result = vec![];

        for expr in expressions {
            let evaluated = self.eval_expression(expr, env);

            if self.is_err_obj(&evaluated) {
                return vec![evaluated];
            }

            result.push(evaluated);
        }

        result
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Object::InternalError(Box::new(InternalError {
                message: format!("unknown operator: {operator}{}", right),
            })),
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
            return Object::InternalError(Box::new(InternalError {
                message: format!("unknown operator: -{}", right),
            }));
        };

        Object::Integer(Box::new(Integer {
            value: -int_expr.value,
        }))
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        if discriminant(&left) != discriminant(&right) {
            return Object::InternalError(Box::new(InternalError {
                message: format!("type mismatch: {} {} {}", left, operator, right),
            }));
        }

        match (left, right) {
            (Object::Integer(left_expr), Object::Integer(right_expr)) => {
                return self.eval_integer_infix_expression(&operator, left_expr, right_expr);
            }
            (Object::Boolean(left_expr), Object::Boolean(right_expr)) => {
                return self.eval_boolean_infix_expression(&operator, left_expr, right_expr);
            }
            (left_expr, right_expr) => Object::InternalError(Box::new(InternalError {
                message: format!(
                    "unknown operator: {} {} {}",
                    left_expr, operator, right_expr
                ),
            })),
        };

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
            _ => Object::InternalError(Box::new(InternalError {
                message: format!("unknown operator: {} {} {}", left, operator, right),
            })),
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
            _ => Object::InternalError(Box::new(InternalError {
                message: format!("unknown operator: {} {} {}", left, operator, right),
            })),
        }
    }

    fn eval_if_expression(&self, if_expr: &Box<IfExpression>, env: &mut Environment) -> Object {
        let condition = self.eval_expression(&if_expr.condition, env);

        if self.is_err_obj(&condition) {
            return condition;
        }

        if self.is_truthy(&condition) {
            let consequence_as_stmt =
                Statement::BlockStatement(Box::new(if_expr.consequence.clone()));

            return self.eval_statement(&consequence_as_stmt, env);
        } else if if_expr.alternative.is_some() {
            let alternative_as_stmt =
                Statement::BlockStatement(Box::new(if_expr.alternative.as_ref().unwrap().clone()));

            return self.eval_statement(&alternative_as_stmt, env);
        }

        Object::Null(Box::new(Null {}))
    }

    fn eval_identifier(&self, ident: &Identifier, env: &mut Environment) -> Object {
        if let Some(ident_val) = env.get(&ident.value) {
            return ident_val.clone();
        }

        return Object::InternalError(Box::new(InternalError {
            message: format!("identifier not found: {}", ident.value),
        }));
    }

    fn is_truthy(&self, obj: &Object) -> bool {
        match obj {
            Object::Null(_) => false,
            Object::Boolean(bool_obj) => bool_obj.value,
            _ => true,
        }
    }

    fn is_err_obj(&self, obj: &Object) -> bool {
        if let Object::InternalError(err) = obj {
            return true;
        }

        false
    }

    fn apply_function(&self, func: Object, args: Vec<Object>) -> Object {
        let Object::Function(func_obj) = &func else {
            return Object::InternalError(Box::new(InternalError {
                message: format!("not a function: {}", func),
            }));
        };

        let mut extended_env = self.extend_function_env(func_obj, args);
        let evaluated = self.eval_statement(
            &Statement::BlockStatement(Box::new(func_obj.body.clone())),
            &mut extended_env,
        );

        if let Object::Return(return_val) = evaluated {
            return return_val.value;
        }

        evaluated
    }

    fn extend_function_env(&self, func_obj: &Box<Function>, args: Vec<Object>) -> Environment {
        let mut new_env = Environment::new_enclosed(Rc::new(RefCell::new(func_obj.env.clone())));

        for (idx, param) in func_obj.parameters.iter().enumerate() {
            new_env.set(&param.value, args[idx].clone());
        }

        new_env
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        ast::Stringer,
        evaluator::Evaluator,
        lexer::Lexer,
        object::{Environment, Object},
        parser::Parser,
    };

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

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            TestCase {
                input: "if (true) { 10 }".to_string(),
                expected: Some(10),
            },
            TestCase {
                input: "if (false) { 10 }".to_string(),
                expected: None,
            },
            TestCase {
                input: "if (1) { 10 }".to_string(),
                expected: Some(10),
            },
            TestCase {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: None,
            },
            TestCase {
                input: "if (1 > 2) { 10 } else { 5 }".to_string(),
                expected: Some(5),
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);

            if test.expected.is_some() {
                if let Object::Integer(int_obj) = &evaluated {
                    test_integer_object(evaluated, test.expected.unwrap());
                } else {
                    panic!("got null: {:?} expected: {:?}", evaluated, test.expected);
                }
            } else {
                test_null_object(evaluated)
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            TestCase {
                input: "return 10;".to_string(),
                expected: 10,
            },
            TestCase {
                input: "return 10; 100500".to_string(),
                expected: 10,
            },
            TestCase {
                input: "return 10 * 5;".to_string(),
                expected: 50,
            },
            TestCase {
                input: "9; return 10 * 5; 100500;".to_string(),
                expected: 50,
            },
            TestCase {
                input: r"
                    if(10 > 1) {
                        if(10 > 1) {
                            return 10;
                        }

                        return 1;
                    }
                "
                .to_string(),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_internal_error_handling() {
        let tests = vec![
            TestCase {
                input: "5 + true;".to_string(),
                expected: "type mismatch: Integer + Boolean",
            },
            TestCase {
                input: "5 + true; 5;".to_string(),
                expected: "type mismatch: Integer + Boolean",
            },
            TestCase {
                input: "-true".to_string(),
                expected: "unknown operator: -Boolean",
            },
            TestCase {
                input: "true + false".to_string(),
                expected: "unknown operator: Boolean + Boolean",
            },
            TestCase {
                input: "5; true + false; 10;".to_string(),
                expected: "unknown operator: Boolean + Boolean",
            },
            TestCase {
                input: "if (10 > 1) { true + false; }".to_string(),
                expected: "unknown operator: Boolean + Boolean",
            },
            TestCase {
                input: r"
                    if(10 > 1) {
                        if(10 > 1) {
                            return false + false;
                        }

                        return 1;
                    }
                "
                .to_string(),
                expected: "unknown operator: Boolean + Boolean",
            },
            TestCase {
                input: "randomVariable".to_string(),
                expected: "identifier not found: randomVariable",
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input.clone());

            let Object::InternalError(internal_err) = evaluated else {
                panic!(
                    "no error object returned. got: {:?}. test case: {:?}",
                    evaluated, test.input
                );
            };

            assert_eq!(
                test.expected, internal_err.message,
                "wrong error message. got: {:?}, need: {:?}",
                internal_err.message, test.expected
            );
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            TestCase {
                input: "let a = 5; a".to_string(),
                expected: 5,
            },
            TestCase {
                input: "let a = 5 * 5; a".to_string(),
                expected: 25,
            },
            TestCase {
                input: "let a = 5; let b = a; b;".to_string(),
                expected: 5,
            },
            TestCase {
                input: "let a = 5; let b = a; let c = a + b + 5; c".to_string(),
                expected: 15,
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "function(x) { x + 2; };".to_string();

        let evaluated = test_eval(input);

        let Object::Function(func) = evaluated else {
            panic!("object is not a function. got: {}", evaluated);
        };

        assert_eq!(func.parameters.len(), 1, "function has wrong parameters");
        assert_eq!(
            func.parameters[0].to_string(),
            "x",
            "function got wrong parameter"
        );
        assert_eq!(
            func.body.to_string(),
            "(x + 2)",
            "body is not (x + 2), got: {}",
            func.body.to_string()
        );
    }

    #[test]
    fn test_function_closure() {
        let input = r"
        let newAdder = function(x) {
            function(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);
        "
        .to_string();

        let evaluated = test_eval(input);

        test_integer_object(evaluated, 4);
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            TestCase {
                input: "let ident = function(x) {x;}; ident(5);".to_string(),
                expected: 5,
            },
            TestCase {
                input: "let ident = function(x) {return x;}; ident(5);".to_string(),
                expected: 5,
            },
            TestCase {
                input: "let ident = function(x) {x * 2;}; ident(5);".to_string(),
                expected: 10,
            },
            TestCase {
                input: "let ident = function(x, y) {x + y;}; ident(5, 5);".to_string(),
                expected: 10,
            },
            TestCase {
                input: "function(x) {x;}(5)".to_string(),
                expected: 5,
            },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated, test.expected);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let evaluator = Evaluator::new();
        let mut environment = Environment::new();

        evaluator.eval(&program, &mut environment)
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

    fn test_null_object(obj: Object) {
        let Object::Null(null_obj) = obj else {
            panic!("object is not NULL. got: {:?}", obj);
        };
    }
}
