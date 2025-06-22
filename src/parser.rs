use crate::{
    ast::{
        self, Boolean, Expression, ExpressionStatement, Identifier, InfixExpression,
        IntegerLiteral, LetStatement, PrefixExpression, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // < >
    Sum,         // +
    Product,     // *
    Prefix,      // --variable
    Call,        // function()
}

impl Precedence {
    pub fn token_to_precedence(token: &TokenType) -> Option<Self> {
        match token {
            TokenType::Eq => Some(Precedence::Equals),
            TokenType::NotEq => Some(Precedence::Equals),
            TokenType::Lt => Some(Precedence::LessGreater),
            TokenType::Gt => Some(Precedence::LessGreater),
            TokenType::Plus => Some(Precedence::Sum),
            TokenType::Minus => Some(Precedence::Sum),
            TokenType::Slash => Some(Precedence::Product),
            TokenType::Asterisk => Some(Precedence::Product),
            _ => None,
        }
    }
}

pub struct Parser {
    lexer: Lexer,

    curr_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::new(),
            peek_token: Token::new(),
            errors: vec![],
        };

        // Set current token and next token to their values
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { body: vec![] };

        loop {
            if self.curr_token.token_type == TokenType::Eof {
                break;
            }

            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                program.body.push(*stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<Statement>> {
        match self.curr_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<Statement>> {
        let let_token = self.curr_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        while !self.is_curr_token(TokenType::Semicolon) {
            self.next_token();
        }

        let let_stmt = Statement::LetStatement(Box::new(LetStatement {
            token: let_token,
            name,
            value: None,
        }));

        Some(Box::new(let_stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Box<Statement>> {
        let return_stmt = Statement::ReturnStatement(Box::new(ReturnStatement {
            token: self.curr_token.clone(),
            return_value: None,
        }));

        self.next_token();

        while !self.is_curr_token(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(return_stmt))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<Statement>> {
        let expr_stmt = Statement::ExpressionStatement(Box::new(ExpressionStatement {
            token: self.curr_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        }));

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(expr_stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.parse_prefix(self.curr_token.token_type.clone());

        if prefix.is_none() {
            self.no_prefix_parse_fn_err(self.curr_token.token_type.clone());
            return None;
        }

        let mut left_expr = prefix.unwrap();

        while !self.is_peek_token(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            match Precedence::token_to_precedence(&self.peek_token.token_type) {
                Some(_prec) => {
                    self.next_token();
                    left_expr = self.parse_infix_expression(left_expr)?;
                }
                None => return Some(left_expr),
            }
        }

        Some(left_expr)
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let value = self.curr_token.literal.parse::<i64>();

        if value.is_err() {
            self.errors.push(format!(
                "could not parse {} as integer",
                self.curr_token.literal
            ));
            return None;
        }

        let int_literal = Expression::IntegerLiteral(Box::new(IntegerLiteral {
            token: self.curr_token.clone(),
            value: value.unwrap(),
        }));

        Some(int_literal)
    }

    fn parse_prefix(&mut self, token: TokenType) -> Option<Expression> {
        match token {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang => self.parse_prefix_expression(),
            TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True => self.parse_boolean(),
            TokenType::False => self.parse_boolean(),
            _ => None,
        }
    }

    fn parse_infix(&mut self, token: TokenType, left: &Expression) -> Option<Expression> {
        match token {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Asterisk
            | TokenType::Eq
            | TokenType::NotEq
            | TokenType::Lt
            | TokenType::Gt => self.parse_infix_expression(left.clone()),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier(Box::new(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        })))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.literal.clone();

        self.next_token();

        let prefix_expr = Expression::PrefixExpression(Box::new(PrefixExpression {
            token,
            operator,
            right: self.parse_expression(Precedence::Prefix)?,
        }));

        Some(prefix_expr)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.literal.clone();
        let precedence = self.curr_precedence();

        self.next_token();

        let infix_expr = Expression::InfixExpression(Box::new(InfixExpression {
            token,
            left,
            operator,
            right: self.parse_expression(precedence)?,
        }));

        Some(infix_expr)
    }

    fn parse_boolean(&self) -> Option<Expression> {
        Some(Expression::Boolean(Box::new(Boolean {
            token: self.curr_token.clone(),
            value: self.is_curr_token(TokenType::True),
        })))
    }

    fn expect_peek(&mut self, expected: TokenType) -> bool {
        if self.is_peek_token(&expected) {
            self.next_token();
            return true;
        } else {
            self.peek_error(expected);
            return false;
        }
    }

    fn is_curr_token(&self, expected: TokenType) -> bool {
        self.curr_token.token_type == expected
    }

    fn is_peek_token(&self, expected: &TokenType) -> bool {
        self.peek_token.token_type == *expected
    }

    fn peek_precedence(&self) -> Precedence {
        let precedence = Precedence::token_to_precedence(&self.peek_token.token_type);

        match precedence {
            Some(prec) => prec,
            None => Precedence::Lowest,
        }
    }

    fn curr_precedence(&self) -> Precedence {
        let precedence = Precedence::token_to_precedence(&self.curr_token.token_type);

        match precedence {
            Some(prec) => prec,
            None => Precedence::Lowest,
        }
    }

    fn peek_error(&mut self, expected: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, self.peek_token.token_type
        );

        self.errors.push(msg);
    }

    fn no_prefix_parse_fn_err(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "no prefix parse function found for: {:?}",
            token_type
        ));
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        ast::{Expression, Identifier, Program, Statement},
        lexer::Lexer,
        parser::Parser,
    };

    #[test]
    fn test_let_statements() {
        let input = r"
            let x = 5;
            let y = 10;
            let longNameVariable = 9999999;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "found errors while parsing: {:#?}",
            parser.errors
        );

        assert_eq!(program.body.len(), 3, "not enough statements in program");

        let tests = vec!["x", "y", "longNameVariable"];

        for (i, test) in tests.iter().enumerate() {
            let statement = &program.body[i];

            test_let_statement(statement, test);
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r"
            return 5;
            return fooBar;
            return 100500;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "found errors while parsing: {:#?}",
            parser.errors
        );

        assert_eq!(program.body.len(), 3, "not enough statements in program");

        for stmt in program.body {
            if let Statement::ReturnStatement(_return_stmt) = &stmt {
                assert_eq!(
                    stmt.token_literal(),
                    "return",
                    "return statement token literal is not 'return'. Got: {} ",
                    stmt.token_literal()
                );
            } else {
                panic!("stmt is not ReturnStatement")
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "variableName".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "found errors while parsing: {:#?}",
            parser.errors
        );

        assert_eq!(program.body.len(), 1, "not enough statements in program");

        if let Statement::ExpressionStatement(expr_stmt) = program.body.first().unwrap() {
            if let Expression::Identifier(identifier) = expr_stmt.expression.as_ref().unwrap() {
                assert_eq!(identifier.value, "variableName");
                assert_eq!(identifier.token.literal, "variableName");
            } else {
                panic!("expression statement is not Identifier");
            }
        } else {
            panic!("stmt is not ExpressionStatement");
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "found errors while parsing: {:#?}",
            parser.errors
        );

        assert_eq!(program.body.len(), 1, "not enough statements in program");

        if let Statement::ExpressionStatement(expr_stmt) = program.body.first().unwrap() {
            if let Expression::IntegerLiteral(int_literal) = expr_stmt.expression.as_ref().unwrap()
            {
                assert_eq!(int_literal.value, 5);
                assert_eq!(int_literal.token.literal, "5");
            } else {
                panic!("expression statement is not IntegerLiteral");
            }
        } else {
            panic!("stmt is not ExpressionStatement");
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestCase {
            input: String,
            operator: String,
            int_value: i64,
        }

        let tests: Vec<TestCase> = vec![
            TestCase {
                input: "!5".to_string(),
                operator: "!".to_string(),
                int_value: 5,
            },
            TestCase {
                input: "-15".to_string(),
                operator: "-".to_string(),
                int_value: 15,
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                parser.errors.len(),
                0,
                "found errors while parsing: {:#?}",
                parser.errors
            );

            assert_eq!(program.body.len(), 1, "not enough statements in program");

            if let Statement::ExpressionStatement(expr_stmt) = program.body.first().unwrap() {
                if let Expression::PrefixExpression(prefix_expr) =
                    expr_stmt.expression.as_ref().unwrap()
                {
                    assert_eq!(prefix_expr.operator, test.operator);

                    if let Expression::IntegerLiteral(int_literal) = &prefix_expr.right {
                        assert_eq!(int_literal.value, test.int_value);
                        assert_eq!(int_literal.token.literal, test.int_value.to_string());
                    } else {
                        panic!("prefix expression right is not IntegerLiteral")
                    }
                } else {
                    panic!("expression statement is not PrefixExpression");
                }
            } else {
                panic!("stmt is not ExpressionStatement");
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct TestCase {
            input: String,
            left_val: i64,
            operator: String,
            right_val: i64,
        }

        let tests: Vec<TestCase> = vec![
            TestCase {
                input: "5 + 5;".to_string(),
                left_val: 5,
                operator: "+".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 - 5;".to_string(),
                left_val: 5,
                operator: "-".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 * 5;".to_string(),
                left_val: 5,
                operator: "*".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 / 5;".to_string(),
                left_val: 5,
                operator: "/".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 > 5;".to_string(),
                left_val: 5,
                operator: ">".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 < 5;".to_string(),
                left_val: 5,
                operator: "<".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 == 5;".to_string(),
                left_val: 5,
                operator: "==".to_string(),
                right_val: 5,
            },
            TestCase {
                input: "5 != 5;".to_string(),
                left_val: 5,
                operator: "!=".to_string(),
                right_val: 5,
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);

            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(
                parser.errors.len(),
                0,
                "found errors while parsing: {:#?}",
                parser.errors
            );

            assert_eq!(program.body.len(), 1, "not enough statements in program");

            let stmt = program.body.first().unwrap();

            if let Statement::ExpressionStatement(expr_stmt) = stmt {
                if let Expression::InfixExpression(infix_expr) =
                    expr_stmt.expression.as_ref().unwrap()
                {
                    assert_eq!(
                        infix_expr.operator, test.operator,
                        "Exprected InfixExpression operator: {}. Got: {}",
                        test.operator, infix_expr.operator
                    );

                    if let Expression::IntegerLiteral(int_literal) = &infix_expr.right {
                        assert_eq!(
                            int_literal.value, test.right_val,
                            "Exprected InfixExpression right value: {}. Got: {}",
                            test.right_val, int_literal.value
                        );
                    } else {
                        panic!("infix expression right is not IntegerLiteral")
                    }

                    if let Expression::IntegerLiteral(int_literal) = &infix_expr.left {
                        assert_eq!(
                            int_literal.value, test.left_val,
                            "Exprected InfixExpression left value: {}. Got: {}",
                            test.left_val, int_literal.value
                        );
                    } else {
                        panic!("infix expression left is not IntegerLiteral")
                    }
                } else {
                    panic!("expression statement is not InfixExpression");
                }
            } else {
                panic!("stmt is not ExpressionStatement");
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0.to_string());

            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(
                parser.errors.len(),
                0,
                "found errors while parsing: {:#?}",
                parser.errors
            );

            assert_eq!(
                program.to_string(),
                test.1,
                "expected: {}. got: {}",
                test.1,
                program.to_string()
            );
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true", "true"), ("false", "false")];

        for test in tests {
            let lexer = Lexer::new(test.0.to_string());

            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(
                parser.errors.len(),
                0,
                "found errors while parsing: {:#?}",
                parser.errors
            );

            assert_eq!(
                program.to_string(),
                test.1,
                "expected: {}. got: {}",
                test.1,
                program.to_string()
            );
        }
    }

    fn test_let_statement(stmt: &Statement, expected: &str) {
        assert_eq!(stmt.token_literal(), "let");

        if let Statement::LetStatement(let_stmt) = stmt {
            assert_eq!(let_stmt.name.value, expected);
            assert_eq!(let_stmt.name.token.literal, expected);
        } else {
            panic!("stmt is not LetStatement");
        };
    }

    fn test_identifier(expr: Expression, value: String) {
        let identifier = match expr {
            Expression::Identifier(identifier) => identifier,
            other => panic!("expression not identifier. got: {:?}", other),
        };

        assert_eq!(
            identifier.value, value,
            "identifier value is not {}. got: {}",
            value, identifier.value
        );

        assert_eq!(
            identifier.token.literal, value,
            "identifier token literal is not {}. got: {}",
            value, identifier.token.literal
        );
    }
}
