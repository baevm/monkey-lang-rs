use crate::{
    ast::{
        self, ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression,
        ExpressionStatement, ForStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression,
        IndexExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        ReturnStatement, Statement, StringLiteral,
    },
    lexer::Lexer,
    parser_error::{InfoPosition, ParseError},
    precedence::Precedence,
    token::{Token, TokenType},
};

pub struct Parser {
    lexer: Lexer,

    curr_token: Token,
    peek_token: Token,

    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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

    pub fn errors(&self) -> Vec<ParseError> {
        self.errors.clone()
    }

    pub fn is_err(&self) -> bool {
        self.errors.len() > 0
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { body: vec![] };

        loop {
            if self.curr_token.token_type == TokenType::Eof {
                break;
            }

            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                program.body.push(stmt);
            }

            self.next_token();

            if self.is_err() {
                break;
            }
        }

        program
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::For => self.parse_for_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = Identifier {
            value: self.curr_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        let let_stmt = Statement::LetStatement(Box::new(LetStatement { name, value }));

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(let_stmt)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        let return_stmt = Statement::ReturnStatement(Box::new(ReturnStatement { return_value }));

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(return_stmt)
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }

        self.next_token();

        let init = self.parse_statement()?;

        // skip semicolon
        self.next_token();

        let test = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Semicolon) {
            return None;
        }
        self.next_token();

        let update = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let for_statement = Statement::ForStatement(Box::new(ForStatement {
            init,
            test,
            update,
            body,
        }));

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(for_statement)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        let expr_stmt = Statement::ExpressionStatement(Box::new(ExpressionStatement {
            expression: Some(expression),
        }));

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(expr_stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.parse_prefix(self.curr_token.token_type.clone());

        if prefix.is_none() {
            if self.errors.last().map_or(true, |e| match e {
                ParseError::UnexpectedToken { info, .. } => {
                    info.line != self.lexer.line || info.column != self.lexer.col
                }
                _ => true,
            }) {
                self.peek_error(None);
            }
            return None;
        }

        let mut left_expr = prefix.unwrap();

        while !self.is_peek_token(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            let last_token = self.peek_token.token_type.clone();

            match Precedence::token_to_precedence(&self.peek_token.token_type) {
                Some(_prec) => {
                    self.next_token();
                    left_expr = self.parse_infix(last_token, &left_expr)?;
                }
                None => return Some(left_expr),
            }
        }

        Some(left_expr)
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let value = self.curr_token.literal.parse::<i64>();

        if value.is_err() {
            self.errors.push(ParseError::CouldNotParseInteger(
                self.curr_token.literal.to_string(),
            ));
            return None;
        }

        let int_literal = Expression::IntegerLiteral(Box::new(IntegerLiteral {
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
            TokenType::Lparen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            TokenType::String => self.parse_string_literal(),
            TokenType::Lbracket => self.parse_array_literal(),
            TokenType::Lbrace => self.parse_hash_literal(),
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
            | TokenType::Gt
            | TokenType::Assign
            | TokenType::AssignAdd
            | TokenType::AssignSub
            | TokenType::AssignDiv
            | TokenType::AssignMul => self.parse_infix_expression(left.clone()),
            TokenType::Lparen => self.parse_call_expression(left.clone()),
            TokenType::Lbracket => self.parse_index_expression(left.clone()),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier(Box::new(Identifier {
            value: self.curr_token.literal.clone(),
        })))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = match self.curr_token.literal.as_ref() {
            "-" => TokenType::Minus,
            "!" => TokenType::Bang,
            _ => return None,
        };

        self.next_token();

        let prefix_expr = Expression::PrefixExpression(Box::new(PrefixExpression {
            operator,
            right: self.parse_expression(Precedence::Prefix)?,
        }));

        Some(prefix_expr)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = match self.curr_token.literal.as_ref() {
            "+=" => TokenType::AssignAdd,
            "-=" => TokenType::AssignSub,
            "*=" => TokenType::AssignMul,
            "/=" => TokenType::AssignDiv,

            "=" => TokenType::Assign,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Asterisk,
            "/" => TokenType::Slash,

            "==" => TokenType::Eq,
            "!=" => TokenType::NotEq,
            ">" => TokenType::Gt,
            "<" => TokenType::Lt,
            _ => return None,
        };
        let precedence = self.curr_precedence();

        self.next_token();

        let infix_expr = Expression::InfixExpression(Box::new(InfixExpression {
            left,
            operator,
            right: self.parse_expression(precedence)?,
        }));

        Some(infix_expr)
    }

    fn parse_boolean(&self) -> Option<Expression> {
        Some(Expression::Boolean(Box::new(Boolean {
            value: self.is_curr_token(TokenType::True),
        })))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        expr
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative: Option<BlockStatement> = if self.is_peek_token(&TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::Lbrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        let if_expr = Expression::IfExpression(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        }));

        Some(if_expr)
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let func_literal =
            Expression::FunctionLiteral(Box::new(FunctionLiteral { parameters, body }));

        Some(func_literal)
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.is_peek_token(&TokenType::Rparen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let ident = Identifier {
            value: self.curr_token.literal.clone(),
        };
        identifiers.push(ident);

        while self.is_peek_token(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Identifier {
                value: self.curr_token.literal.clone(),
            };

            identifiers.push(ident);
        }

        self.next_token();

        identifiers
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = self.parse_expression_list(TokenType::Rparen);

        let call_expr = Expression::CallExpression(Box::new(CallExpression {
            function,
            arguments,
        }));

        Some(call_expr)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block_statement = BlockStatement { statements: vec![] };

        self.next_token();

        while !self.is_curr_token(TokenType::Rbrace) && !self.is_curr_token(TokenType::Eof) {
            let statement = self.parse_statement();

            if let Some(stmt) = statement {
                block_statement.statements.push(stmt);
            }

            self.next_token();
        }

        block_statement
    }

    fn parse_string_literal(&self) -> Option<Expression> {
        Some(Expression::StringLiteral(Box::new(StringLiteral {
            value: self.curr_token.literal.clone(),
        })))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let array_literal = Expression::ArrayLiteral(Box::new(ArrayLiteral {
            elements: self.parse_expression_list(TokenType::Rbracket),
        }));

        Some(array_literal)
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Expression> {
        let mut list: Vec<Expression> = vec![];

        if self.is_peek_token(&end) {
            self.next_token();
            return list;
        }

        self.next_token();

        if let Some(new_expr) = self.parse_expression(Precedence::Lowest) {
            list.push(new_expr);
        }

        while self.is_peek_token(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            if let Some(new_expr) = self.parse_expression(Precedence::Lowest) {
                list.push(new_expr);
            }
        }

        if !self.expect_peek(end) {
            return vec![];
        }

        list
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let index_expr = Expression::IndexExpression(Box::new(IndexExpression {
            left,
            index: self.parse_expression(Precedence::Lowest)?,
        }));

        if !self.expect_peek(TokenType::Rbracket) {
            return None;
        }

        Some(index_expr)
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut hash_literal = HashLiteral { pairs: vec![] };

        while !self.is_peek_token(&TokenType::Rbrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest);

            if !self.expect_peek(TokenType::Colon) || key.is_none() {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest);

            value.as_ref()?;

            hash_literal.pairs.push((key.unwrap(), value.unwrap()));

            if !self.is_peek_token(&TokenType::Rbrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenType::Rbrace) {
            return None;
        }

        Some(Expression::HashLiteral(Box::new(hash_literal)))
    }

    fn expect_peek(&mut self, expected: TokenType) -> bool {
        if self.is_peek_token(&expected) {
            self.next_token();
            true
        } else {
            self.peek_error(Some(expected));
            false
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

    fn peek_error(&mut self, expected: Option<TokenType>) {
        self.errors.push(ParseError::UnexpectedToken {
            expected,
            got: self.peek_token.token_type.clone(),
            info: InfoPosition {
                line: self.lexer.line,
                column: self.lexer.col,
            },
        });
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::collections::HashMap;

    use crate::{
        ast::{Expression, ExpressionStatement, Statement},
        lexer::Lexer,
        parser::Parser,
        token::TokenType,
    };

    #[test]
    fn test_let_statements() {
        #[derive(Debug)]
        enum ExpectedValue {
            Integer(i64),
            Boolean(bool),
            String(String),
        }

        #[derive(Debug)]
        struct TestCase {
            input: String,
            expected_identifier: String,
            expected_value: ExpectedValue,
        }

        let tests = vec![
            TestCase {
                input: "let x = 5;".to_string(),
                expected_identifier: "x".to_string(),
                expected_value: ExpectedValue::Integer(5),
            },
            TestCase {
                input: "let y = true;".to_string(),
                expected_identifier: "y".to_string(),
                expected_value: ExpectedValue::Boolean(true),
            },
            TestCase {
                input: "let foobar = y;".to_string(),
                expected_identifier: "foobar".to_string(),
                expected_value: ExpectedValue::String("y".to_string()),
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

            let statement = &program.body[0];

            let let_stmt = match statement {
                Statement::LetStatement(let_statement) => let_statement,
                other => panic!("statement is not LetStatement. got: {:?}", other),
            };

            assert_eq!(
                let_stmt.name.value.as_ref(),
                test.expected_identifier.as_str(),
                "let statement name is not {}. got: {}",
                test.expected_identifier,
                let_stmt.name.value
            );

            let value_expr = let_stmt.value.as_ref().expect("let statement has no value");

            match (&test.expected_value, value_expr) {
                (ExpectedValue::Integer(expected), Expression::IntegerLiteral(actual)) => {
                    assert_eq!(
                        actual.value, *expected,
                        "let statement value is not {}. got: {}",
                        expected, actual.value
                    );
                }
                (ExpectedValue::Boolean(expected), Expression::Boolean(actual)) => {
                    assert_eq!(
                        actual.value, *expected,
                        "let statement value is not {}. got: {}",
                        expected, actual.value
                    );
                }
                (ExpectedValue::String(expected), Expression::Identifier(actual)) => {
                    assert_eq!(
                        actual.value.as_ref(),
                        expected.as_str(),
                        "let statement value is not {}. got: {}",
                        expected,
                        actual.value
                    );
                }
                (expected_type, actual_expr) => {
                    panic!(
                        "Type mismatch: expected {:?}, got {:?}",
                        expected_type, actual_expr
                    );
                }
            }
        }
    }

    #[test]
    fn test_return_statement() {
        #[derive(Debug)]
        enum ExpectedValue {
            Integer(i64),
            String(String),
        }

        #[derive(Debug)]
        struct TestCase {
            input: String,
            expected_value: ExpectedValue,
        }

        let tests = vec![
            TestCase {
                input: "return 5;".to_string(),
                expected_value: ExpectedValue::Integer(5),
            },
            TestCase {
                input: "return fooBar;".to_string(),
                expected_value: ExpectedValue::String("fooBar".to_string()),
            },
            TestCase {
                input: "return 100500;".to_string(),
                expected_value: ExpectedValue::Integer(100500),
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

            let statement = &program.body[0];

            let return_stmt = match statement {
                Statement::ReturnStatement(return_statement) => return_statement,
                other => panic!("statement is not ReturnStatement. got: {:?}", other),
            };

            let value_expr = return_stmt
                .return_value
                .as_ref()
                .expect("return statement has no value");

            match (&test.expected_value, value_expr) {
                (ExpectedValue::Integer(expected), Expression::IntegerLiteral(actual)) => {
                    assert_eq!(
                        actual.value, *expected,
                        "return statement value is not {}. got: {}",
                        expected, actual.value
                    );
                }
                (ExpectedValue::String(expected), Expression::Identifier(actual)) => {
                    assert_eq!(
                        actual.value.as_ref(),
                        expected.as_str(),
                        "return statement value is not {}. got: {}",
                        expected,
                        actual.value
                    );
                }
                (expected_type, actual_expr) => {
                    panic!(
                        "Type mismatch: expected {:?}, got {:?}",
                        expected_type, actual_expr
                    );
                }
            }
        }
    }

    #[test]
    fn test_parsing_for_statement() {
        #[derive(Debug)]
        struct TestCase {
            input: String,
            expected_init: String,
            expected_test: String,
            expected_update: String,
            expected_body_str: String,
        }

        let tests = vec![TestCase {
            input: "for (let i = 0; i < 10; i += 1) { let b = 0; }".to_string(),
            expected_body_str: "let b = 0;".to_string(),
            expected_init: "let i = 0;".to_string(),
            expected_test: "(i < 10)".to_string(),
            expected_update: "(i += 1)".to_string(),
        }];

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

            let statement = &program.body[0];

            let for_stmt = match statement {
                Statement::ForStatement(for_stmt) => for_stmt,
                other => panic!("statement is not ForStatement. got: {:?}", other),
            };

            assert_eq!(for_stmt.init.to_string(), test.expected_init);
            assert_eq!(for_stmt.test.to_string(), test.expected_test);
            assert_eq!(for_stmt.update.to_string(), test.expected_update);
            assert_eq!(for_stmt.body.to_string(), test.expected_body_str);
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
                assert_eq!(identifier.value.as_ref(), "variableName");
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
            operator: TokenType,
            int_value: i64,
        }

        let tests: Vec<TestCase> = vec![
            TestCase {
                input: "!5".to_string(),
                operator: TokenType::Bang,
                int_value: 5,
            },
            TestCase {
                input: "-15".to_string(),
                operator: TokenType::Minus,
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

            let expr_stmt = match stmt {
                Statement::ExpressionStatement(expression_statement) => expression_statement,

                other => panic!("statement is not ExpressionStatement. got: {:?}", other),
            };

            test_infix_expression(
                &expr_stmt.expression.as_ref().expect("Expression is None"),
                test.left_val,
                &test.operator,
                test.right_val,
            );
        }
    }

    #[test]
    fn test_parsing_assign_operators_infix_expression() {
        struct TestCase {
            input: String,
            left_val: String,
            operator: String,
            right_val: i64,
        }

        let tests: Vec<TestCase> = vec![
            TestCase {
                input: "a += 1;".to_string(),
                left_val: "a".to_string(),
                operator: "+=".to_string(),
                right_val: 1,
            },
            TestCase {
                input: "a *= 1;".to_string(),
                left_val: "a".to_string(),
                operator: "*=".to_string(),
                right_val: 1,
            },
            TestCase {
                input: "a -= 1;".to_string(),
                left_val: "a".to_string(),
                operator: "-=".to_string(),
                right_val: 1,
            },
            TestCase {
                input: "a /= 1;".to_string(),
                left_val: "a".to_string(),
                operator: "/=".to_string(),
                right_val: 1,
            },
            TestCase {
                input: "a = 5;".to_string(),
                left_val: "a".to_string(),
                operator: "=".to_string(),
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

            let expr_stmt = match stmt {
                Statement::ExpressionStatement(expression_statement) => expression_statement,

                other => panic!("statement is not ExpressionStatement. got: {:?}", other),
            };

            let infix_expr = match expr_stmt
                .expression
                .as_ref()
                .expect("Expression statement Expression is None")
            {
                Expression::InfixExpression(infix_expr) => infix_expr,
                other => panic!("expression is not InfixExpression. got: {:?}", other),
            };

            assert_eq!(
                infix_expr.operator.to_str(),
                test.operator,
                "Expected InfixExpression operator: {}. Got: {}",
                test.operator,
                infix_expr.operator
            );

            let left_ident = match &infix_expr.left {
                Expression::Identifier(ident) => ident,
                other => panic!(
                    "infix expression left is not IntegerLiteral. got: {:?}",
                    other
                ),
            };

            assert_eq!(
                left_ident.value.as_ref(),
                test.left_val,
                "Expected InfixExpression left value: {}. Got: {}",
                test.left_val,
                left_ident.value
            );

            let right_literal = match &infix_expr.right {
                Expression::IntegerLiteral(int_literal) => int_literal,
                other => panic!(
                    "infix expression right is not IntegerLiteral. got: {:?}",
                    other
                ),
            };

            assert_eq!(
                right_literal.value, test.right_val,
                "Expected InfixExpression right value: {}. Got: {}",
                test.right_val, right_literal.value
            );
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
            /* Grouped expressions */
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            /* Functions */
            ("add(1, 2)", "add(1, 2)"),
            ("add(1, 2 * 3, 4 + 5)", "add(1, (2 * 3), (4 + 5))"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
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

    #[test]
    fn test_if_expression() {
        let tests = vec![
            "if (x < y) { x }".to_string(),
            "if (x < y) { x } else { y }".to_string(),
        ];

        for test in tests {
            let lexer = Lexer::new(test);

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

            let expr_stmt = match stmt {
                Statement::ExpressionStatement(expression_statement) => expression_statement,

                other => panic!("statement is not ExpressionStatement. got: {:?}", other),
            };

            let if_expr = match &expr_stmt.expression {
                Some(Expression::IfExpression(if_expr)) => if_expr,
                other => panic!("expression statement is not IfExpression. got: {:?}", other),
            };

            let infix_expr = match &if_expr.condition {
                Expression::InfixExpression(infix_expr) => infix_expr,
                other => panic!(
                    "if expression condition is not InfixExpression. got: {:?}",
                    other
                ),
            };

            assert_eq!(
                infix_expr.operator.to_str(),
                "<",
                "infix expression operator is not '<'. got: {}",
                infix_expr.operator
            );

            let left_ident = match &infix_expr.left {
                Expression::Identifier(ident) => ident,
                other => panic!("infix expression left is not Identifier. got: {:?}", other),
            };

            assert_eq!(
                left_ident.value.as_ref(),
                "x",
                "left identifier value is not 'x'. got: {}",
                left_ident.value
            );

            let right_ident = match &infix_expr.right {
                Expression::Identifier(ident) => ident,
                other => panic!("infix expression right is not Identifier. got: {:?}", other),
            };

            assert_eq!(
                right_ident.value.as_ref(),
                "y",
                "right identifier value is not 'y'. got: {}",
                right_ident.value
            );

            assert_eq!(
                if_expr.consequence.statements.len(),
                1,
                "consequece statements is not 1. got: {}",
                if_expr.consequence.statements.len()
            );

            let consequence = match if_expr.consequence.statements.first().unwrap() {
                Statement::ExpressionStatement(expression_statement) => expression_statement,

                other => panic!(
                    "if expression consequence is not ExpressionStatement. got: {:?}",
                    other
                ),
            };

            test_identifier(consequence.expression.as_ref().unwrap().clone(), "x");

            if if_expr.alternative.is_some() {
                let alternative = if_expr.alternative.as_ref().unwrap();

                assert_eq!(
                    alternative.statements.len(),
                    1,
                    "alternative statements is not 1. got: {}",
                    alternative.statements.len()
                );

                let alternative_stmt = match alternative.statements.first().unwrap() {
                    Statement::ExpressionStatement(expression_statement) => expression_statement,

                    other => panic!(
                        "if expression alternative is not ExpressionStatement. got: {:?}",
                        other
                    ),
                };

                test_identifier(alternative_stmt.expression.as_ref().unwrap().clone(), "y");
            }
        }
    }

    #[test]
    fn test_parsing_function_literal() {
        let input = "function(x, y) { x + y; }".to_string();

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

        let stmt = program.body.first().unwrap();

        let expr_stmt = match stmt {
            Statement::ExpressionStatement(expression_statement) => expression_statement,

            other => panic!("statement is not ExpressionStatement. got: {:?}", other),
        };

        let function_literal = match &expr_stmt.expression {
            Some(Expression::FunctionLiteral(function_literal)) => function_literal,
            other => panic!(
                "expression statement is not FunctionLiteral. got: {:?}",
                other
            ),
        };

        assert_eq!(
            function_literal.parameters.len(),
            2,
            "function parameter slength should be 2. got: {}",
            function_literal.parameters.len()
        );

        assert_eq!(
            function_literal.parameters[0].value.as_ref(),
            "x",
            "first parameter should be x. got: {}",
            function_literal.parameters[0].value
        );

        assert_eq!(
            function_literal.parameters[1].value.as_ref(),
            "y",
            "second parameter should be y. got: {}",
            function_literal.parameters[1].value
        );

        assert_eq!(
            function_literal.body.statements.len(),
            1,
            "function body statements should be 1. got: {}",
            function_literal.body.statements.len()
        );

        let body_stmt = match &function_literal.body.statements[0] {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            other => panic!(
                "function body statement is not ExpressionStatement. got: {:?}",
                other
            ),
        };

        let infix_expr = match &body_stmt.expression {
            Some(Expression::InfixExpression(infix_expr)) => infix_expr,
            other => panic!("expression is not InfixExpression. got: {:?}", other),
        };

        assert_eq!(
            infix_expr.operator.to_str(),
            "+",
            "Expected InfixExpression operator: +. Got: {}",
            infix_expr.operator
        );

        let left_literal = match &infix_expr.left {
            Expression::Identifier(ident) => ident,
            other => panic!(
                "infix expression left is not IntegerLiteral. got: {:?}",
                other
            ),
        };

        assert_eq!(
            left_literal.value.as_ref(),
            "x",
            "Expected InfixExpression left value: 'x'. Got: {}",
            left_literal.value
        );

        let right_literal = match &infix_expr.right {
            Expression::Identifier(ident) => ident,
            other => panic!(
                "infix expression right is not IntegerLiteral. got: {:?}",
                other
            ),
        };

        assert_eq!(
            right_literal.value.as_ref(),
            "y",
            "Expected InfixExpression right value: 'y'. Got: {}",
            right_literal.value
        );
    }

    #[test]
    fn test_parsing_function_parameters() {
        let tests = vec![
            ("function() {};".to_string(), vec![]),
            ("function(x) {};".to_string(), vec!["x".to_string()]),
            (
                "function(x, y) {};".to_string(),
                vec!["x".to_string(), "y".to_string()],
            ),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);

            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(
                parser.errors.len(),
                0,
                "found errors while parsing: {:#?}",
                parser.errors
            );

            let stmt = program.body.first().unwrap();

            let expr_stmt = match stmt {
                Statement::ExpressionStatement(expression_statement) => expression_statement,
                other => panic!("statement is not ExpressionStatement. got: {:?}", other),
            };

            let function_literal = match &expr_stmt.expression {
                Some(Expression::FunctionLiteral(function_literal)) => function_literal,
                other => panic!(
                    "expression statement is not FunctionLiteral. got: {:?}",
                    other
                ),
            };

            assert_eq!(
                function_literal.parameters.len(),
                test.1.len(),
                "length parameters wrong. got: {}, need: {}",
                function_literal.parameters.len(),
                test.1.len()
            );

            for (i, ident) in test.1.into_iter().enumerate() {
                assert_eq!(
                    function_literal.parameters[i].value.as_ref(),
                    ident,
                    "first parameter should be {}. got: {}",
                    ident,
                    function_literal.parameters[i].value
                );
            }
        }
    }

    #[test]
    fn test_parsing_call_expression() {
        let input = "method(1, 2 * 3, 4 + 5);".to_string();

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

        let stmt = program.body.first().unwrap();

        let expr_stmt = match stmt {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            other => panic!("statement is not ExpressionStatement. got: {:?}", other),
        };

        let call_expr = match &expr_stmt.expression {
            Some(Expression::CallExpression(call_expr)) => call_expr.clone(),
            other => panic!("expression is not CallExpression. got: {:?}", other),
        };

        test_identifier(call_expr.function, "method");

        assert_eq!(
            call_expr.arguments.len(),
            3,
            "wrong length of arguments. got: {}. want: {}",
            call_expr.arguments.len(),
            3
        );

        let first_arg = match &call_expr.arguments[0] {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            other => panic!("expression is not IntegerLiteral. got: {:?}", other),
        };

        assert_eq!(first_arg.value, 1);

        let second_arg = match &call_expr.arguments[1] {
            Expression::InfixExpression(infix_expr) => infix_expr,
            other => panic!("expression is not InfixExpression. got: {:?}", other),
        };

        assert_eq!(second_arg.operator.to_str(), "*");

        let second_left = match &second_arg.left {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            other => panic!("expression is not IntegerLiteral. got: {:?}", other),
        };
        assert_eq!(second_left.value, 2);

        let second_right = match &second_arg.right {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            other => panic!("expression is not IntegerLiteral. got: {:?}", other),
        };
        assert_eq!(second_right.value, 3);

        let third_arg = match &call_expr.arguments[2] {
            Expression::InfixExpression(infix_expr) => infix_expr,
            other => panic!("expression is not InfixExpression. got: {:?}", other),
        };

        assert_eq!(third_arg.operator.to_str(), "+");

        let third_left = match &third_arg.left {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            other => panic!("expression is not IntegerLiteral. got: {:?}", other),
        };
        assert_eq!(third_left.value, 4);

        let third_right = match &third_arg.right {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            other => panic!("expression is not IntegerLiteral. got: {:?}", other),
        };
        assert_eq!(third_right.value, 5);
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"".to_string();

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

        let stmt = program.body.first().unwrap();

        let Statement::ExpressionStatement(expr_stmt) = stmt else {
            panic!("statement is not ExpressionStatement, got: {:?}", stmt);
        };

        let Expression::StringLiteral(str_literal) = expr_stmt
            .expression
            .as_ref()
            .expect("ExpressionStatement expression is empty")
        else {
            panic!("expression is not StringLiteral, got: {:?}", stmt);
        };

        assert_eq!(str_literal.value.as_ref(), "hello world");
    }

    #[test]
    fn test_parsing_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]".to_string();

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

        let stmt = program.body.first().unwrap();

        let Statement::ExpressionStatement(expr_stmt) = stmt else {
            panic!("statement is not ExpressionStatement, got: {:?}", stmt);
        };

        let Some(Expression::ArrayLiteral(array_literal)) = &expr_stmt.expression else {
            panic!(
                "ExpressionStatement is not ArrayLiteral, got: {:?}",
                expr_stmt.expression
            );
        };
        assert_eq!(
            array_literal.elements.len(),
            3,
            "not enough elements in array, got: {}",
            array_literal.elements.len()
        );

        if let Expression::IntegerLiteral(int_literal) = &array_literal.elements[0] {
            assert_eq!(int_literal.value, 1);
        } else {
            panic!("array_literal.elements[0] is not IntegerLiteral");
        }

        test_infix_expression(&array_literal.elements[1], 2, "*", 2);
        test_infix_expression(&array_literal.elements[2], 3, "+", 3);
    }

    #[test]
    fn test_parsing_index_expression() {
        let input = "arr[1 + 1]".to_string();

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

        let stmt = program.body.first().unwrap();

        let Statement::ExpressionStatement(expr_stmt) = stmt else {
            panic!("statement is not ExpressionStatement, got: {:?}", stmt);
        };

        let Some(Expression::IndexExpression(index_expr)) = &expr_stmt.expression else {
            panic!("statement is not ExpressionStatement, got: {:?}", stmt);
        };

        test_identifier(index_expr.left.clone(), "arr");

        test_infix_expression(&index_expr.index, 1, "+", 1);
    }

    #[test]
    fn test_parsing_hash_literal_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}".to_string();
        let expected = HashMap::from([
            ("one".to_string(), 1),
            ("two".to_string(), 2),
            ("three".to_string(), 3),
        ]);

        let expr_stmt = parse(&input);

        let Some(Expression::HashLiteral(hash_literal)) = &expr_stmt.expression else {
            panic!(
                "expression is not HashLiteral, got: {:?}",
                expr_stmt.expression
            );
        };

        assert_eq!(hash_literal.pairs.len(), 3);

        for (key, value) in &hash_literal.pairs {
            let Expression::StringLiteral(key) = key else {
                panic!("key is not StringLiteral, got: {:?}", key);
            };

            let expected_val = expected
                .get(&key.value.to_string())
                .expect("Key not found in expected HashMap");

            let Expression::IntegerLiteral(int_lit) = value else {
                panic!("value is not IntegerLiteral, got: {:?}", value);
            };

            assert_eq!(int_lit.value, *expected_val);
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}".to_string();

        let expr_stmt = parse(&input);

        let Some(Expression::HashLiteral(hash_literal)) = &expr_stmt.expression else {
            panic!(
                "expression is not HashLiteral, got: {:?}",
                expr_stmt.expression
            );
        };

        assert_eq!(hash_literal.pairs.len(), 3);

        for (key, value) in &hash_literal.pairs {
            let Expression::StringLiteral(key) = key else {
                panic!("key is not StringLiteral, got: {:?}", key);
            };

            match key.value.as_ref() {
                "one" => test_infix_expression(value, 0, "+", 1),
                "two" => test_infix_expression(value, 10, "-", 8),
                "three" => test_infix_expression(value, 15, "/", 5),
                _ => panic!("Unexpected key: {}", key.value),
            }
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}".to_string();

        let expr_stmt = parse(&input);

        let Some(Expression::HashLiteral(hash_literal)) = &expr_stmt.expression else {
            panic!(
                "expression is not HashLiteral, got: {:?}",
                expr_stmt.expression
            );
        };

        assert_eq!(hash_literal.pairs.len(), 0);
    }

    #[test]
    fn test_syntax_error_position() {
        // TODO: !!!!! fix index by 1 errors, maybe move line and columns to Token struct
        let tests = vec![
            (
                "let x 5;",
                "SyntaxError(Line: 1, Column: 8): Expected next token to be '=', got 'INT' instead",
            ),
            (
                "let = 5;",
                "SyntaxError(Line: 1, Column: 6): Expected next token to be 'IDENT', got '=' instead",
            ),
            // (
            //     "if (x > y) { x } else",
            //     "SyntaxError(Line: 1, Column: 23): Expected next token to be '{', got 'EOF' instead",
            // ),
        ];

        for (input, expected_error) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            parser.parse_program();

            assert!(
                parser.is_err(),
                "parser should have errors for input: \"{}\"",
                input
            );
            assert_eq!(
                parser.errors.len(),
                1,
                "parser should have exactly one error for input: \"{}\", got: {:?}",
                input,
                parser.errors
            );

            let error = parser.errors[0].to_string();
            assert_eq!(error, expected_error, "for input \"{}\"", input);
        }
    }

    fn parse(input: &str) -> ExpressionStatement {
        let lexer = Lexer::new(input.to_string());
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

        let Statement::ExpressionStatement(expr_stmt) = stmt else {
            panic!("statement is not ExpressionStatement, got: {:?}", stmt);
        };

        *expr_stmt.clone()
    }

    fn test_infix_expression(expr: &Expression, left: i64, operator: &str, right: i64) {
        let infix_expr = match expr {
            Expression::InfixExpression(infix_expr) => infix_expr,
            other => panic!("expression is not InfixExpression. got: {:?}", other),
        };

        assert_eq!(
            infix_expr.operator.to_str(),
            operator,
            "Expected InfixExpression operator: {}. Got: {}",
            operator,
            infix_expr.operator
        );

        let left_literal = match &infix_expr.left {
            Expression::IntegerLiteral(int_literal) => int_literal,
            other => panic!(
                "infix expression left is not IntegerLiteral. got: {:?}",
                other
            ),
        };

        assert_eq!(
            left_literal.value, left,
            "Expected InfixExpression left value: {}. Got: {}",
            left, left_literal.value
        );

        let right_literal = match &infix_expr.right {
            Expression::IntegerLiteral(int_literal) => int_literal,
            other => panic!(
                "infix expression right is not IntegerLiteral. got: {:?}",
                other
            ),
        };

        assert_eq!(
            right_literal.value, right,
            "Expected InfixExpression right value: {}. Got: {}",
            right, right_literal.value
        );
    }

    fn test_let_statement(stmt: &Statement, expected: &str) {
        if let Statement::LetStatement(let_stmt) = stmt {
            assert_eq!(let_stmt.name.value.as_ref(), expected);
        } else {
            panic!("stmt is not LetStatement");
        };
    }

    fn test_identifier(expr: Expression, value: &str) {
        let identifier = match expr {
            Expression::Identifier(identifier) => identifier,
            other => panic!("expression not identifier. got: {:?}", other),
        };

        assert_eq!(
            identifier.value.as_ref(),
            value,
            "identifier value is not {}. got: {}",
            value,
            identifier.value
        );
    }
}
