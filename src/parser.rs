use crate::{
    ast::{
        self, Expression, ExpressionStatement, Identifier, LetStatement, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
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
        let mut program = ast::Program { statements: vec![] };

        loop {
            if self.curr_token.token_type == TokenType::Eof {
                break;
            }

            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.curr_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.curr_token.clone();

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

        let let_stmt = LetStatement {
            token,
            name,
            value: None,
        };

        Some(Box::new(let_stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let return_stmt = ReturnStatement {
            token: self.curr_token.clone(),
            return_value: None,
        };

        self.next_token();

        while !self.is_curr_token(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(return_stmt))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let expr_stmt = ExpressionStatement {
            token: self.curr_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.is_peek_token(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(expr_stmt))
    }

    fn parse_expression(&self, lowest: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.parse_prefix(&self.curr_token.token_type);

        if prefix.is_none() {
            return None;
        }

        return prefix;
    }

    fn parse_prefix(&self, token: &TokenType) -> Option<Box<dyn Expression>> {
        match token {
            TokenType::Ident => self.parse_identifier(),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        }))
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

    fn peek_error(&mut self, expected: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, self.peek_token.token_type
        );

        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        ast::{
            Expression, ExpressionStatement, Identifier, LetStatement, ReturnStatement, Statement,
        },
        lexer::{self, Lexer},
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

        assert_eq!(
            program.statements.len(),
            3,
            "not enough statements in program"
        );

        let tests = vec!["x", "y", "longNameVariable"];

        for (i, test) in tests.iter().enumerate() {
            let statement = &*program.statements[i];

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

        assert_eq!(
            program.statements.len(),
            3,
            "not enough statements in program"
        );

        for stmt in program.statements {
            if let Some(return_stmt) = stmt.as_any().downcast_ref::<ReturnStatement>() {
                assert_eq!(
                    return_stmt.token_literal(),
                    "return",
                    "return statement token literal is not 'return'. Got: {} ",
                    return_stmt.token_literal()
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

        assert_eq!(
            program.statements.len(),
            1,
            "not enough statements in program"
        );

        if let Some(expr_stmt) = program
            .statements
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<ExpressionStatement>()
        {
            if let Some(identifier) = expr_stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<Identifier>()
            {
                assert_eq!(identifier.value, "variableName");
                assert_eq!(identifier.token_literal(), "variableName");
            } else {
                panic!("expression statement is not Identifier");
            }
        } else {
            panic!("stmt is not ExpressionStatement");
        }
    }

    fn test_let_statement(stmt: &dyn Statement, expected: &str) {
        assert_eq!(stmt.token_literal(), "let");

        if let Some(let_stmt) = stmt.as_any().downcast_ref::<LetStatement>() {
            assert_eq!(let_stmt.name.value, expected);
            assert_eq!(let_stmt.name.token_literal(), expected);
        } else {
            panic!("stmt is not LetStatement");
        };
    }
}
