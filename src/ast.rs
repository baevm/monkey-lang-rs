use std::any::Any;

use crate::token::{Token, TokenType};

pub trait Statement: Any {
    fn token_literal(&self) -> String;

    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

pub trait Expression {
    fn token_literal(&self) -> String;

    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements.first().unwrap().token_literal()
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        let mut sb = String::new();

        for stmt in &self.statements {
            sb.push_str(&stmt.to_string());
        }

        sb
    }
}

/* Statements */

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!(
            "{} {} = ",
            self.token_literal(),
            self.name.to_string(),
        ));

        if let Some(val) = &self.value {
            sb.push_str(&val.to_string());
        }

        sb.push_str(";");

        sb
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!("{} ", self.token_literal()));

        if let Some(return_val) = &self.return_value {
            sb.push_str(&return_val.to_string());
        }

        sb.push_str(";");

        sb
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut sb = String::new();

        if let Some(expr_stmt) = &self.expression {
            sb.push_str(&expr_stmt.to_string());
        }

        sb
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/* Expressions */

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Identifier, LetStatement, Program},
        token::{Token, TokenType},
    };

    #[test]
    fn test_string_method() {
        // let myVar = anotherVar;
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token {
                    literal: "let".to_string(),
                    token_type: TokenType::Let,
                },
                name: Identifier {
                    token: Token {
                        literal: "myVar".to_string(),
                        token_type: TokenType::Ident,
                    },
                    value: "myVar".to_string(),
                },
                value: Some(Box::new(Identifier {
                    token: Token {
                        literal: "anotherVar".to_string(),
                        token_type: TokenType::Ident,
                    },
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert_eq!(
            program.to_string(),
            "let myVar = anotherVar;",
            "string method is wrong. Got: {}",
            program.to_string()
        )
    }
}
