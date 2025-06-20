use crate::token::Token;

pub trait Stringer {
    fn to_string(&self) -> String;
}

pub struct Program {
    pub body: Vec<Statement>,
}

/* Statements */
pub enum Statement {
    LetStatement(Box<LetStatement>),
    ReturnStatement(Box<ReturnStatement>),
    ExpressionStatement(Box<ExpressionStatement>),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.token.literal.clone(),
            Statement::ReturnStatement(return_statement) => return_statement.token.literal.clone(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.token.literal.clone()
            }
        }
    }
}

impl Stringer for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.to_string()
            }
        }
    }
}

impl Program {
    fn token_literal(&self) -> String {
        if self.body.len() > 0 {
            self.body.first().unwrap().token_literal()
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        let mut sb = String::new();

        for stmt in &self.body {
            sb.push_str(&stmt.to_string());
        }

        sb
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl Stringer for LetStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!(
            "{} {} = ",
            self.token.literal,
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
    pub return_value: Option<Expression>,
}

impl Stringer for ReturnStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!("{} ", self.token.literal));

        if let Some(return_val) = &self.return_value {
            sb.push_str(&return_val.to_string());
        }

        sb.push_str(";");

        sb
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl Stringer for ExpressionStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        if let Some(expr_stmt) = &self.expression {
            match &*expr_stmt {
                Expression::Identifier(identifier) => {
                    sb.push_str(&identifier.to_string());
                }
                Expression::IntegerLiteral(integer_literal) => {
                    sb.push_str(&integer_literal.to_string());
                }
            }
        }

        sb
    }
}

/* Expressions */
pub enum Expression {
    Identifier(Box<Identifier>),
    IntegerLiteral(Box<IntegerLiteral>),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.token.literal.clone(),
            Expression::IntegerLiteral(int_litereal) => int_litereal.token.literal.clone(),
        }
    }
}

impl Stringer for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(int_literal) => int_literal.to_string(),
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Stringer for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Stringer for IntegerLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Identifier, LetStatement, Program, Statement},
        token::{Token, TokenType},
    };

    #[test]
    fn test_string_method() {
        // let myVar = anotherVar;
        let program = Program {
            body: vec![Statement::LetStatement(Box::new(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Some(Expression::Identifier(Box::new(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }))),
            }))],
        };

        assert_eq!(
            program.to_string(),
            "let myVar = anotherVar;",
            "string method is wrong. Got: {}",
            program.to_string()
        )
    }
}
