use crate::token::Token;

pub trait Stringer {
    fn to_string(&self) -> String;
}

pub struct Program {
    pub body: Vec<Statement>,
}

/* Statements */
#[derive(Debug, Clone)]
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

    pub fn to_string(&self) -> String {
        let mut sb = String::new();

        for stmt in &self.body {
            sb.push_str(&stmt.to_string());
        }

        sb
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
                Expression::PrefixExpression(prefix_expression) => {
                    sb.push_str(&prefix_expression.to_string())
                }
                Expression::InfixExpression(infix_expression) => {
                    sb.push_str(&infix_expression.to_string())
                }
                Expression::Boolean(boolean) => sb.push_str(&boolean.to_string()),
                Expression::IfExpression(if_expression) => sb.push_str(&if_expression.to_string()),
                Expression::FunctionLiteral(function_literal) => {
                    sb.push_str(&function_literal.to_string())
                }
                Expression::CallExpression(call_expression) => {
                    sb.push_str(&call_expression.to_string())
                }
            }
        }

        sb
    }
}

/* Expressions */
#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Box<Identifier>),
    IntegerLiteral(Box<IntegerLiteral>),
    PrefixExpression(Box<PrefixExpression>),
    InfixExpression(Box<InfixExpression>),
    Boolean(Box<Boolean>),
    IfExpression(Box<IfExpression>),
    FunctionLiteral(Box<FunctionLiteral>),
    CallExpression(Box<CallExpression>),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.token.literal.clone(),
            Expression::IntegerLiteral(int_litereal) => int_litereal.token.literal.clone(),
            Expression::PrefixExpression(prefix_expression) => {
                prefix_expression.token.literal.clone()
            }
            Expression::InfixExpression(infix_expression) => infix_expression.token.literal.clone(),
            Expression::Boolean(boolean) => boolean.token.literal.clone(),
            Expression::IfExpression(if_expression) => if_expression.token.literal.clone(),
            Expression::FunctionLiteral(function_literal) => function_literal.token.literal.clone(),
            Expression::CallExpression(call_expression) => call_expression.token.literal.clone(),
        }
    }
}

impl Stringer for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(int_literal) => int_literal.to_string(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.to_string(),
            Expression::InfixExpression(infix_expression) => infix_expression.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::IfExpression(if_expression) => if_expression.to_string(),
            Expression::FunctionLiteral(function_literal) => function_literal.to_string(),
            Expression::CallExpression(call_expression) => call_expression.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Stringer for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Stringer for IntegerLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

impl Stringer for PrefixExpression {
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

impl Stringer for InfixExpression {
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Stringer for Boolean {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,         // if body
    pub alternative: Option<BlockStatement>, // optional else body
}

impl Stringer for IfExpression {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!(
            "if {} {}",
            self.condition.to_string(),
            self.consequence.to_string(),
        ));

        if let Some(alternative) = &self.alternative {
            sb.push_str(&format!("else {}", alternative.to_string()));
        }

        sb
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Stringer for BlockStatement {
    fn to_string(&self) -> String {
        self.statements.iter().map(|st| st.to_string()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Stringer for FunctionLiteral {
    fn to_string(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "{}({}){}",
            self.token.literal,
            params,
            self.body.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl Stringer for CallExpression {
    fn to_string(&self) -> String {
        let args = self
            .arguments
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!("{}({})", self.function.to_string(), args)
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
