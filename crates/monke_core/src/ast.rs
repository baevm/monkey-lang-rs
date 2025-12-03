use std::rc::Rc;

use crate::token::TokenType;

pub struct Program {
    pub body: Vec<Statement>,
}

/* Statements */
#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(Box<LetStatement>),
    ReturnStatement(Box<ReturnStatement>),
    ExpressionStatement(Box<ExpressionStatement>),
    BlockStatement(Box<BlockStatement>),
    ForStatement(Box<ForStatement>),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display_msg = match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.to_string()
            }
            Statement::BlockStatement(block_stmt) => block_stmt.to_string(),
            Statement::ForStatement(for_statement) => for_statement.to_string(),
        };

        write!(f, "{}", display_msg)
    }
}

impl Program {
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
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        sb.push_str(&format!("let {} = ", self.name.to_string(),));

        if let Some(val) = &self.value {
            sb.push_str(&val.to_string());
        }

        sb.push(';');

        write!(f, "{}", sb)
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Option<Expression>,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        sb.push_str("return");

        if let Some(return_val) = &self.return_value {
            sb.push_str(&return_val.to_string());
        }

        sb.push(';');

        write!(f, "{}", sb)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Option<Expression>,
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        if let Some(expr_stmt) = &self.expression {
            sb.push_str(&expr_stmt.to_string())
        }

        write!(f, "{}", sb)
    }
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: Statement,
    pub test: Expression,
    pub update: Expression,
    pub body: BlockStatement,
}

impl std::fmt::Display for ForStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        sb.push_str(&format!(
            "for({};{};{}) {{\n {} \n}}",
            self.init.to_string(),
            self.test.to_string(),
            self.update.to_string(),
            self.body.to_string(),
        ));

        write!(f, "{}", sb)
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
    StringLiteral(Box<StringLiteral>),
    ArrayLiteral(Box<ArrayLiteral>),
    IndexExpression(Box<IndexExpression>),
    HashLiteral(Box<HashLiteral>),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display_msg = match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(int_literal) => int_literal.to_string(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.to_string(),
            Expression::InfixExpression(infix_expression) => infix_expression.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::IfExpression(if_expression) => if_expression.to_string(),
            Expression::FunctionLiteral(function_literal) => function_literal.to_string(),
            Expression::CallExpression(call_expression) => call_expression.to_string(),
            Expression::StringLiteral(string_literal) => string_literal.to_string(),
            Expression::ArrayLiteral(array_literal) => array_literal.to_string(),
            Expression::IndexExpression(index_expression) => index_expression.to_string(),
            Expression::HashLiteral(hash_literal) => hash_literal.to_string(),
        };

        write!(f, "{}", display_msg)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: Rc<str>,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub operator: TokenType,
    pub right: Expression,
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!("({}{})", self.operator, self.right.to_string())
        )
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub left: Expression,
    pub operator: TokenType,
    pub right: Expression,
}

impl InfixExpression {
    pub fn is_compound_assign(&self) -> bool {
        match self.operator {
            TokenType::AssignAdd
            | TokenType::AssignSub
            | TokenType::AssignMul
            | TokenType::AssignDiv => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "({} {} {})",
                self.left.to_string(),
                self.operator,
                self.right.to_string()
            )
        )
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,         // if body
    pub alternative: Option<BlockStatement>, // optional else body
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sb = String::new();

        sb.push_str(&format!(
            "if {} {}",
            self.condition.to_string(),
            self.consequence.to_string(),
        ));

        if let Some(alternative) = &self.alternative {
            sb.push_str(&format!("else {}", alternative.to_string()));
        }

        write!(f, "{}", sb)
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: String = self.statements.iter().map(|st| st.to_string()).collect();

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub name: String,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl std::fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(
            f,
            "{}",
            format!("function({}){}", params, self.body.to_string())
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}", format!("{}({})", self.function.to_string(), args))
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: Rc<str>,
}

impl std::fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl std::fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();

        write!(f, "{}", format!("[{}]", elements.join(", ")))
    }
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub left: Expression,
    pub index: Expression,
}

impl std::fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!("({}[{}])", self.left.to_string(), self.index.to_string())
        )
    }
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}

impl std::fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}:{}", k.to_string(), v.to_string()))
            .collect();

        write!(f, "{}", format!("{{{}}}", elements.join(", ")))
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};

    #[test]
    fn test_string_method() {
        // let myVar = anotherVar;
        let program = Program {
            body: vec![Statement::LetStatement(Box::new(LetStatement {
                name: Identifier {
                    value: Rc::from("myVar"),
                },
                value: Some(Expression::Identifier(Box::new(Identifier {
                    value: Rc::from("anotherVar"),
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
