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
    BlockStatement(Box<BlockStatement>),
}

impl Stringer for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.to_string()
            }
            Statement::BlockStatement(block_stmt) => block_stmt.to_string(),
        }
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

impl Stringer for LetStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str(&format!("let {} = ", self.name.to_string(),));

        if let Some(val) = &self.value {
            sb.push_str(&val.to_string());
        }

        sb.push_str(";");

        sb
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Option<Expression>,
}

impl Stringer for ReturnStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        sb.push_str("return");

        if let Some(return_val) = &self.return_value {
            sb.push_str(&return_val.to_string());
        }

        sb.push_str(";");

        sb
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Option<Expression>,
}

impl Stringer for ExpressionStatement {
    fn to_string(&self) -> String {
        let mut sb = String::new();

        if let Some(expr_stmt) = &self.expression {
            sb.push_str(&expr_stmt.to_string())
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
    StringLiteral(Box<StringLiteral>),
    ArrayLiteral(Box<ArrayLiteral>),
    IndexExpression(Box<IndexExpression>),
    HashLiteral(Box<HashLiteral>),
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
            Expression::StringLiteral(string_literal) => string_literal.to_string(),
            Expression::ArrayLiteral(array_literal) => array_literal.to_string(),
            Expression::IndexExpression(index_expression) => index_expression.to_string(),
            Expression::HashLiteral(hash_literal) => hash_literal.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: String,
}

impl Stringer for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl Stringer for IntegerLiteral {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
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
    pub value: bool,
}

impl Stringer for Boolean {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
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
    pub statements: Vec<Statement>,
}

impl Stringer for BlockStatement {
    fn to_string(&self) -> String {
        self.statements.iter().map(|st| st.to_string()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
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

        format!("function({}){}", params, self.body.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
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

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl Stringer for StringLiteral {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl Stringer for ArrayLiteral {
    fn to_string(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();

        format!("[{}]", elements.join(", "))
    }
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub left: Expression,
    pub index: Expression,
}

impl Stringer for IndexExpression {
    fn to_string(&self) -> String {
        format!("({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Stringer for HashLiteral {
    fn to_string(&self) -> String {
        let elements: Vec<String> = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}:{}", k.to_string(), v.to_string()))
            .collect();

        format!("{{{}}}", elements.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};

    #[test]
    fn test_string_method() {
        // let myVar = anotherVar;
        let program = Program {
            body: vec![Statement::LetStatement(Box::new(LetStatement {
                name: Identifier {
                    value: "myVar".to_string(),
                },
                value: Some(Expression::Identifier(Box::new(Identifier {
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
