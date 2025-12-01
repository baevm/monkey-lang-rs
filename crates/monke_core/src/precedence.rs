use crate::token::TokenType;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Equals,      // ==
    LessGreater, // < >
    Sum,         // +, -, +=, -=
    Product,     // *, /, *=, /=
    Prefix,      // --variable
    Call,        // function()
    Index,       // array[someIndex]
}

impl Precedence {
    pub fn token_to_precedence(token: &TokenType) -> Option<Self> {
        match token {
            TokenType::Assign => Some(Precedence::Assign),

            TokenType::Eq => Some(Precedence::Equals),
            TokenType::NotEq => Some(Precedence::Equals),

            TokenType::Lt => Some(Precedence::LessGreater),
            TokenType::Gt => Some(Precedence::LessGreater),

            TokenType::Plus => Some(Precedence::Sum),
            TokenType::Minus => Some(Precedence::Sum),
            TokenType::AssignAdd => Some(Precedence::Sum),
            TokenType::AssignSub => Some(Precedence::Sum),

            TokenType::Slash => Some(Precedence::Product),
            TokenType::Asterisk => Some(Precedence::Product),
            TokenType::AssignDiv => Some(Precedence::Product),
            TokenType::AssignMul => Some(Precedence::Product),

            TokenType::Lparen => Some(Precedence::Call),
            TokenType::Lbracket => Some(Precedence::Index),
            _ => None,
        }
    }
}
