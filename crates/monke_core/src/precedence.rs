use crate::token::Kind;

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
    pub fn token_to_precedence(token: &Kind) -> Option<Self> {
        match token {
            Kind::Assign => Some(Precedence::Assign),

            Kind::Eq => Some(Precedence::Equals),
            Kind::NotEq => Some(Precedence::Equals),

            Kind::Lt => Some(Precedence::LessGreater),
            Kind::Gt => Some(Precedence::LessGreater),

            Kind::Plus => Some(Precedence::Sum),
            Kind::Minus => Some(Precedence::Sum),
            Kind::AssignAdd => Some(Precedence::Sum),
            Kind::AssignSub => Some(Precedence::Sum),

            Kind::Slash => Some(Precedence::Product),
            Kind::Asterisk => Some(Precedence::Product),
            Kind::AssignDiv => Some(Precedence::Product),
            Kind::AssignMul => Some(Precedence::Product),

            Kind::Lparen => Some(Precedence::Call),
            Kind::Lbracket => Some(Precedence::Index),
            _ => None,
        }
    }
}
