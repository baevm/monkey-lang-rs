use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Kind {
    Illegal, // unknown token
    Eof,     // end of file

    // Identifiers + literals
    Ident,
    Int,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Slash,
    Asterisk,

    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,

    Lt,
    Gt,
    Eq,
    NotEq,

    // Delimeters
    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
    For,
}

impl Kind {
    pub fn to_str(&self) -> &'static str {
        match self {
            Kind::Illegal => "ILLEGAL",
            Kind::Eof => "EOF",
            Kind::Ident => "IDENT",
            Kind::Int => "INT",
            Kind::String => "STRING",
            Kind::Assign => "=",
            Kind::Plus => "+",
            Kind::Minus => "-",
            Kind::Bang => "!",
            Kind::Slash => "/",
            Kind::Asterisk => "*",
            Kind::AssignAdd => "+=",
            Kind::AssignSub => "-=",
            Kind::AssignMul => "*=",
            Kind::AssignDiv => "/=",
            Kind::Lt => "<",
            Kind::Gt => ">",
            Kind::Eq => "==",
            Kind::NotEq => "!=",
            Kind::Comma => ",",
            Kind::Semicolon => ";",
            Kind::Colon => ":",
            Kind::Lparen => "(",
            Kind::Rparen => ")",
            Kind::Lbrace => "{",
            Kind::Rbrace => "}",
            Kind::Lbracket => "[",
            Kind::Rbracket => "]",
            Kind::Function => "FUNCTION",
            Kind::Let => "LET",
            Kind::If => "IF",
            Kind::Else => "ELSE",
            Kind::Return => "RETURN",
            Kind::True => "TRUE",
            Kind::False => "FALSE",
            Kind::For => "FOR",
        }
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Kind,
    pub value: Rc<str>,
}

impl Token {
    pub fn new() -> Self {
        Token {
            kind: Kind::Eof,
            value: Rc::from(""),
        }
    }

    pub fn check_ident(ident: &str) -> Kind {
        match ident {
            "function" => Kind::Function,
            "let" => Kind::Let,
            "if" => Kind::If,
            "else" => Kind::Else,
            "return" => Kind::Return,
            "true" => Kind::True,
            "false" => Kind::False,
            "for" => Kind::For,
            _ => Kind::Ident,
        }
    }
}
