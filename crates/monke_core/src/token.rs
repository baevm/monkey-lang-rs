use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
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

impl TokenType {
    pub fn to_str(&self) -> &'static str {
        match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",
            TokenType::String => "STRING",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Slash => "/",
            TokenType::Asterisk => "*",
            TokenType::AssignAdd => "+=",
            TokenType::AssignSub => "-=",
            TokenType::AssignMul => "*=",
            TokenType::AssignDiv => "/=",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Colon => ":",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Lbracket => "[",
            TokenType::Rbracket => "]",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::For => "FOR",
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Rc<str>,
}

impl Token {
    pub fn new() -> Self {
        Token {
            token_type: TokenType::Eof,
            literal: Rc::from(""),
        }
    }

    pub fn check_ident(ident: &str) -> TokenType {
        match ident {
            "function" => TokenType::Function,
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "for" => TokenType::For,
            _ => TokenType::Ident,
        }
    }
}
