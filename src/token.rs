#[derive(PartialEq, Debug)]
pub enum TokenType {
    ILLEGAL, // unknown token
    EOF,     // end of file

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,

    // Delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn check_ident(ident: &str) -> TokenType {
        match ident {
            "function" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            _ => TokenType::IDENT,
        }
    }
}
