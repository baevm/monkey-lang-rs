#[derive(PartialEq, Debug)]
pub enum TokenType {
    Illegal, // unknown token
    Eof,     // end of file

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Slash,
    Asterisk,

    Lt,
    Gt,
    Eq,
    NotEq,

    // Delimeters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token: {:?}. Literal: {}", self.token_type, self.literal)
    }
}

impl Token {
    pub fn check_ident(ident: &str) -> TokenType {
        match ident {
            "function" => TokenType::Function,
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Ident,
        }
    }
}
