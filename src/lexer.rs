use crate::token::{Token, TokenType};

struct Lexer {
    input: String,
    position: u64,      // current position in input (points to current char)
    read_position: u64, // current reading position in input (after current char)
    ch: char,           // current char
}

impl Lexer {
    const ASCII_NUL: char = '\0';

    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: Self::ASCII_NUL,
        };

        // read next character at the start
        lexer.read_char();

        lexer
    }

    /// Reads next token in input
    fn next_token(&mut self) -> Token {
        let mut token: Token = Token {
            token_type: TokenType::EOF,
            literal: Self::ASCII_NUL.to_string(),
        };

        self.skip_whitespace();

        match self.ch {
            '(' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::LPAREN,
                }
            }
            ')' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::RPAREN,
                }
            }
            '{' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::LBRACE,
                }
            }
            '}' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::RBRACE,
                }
            }
            ';' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::SEMICOLON,
                }
            }
            ',' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::COMMA,
                }
            }
            '+' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::PLUS,
                }
            }
            '-' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::MINUS,
                }
            }
            '*' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::ASTERISK,
                }
            }
            '/' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::SLASH,
                }
            }
            '<' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::LT,
                }
            }
            '>' => {
                token = Token {
                    literal: self.ch.to_string(),
                    token_type: TokenType::GT,
                }
            }
            '=' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: "==".to_string(),
                        token_type: TokenType::EQ,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: self.ch.to_string(),
                        token_type: TokenType::ASSIGN,
                    }
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: "!=".to_string(),
                        token_type: TokenType::NOT_EQ,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: self.ch.to_string(),
                        token_type: TokenType::BANG,
                    }
                }
            }
            Self::ASCII_NUL => {
                token = Token {
                    literal: "".to_string(),
                    token_type: TokenType::EOF,
                }
            }
            _ => {
                if self.is_letter(self.ch) {
                    let literal = self.read_identififer();
                    let token_type = Token::check_ident(&literal);

                    return Token {
                        literal,
                        token_type,
                    };
                } else if self.is_digit(self.ch) {
                    let token_type = TokenType::INT;
                    let literal = self.read_number();

                    return Token {
                        literal,
                        token_type,
                    };
                } else {
                    token = Token {
                        literal: self.ch.to_string(),
                        token_type: TokenType::ILLEGAL,
                    }
                }
            }
        };

        self.read_char();

        token
    }

    /// Reads next character in input
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as u64 {
            self.ch = Self::ASCII_NUL
        } else {
            // TODO: avoid O(N) lookup of char in input
            self.ch = self.input.chars().nth(self.read_position as usize).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_numeric()
    }

    fn read_identififer(&mut self) -> String {
        let position = self.position;

        while self.is_letter(self.ch) {
            self.read_char();
        }

        return self.input[position as usize..self.position as usize].to_string();
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while self.is_digit(self.ch) {
            self.read_char();
        }

        return self.input[position as usize..self.position as usize].to_string();
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() as u64 {
            Self::ASCII_NUL
        } else {
            self.input.chars().nth(self.read_position as usize).unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = r"
            let five = 5;
            let ten = 10;

            let add = function(x, y) {
                x + y;
            };

            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        "
        .to_string();

        let tests = vec![
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "function"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::INT, "10"),
            (TokenType::EQ, "=="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "10"),
            (TokenType::NOT_EQ, "!="),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token = lexer.next_token();

            assert_eq!(
                test.0, token.token_type,
                "token type wrong: expected={:?}, got={:?}",
                test.0, token.token_type
            );

            assert_eq!(
                test.1, token.literal,
                "token literal wrong: expected={:?}, got={:?}",
                test.1, token.literal
            )
        }
    }
}
