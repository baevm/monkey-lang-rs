use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: u64,      // current position in input (points to current char)
    read_position: u64, // current reading position in input (after current char)
    ch: char,           // current char
    pub line: usize,    // current line
    pub col: usize,     // current column
}

impl Lexer {
    const ASCII_NUL: char = '\0';

    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: Self::ASCII_NUL,
            line: 1,
            col: 0,
        };

        // read next character at the start
        lexer.read_char();

        lexer
    }

    /// Reads next token in input
    pub fn next_token(&mut self) -> Token {
        let mut token: Token = Token {
            token_type: TokenType::Eof,
            literal: Self::ASCII_NUL.to_string(),
        };

        self.skip_whitespace();

        match self.ch {
            '(' => {
                token = Token {
                    literal: TokenType::Lparen.to_string(),
                    token_type: TokenType::Lparen,
                }
            }
            ')' => {
                token = Token {
                    literal: TokenType::Rparen.to_string(),
                    token_type: TokenType::Rparen,
                }
            }
            '{' => {
                token = Token {
                    literal: TokenType::Lbrace.to_string(),
                    token_type: TokenType::Lbrace,
                }
            }
            '}' => {
                token = Token {
                    literal: TokenType::Rbrace.to_string(),
                    token_type: TokenType::Rbrace,
                }
            }
            ';' => {
                token = Token {
                    literal: TokenType::Semicolon.to_string(),
                    token_type: TokenType::Semicolon,
                }
            }
            ':' => {
                token = Token {
                    literal: TokenType::Colon.to_string(),
                    token_type: TokenType::Colon,
                }
            }
            ',' => {
                token = Token {
                    literal: TokenType::Comma.to_string(),
                    token_type: TokenType::Comma,
                }
            }
            '+' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::AssignAdd.to_string(),
                        token_type: TokenType::AssignAdd,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Plus.to_string(),
                        token_type: TokenType::Plus,
                    }
                }
            }
            '-' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::AssignSub.to_string(),
                        token_type: TokenType::AssignSub,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Minus.to_string(),
                        token_type: TokenType::Minus,
                    }
                }
            }
            '*' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::AssignMul.to_string(),
                        token_type: TokenType::AssignMul,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Asterisk.to_string(),
                        token_type: TokenType::Asterisk,
                    }
                }
            }
            '/' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::AssignDiv.to_string(),
                        token_type: TokenType::AssignDiv,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Slash.to_string(),
                        token_type: TokenType::Slash,
                    }
                }
            }
            '<' => {
                token = Token {
                    literal: TokenType::Lt.to_string(),
                    token_type: TokenType::Lt,
                }
            }
            '>' => {
                token = Token {
                    literal: TokenType::Gt.to_string(),
                    token_type: TokenType::Gt,
                }
            }
            '=' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::Eq.to_string(),
                        token_type: TokenType::Eq,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Assign.to_string(),
                        token_type: TokenType::Assign,
                    }
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    token = Token {
                        literal: TokenType::NotEq.to_string(),
                        token_type: TokenType::NotEq,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        literal: TokenType::Bang.to_string(),
                        token_type: TokenType::Bang,
                    }
                }
            }
            '"' => {
                token = Token {
                    literal: self.read_string(),
                    token_type: TokenType::String,
                };
            }
            '[' => {
                token = Token {
                    literal: TokenType::Lbracket.to_string(),
                    token_type: TokenType::Lbracket,
                };
            }
            ']' => {
                token = Token {
                    literal: TokenType::Rbracket.to_string(),
                    token_type: TokenType::Rbracket,
                };
            }
            Self::ASCII_NUL => {
                token = Token {
                    literal: TokenType::Eof.to_string(),
                    token_type: TokenType::Eof,
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
                    let token_type = TokenType::Int;
                    let literal = self.read_number();

                    return Token {
                        literal,
                        token_type,
                    };
                } else {
                    token = Token {
                        literal: TokenType::Illegal.to_string(),
                        token_type: TokenType::Illegal,
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
        self.col += 1;
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

        self.input[position as usize..self.position as usize].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while self.is_digit(self.ch) {
            self.read_char();
        }

        self.input[position as usize..self.position as usize].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            if self.ch == '\n' {
                self.line += 1;
                self.col = 0;
            }

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

    fn read_string(&mut self) -> String {
        let position = self.position + 1; // skip " quote

        loop {
            self.read_char();

            if self.ch == '"' || self.ch == Self::ASCII_NUL {
                break;
            }
        }

        self.input[position as usize..self.position as usize].to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "
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

            \"randomString\";
            [1, 2];
            
            {\"foo\": \"bar\"};

            a += 1;
            a -= 1;
            a *= 1;
            a /= 1;
            a = 1;

            for(let i = 0; i < 10; i += 1) {}
        "
        .to_string();

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "function"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::Lparen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "randomString"),
            (TokenType::Semicolon, ";"),
            (TokenType::Lbracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::Rbracket, "]"),
            (TokenType::Semicolon, ";"),
            (TokenType::Lbrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::Rbrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "a"),
            (TokenType::AssignAdd, "+="),
            (TokenType::Int, "1"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "a"),
            (TokenType::AssignSub, "-="),
            (TokenType::Int, "1"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "a"),
            (TokenType::AssignMul, "*="),
            (TokenType::Int, "1"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "a"),
            (TokenType::AssignDiv, "/="),
            (TokenType::Int, "1"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "a"),
            (TokenType::Assign, "="),
            (TokenType::Int, "1"),
            (TokenType::Semicolon, ";"),
            (TokenType::For, "for"),
            (TokenType::Lparen, "("),
            (TokenType::Let, "let"),
            (TokenType::Ident, "i"),
            (TokenType::Assign, "="),
            (TokenType::Int, "0"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "i"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Ident, "i"),
            (TokenType::AssignAdd, "+="),
            (TokenType::Int, "1"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Rbrace, "}"),
            (TokenType::Eof, "EOF"),
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

    #[test]
    fn test_line_column_counter() {
        let input = r#"let a = 250;
let b = 440;"#
            .to_string();

        let expected = vec![
            // token, line, col
            (TokenType::Let, 1, 4),
            (TokenType::Ident, 1, 6),
            (TokenType::Assign, 1, 8),
            (TokenType::Int, 1, 12),
            (TokenType::Semicolon, 1, 13),
            (TokenType::Let, 2, 4),
            (TokenType::Ident, 2, 6),
            (TokenType::Assign, 2, 8),
            (TokenType::Int, 2, 12),
            (TokenType::Semicolon, 2, 13),
            (TokenType::Eof, 2, 14),
        ];

        let mut lexer = Lexer::new(input);

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(
                expect.0, token.token_type,
                "token type wrong: expected={:?}, got={:?}",
                expect.0, token.token_type
            );
            assert_eq!(
                expect.1, lexer.line,
                "line wrong for literal '{}': expected={}, got={}",
                token.literal, expect.1, lexer.line
            );
            assert_eq!(
                expect.2, lexer.col,
                "col wrong for literal '{}': expected={}, got={}",
                token.literal, expect.2, lexer.col
            );
        }
    }
}
