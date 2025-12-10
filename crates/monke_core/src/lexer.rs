use std::{rc::Rc, str::Chars};

use crate::token::{Kind, Token};

pub struct Lexer<'a> {
    input: &'a str,
    ch: char,        // current char
    pub line: usize, // current line
    pub col: usize,  // current column
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    const ASCII_NULL: char = '\0';

    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input: &input,
            chars: input.chars(),
            ch: Self::ASCII_NULL,
            line: 1,
            col: 0,
        };

        // read next character at the start
        lexer.read_char();

        lexer
    }

    /// Reads next token in input
    #[allow(unused_assignments)]
    pub fn next_token(&mut self) -> Token {
        let mut token: Token = Token {
            kind: Kind::Eof,
            value: Rc::from(Self::ASCII_NULL.to_string()),
        };

        self.skip_whitespace();

        match self.ch {
            '(' => {
                token = Token {
                    value: Rc::from(Kind::Lparen.to_str()),
                    kind: Kind::Lparen,
                }
            }
            ')' => {
                token = Token {
                    value: Rc::from(Kind::Rparen.to_str()),
                    kind: Kind::Rparen,
                }
            }
            '{' => {
                token = Token {
                    value: Rc::from(Kind::Lbrace.to_str()),
                    kind: Kind::Lbrace,
                }
            }
            '}' => {
                token = Token {
                    value: Rc::from(Kind::Rbrace.to_str()),
                    kind: Kind::Rbrace,
                }
            }
            ';' => {
                token = Token {
                    value: Rc::from(Kind::Semicolon.to_str()),
                    kind: Kind::Semicolon,
                }
            }
            ':' => {
                token = Token {
                    value: Rc::from(Kind::Colon.to_str()),
                    kind: Kind::Colon,
                }
            }
            ',' => {
                token = Token {
                    value: Rc::from(Kind::Comma.to_str()),
                    kind: Kind::Comma,
                }
            }
            '+' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::AssignAdd.to_str()),
                        kind: Kind::AssignAdd,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Plus.to_str()),
                        kind: Kind::Plus,
                    }
                }
            }
            '-' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::AssignSub.to_str()),
                        kind: Kind::AssignSub,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Minus.to_str()),
                        kind: Kind::Minus,
                    }
                }
            }
            '*' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::AssignMul.to_str()),
                        kind: Kind::AssignMul,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Asterisk.to_str()),
                        kind: Kind::Asterisk,
                    }
                }
            }
            '/' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::AssignDiv.to_str()),
                        kind: Kind::AssignDiv,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Slash.to_str()),
                        kind: Kind::Slash,
                    }
                }
            }
            '<' => {
                token = Token {
                    value: Rc::from(Kind::Lt.to_str()),
                    kind: Kind::Lt,
                }
            }
            '>' => {
                token = Token {
                    value: Rc::from(Kind::Gt.to_str()),
                    kind: Kind::Gt,
                }
            }
            '=' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::Eq.to_str()),
                        kind: Kind::Eq,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Assign.to_str()),
                        kind: Kind::Assign,
                    }
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    token = Token {
                        value: Rc::from(Kind::NotEq.to_str()),
                        kind: Kind::NotEq,
                    };

                    self.read_char();
                } else {
                    token = Token {
                        value: Rc::from(Kind::Bang.to_str()),
                        kind: Kind::Bang,
                    }
                }
            }
            '"' => {
                token = Token {
                    value: self.read_string(),
                    kind: Kind::String,
                };
            }
            '[' => {
                token = Token {
                    value: Rc::from(Kind::Lbracket.to_str()),
                    kind: Kind::Lbracket,
                };
            }
            ']' => {
                token = Token {
                    value: Rc::from(Kind::Rbracket.to_str()),
                    kind: Kind::Rbracket,
                };
            }
            Self::ASCII_NULL => {
                token = Token {
                    value: Rc::from(Kind::Eof.to_str()),
                    kind: Kind::Eof,
                }
            }
            _ => {
                if self.is_letter(self.ch) {
                    let value = self.read_identififer();

                    let kind = Token::check_ident(&value);

                    return Token { value, kind };
                } else if self.is_digit(self.ch) {
                    let kind = Kind::Int;
                    let value = self.read_number();

                    return Token { value, kind };
                } else {
                    token = Token {
                        value: Rc::from(Kind::Illegal.to_string()),
                        kind: Kind::Illegal,
                    }
                }
            }
        };

        self.read_char();

        token
    }

    /// Reads next character in input
    fn read_char(&mut self) {
        if let Some(ch) = self.chars.next() {
            self.ch = ch;
        } else {
            self.ch = Self::ASCII_NULL;
        }

        self.col += 1;
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_numeric()
    }

    fn read_identififer(&mut self) -> Rc<str> {
        let mut result = String::new();

        while self.is_letter(self.ch) {
            result.push(self.ch);
            self.read_char();
        }

        Rc::from(result)
    }

    fn read_number(&mut self) -> Rc<str> {
        let mut result = String::new();

        while self.is_digit(self.ch) {
            result.push(self.ch);
            self.read_char();
        }

        Rc::from(result)
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
        if let Some(ch) = self.chars.clone().next() {
            ch
        } else {
            Self::ASCII_NULL
        }
    }

    fn read_string(&mut self) -> Rc<str> {
        let mut result = String::new();
        self.read_char();

        while self.ch != '"' && self.ch != Self::ASCII_NULL {
            result.push(self.ch);

            self.read_char();
        }

        Rc::from(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Kind;

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
            (Kind::Let, "let"),
            (Kind::Ident, "five"),
            (Kind::Assign, "="),
            (Kind::Int, "5"),
            (Kind::Semicolon, ";"),
            (Kind::Let, "let"),
            (Kind::Ident, "ten"),
            (Kind::Assign, "="),
            (Kind::Int, "10"),
            (Kind::Semicolon, ";"),
            (Kind::Let, "let"),
            (Kind::Ident, "add"),
            (Kind::Assign, "="),
            (Kind::Function, "function"),
            (Kind::Lparen, "("),
            (Kind::Ident, "x"),
            (Kind::Comma, ","),
            (Kind::Ident, "y"),
            (Kind::Rparen, ")"),
            (Kind::Lbrace, "{"),
            (Kind::Ident, "x"),
            (Kind::Plus, "+"),
            (Kind::Ident, "y"),
            (Kind::Semicolon, ";"),
            (Kind::Rbrace, "}"),
            (Kind::Semicolon, ";"),
            (Kind::Let, "let"),
            (Kind::Ident, "result"),
            (Kind::Assign, "="),
            (Kind::Ident, "add"),
            (Kind::Lparen, "("),
            (Kind::Ident, "five"),
            (Kind::Comma, ","),
            (Kind::Ident, "ten"),
            (Kind::Rparen, ")"),
            (Kind::Semicolon, ";"),
            (Kind::Bang, "!"),
            (Kind::Minus, "-"),
            (Kind::Slash, "/"),
            (Kind::Asterisk, "*"),
            (Kind::Int, "5"),
            (Kind::Semicolon, ";"),
            (Kind::Int, "5"),
            (Kind::Lt, "<"),
            (Kind::Int, "10"),
            (Kind::Gt, ">"),
            (Kind::Int, "5"),
            (Kind::Semicolon, ";"),
            (Kind::If, "if"),
            (Kind::Lparen, "("),
            (Kind::Int, "5"),
            (Kind::Lt, "<"),
            (Kind::Int, "10"),
            (Kind::Rparen, ")"),
            (Kind::Lbrace, "{"),
            (Kind::Return, "return"),
            (Kind::True, "true"),
            (Kind::Semicolon, ";"),
            (Kind::Rbrace, "}"),
            (Kind::Else, "else"),
            (Kind::Lbrace, "{"),
            (Kind::Return, "return"),
            (Kind::False, "false"),
            (Kind::Semicolon, ";"),
            (Kind::Rbrace, "}"),
            (Kind::Int, "10"),
            (Kind::Eq, "=="),
            (Kind::Int, "10"),
            (Kind::Semicolon, ";"),
            (Kind::Int, "10"),
            (Kind::NotEq, "!="),
            (Kind::Int, "9"),
            (Kind::Semicolon, ";"),
            (Kind::String, "randomString"),
            (Kind::Semicolon, ";"),
            (Kind::Lbracket, "["),
            (Kind::Int, "1"),
            (Kind::Comma, ","),
            (Kind::Int, "2"),
            (Kind::Rbracket, "]"),
            (Kind::Semicolon, ";"),
            (Kind::Lbrace, "{"),
            (Kind::String, "foo"),
            (Kind::Colon, ":"),
            (Kind::String, "bar"),
            (Kind::Rbrace, "}"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "a"),
            (Kind::AssignAdd, "+="),
            (Kind::Int, "1"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "a"),
            (Kind::AssignSub, "-="),
            (Kind::Int, "1"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "a"),
            (Kind::AssignMul, "*="),
            (Kind::Int, "1"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "a"),
            (Kind::AssignDiv, "/="),
            (Kind::Int, "1"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "a"),
            (Kind::Assign, "="),
            (Kind::Int, "1"),
            (Kind::Semicolon, ";"),
            (Kind::For, "for"),
            (Kind::Lparen, "("),
            (Kind::Let, "let"),
            (Kind::Ident, "i"),
            (Kind::Assign, "="),
            (Kind::Int, "0"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "i"),
            (Kind::Lt, "<"),
            (Kind::Int, "10"),
            (Kind::Semicolon, ";"),
            (Kind::Ident, "i"),
            (Kind::AssignAdd, "+="),
            (Kind::Int, "1"),
            (Kind::Rparen, ")"),
            (Kind::Lbrace, "{"),
            (Kind::Rbrace, "}"),
            (Kind::Eof, "EOF"),
        ];

        let mut lexer = Lexer::new(&input);

        for test in tests {
            let token = lexer.next_token();

            assert_eq!(
                test.0, token.kind,
                "token kind wrong: expected={:?}, got={:?}",
                test.0, token.kind
            );

            assert_eq!(
                test.1,
                token.value.as_ref(),
                "token value wrong: expected={:?}, got={:?}",
                test.1,
                token.value
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
            (Kind::Let, 1, 4),
            (Kind::Ident, 1, 6),
            (Kind::Assign, 1, 8),
            (Kind::Int, 1, 12),
            (Kind::Semicolon, 1, 13),
            (Kind::Let, 2, 4),
            (Kind::Ident, 2, 6),
            (Kind::Assign, 2, 8),
            (Kind::Int, 2, 12),
            (Kind::Semicolon, 2, 13),
            (Kind::Eof, 2, 14),
        ];

        let mut lexer = Lexer::new(&input);

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(
                expect.0, token.kind,
                "token kind wrong: expected={:?}, got={:?}",
                expect.0, token.kind
            );
            assert_eq!(
                expect.1, lexer.line,
                "line wrong for value '{}': expected={}, got={}",
                token.value, expect.1, lexer.line
            );
            assert_eq!(
                expect.2, lexer.col,
                "col wrong for value '{}': expected={}, got={}",
                token.value, expect.2, lexer.col
            );
        }
    }
}
