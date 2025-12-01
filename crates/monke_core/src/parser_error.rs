use crate::token::TokenType;

#[derive(Debug, Clone)]
pub struct InfoPosition {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<TokenType>,
        got: TokenType,
        info: InfoPosition,
    },
    MissingPrefixParseFn(TokenType),
    CouldNotParseInteger(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                got,
                info,
            } => match expected {
                Some(with_expected) => write!(
                    f,
                    "SyntaxError(Line: {}, Column: {}): Expected next token to be '{}', got '{}' instead",
                    info.line, info.column, with_expected, got
                ),
                None => write!(
                    f,
                    "SyntaxError(Line: {}, Column: {}): Unexpected token: '{}'",
                    info.line, info.column, got
                ),
            },
            ParseError::MissingPrefixParseFn(token_type) => {
                write!(f, "No prefix parse function found for: '{:?}'", token_type)
            }
            ParseError::CouldNotParseInteger(literal) => {
                write!(f, "Could not parse '{}' as integer", literal)
            }
        }
    }
}
