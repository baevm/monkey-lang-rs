use crate::token::Kind;

#[derive(Debug, Clone)]
pub struct InfoPosition {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<Kind>,
        got: Kind,
        info: InfoPosition,
    },
    MissingPrefixParseFn(Kind),
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
            ParseError::MissingPrefixParseFn(kind) => {
                write!(f, "No prefix parse function found for: '{:?}'", kind)
            }
            ParseError::CouldNotParseInteger(literal) => {
                write!(f, "Could not parse '{}' as integer", literal)
            }
        }
    }
}
