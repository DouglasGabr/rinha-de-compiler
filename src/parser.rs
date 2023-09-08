use std::iter::Peekable;

use thiserror::Error;

use crate::{
    common::Location,
    lexer::{LexerError, Token, TokenType},
};

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

#[derive(Debug)]
pub struct Parameter<'a> {
    text: String,
    location: Location<'a>,
}

#[derive(Debug)]
pub enum TermType<'a> {
    Int {
        value: i32,
    },
    Str {
        value: String,
    },
    Call {
        callee: Box<Term<'a>>,
        arguments: Vec<Term<'a>>,
    },
    Binary {
        lhs: Box<Term<'a>>,
        op: BinaryOp,
        rhs: Box<Term<'a>>,
    },
    Function {
        value: Box<Term<'a>>,
        parameters: Vec<Parameter<'a>>,
    },
    Let {
        name: Parameter<'a>,
        value: Box<Term<'a>>,
        next: Option<Box<Term<'a>>>,
    },
    If {
        condition: Box<Term<'a>>,
        then: Box<Term<'a>>,
        otherwise: Box<Term<'a>>,
    },
    Print {
        value: Box<Term<'a>>,
    },
    Tuple {
        first: Box<Term<'a>>,
        second: Box<Term<'a>>,
    },
    First {
        value: Box<Term<'a>>,
    },
    Second {
        value: Box<Term<'a>>,
    },
    Bool {
        value: bool,
    },
    Var {
        text: String,
    },
}

#[derive(Debug)]
struct Term<'a> {
    value: TermType<'a>,
    location: Location<'a>,
}

#[derive(Debug)]
pub struct File<'a> {
    name: String,
    expression: Term<'a>,
    location: Location<'a>,
}

struct Parser<'a, T: Iterator<Item = Result<Token<'a>, LexerError>>> {
    tokens: Peekable<T>,
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Lexer error")]
    LexerError(#[from] LexerError),
    #[error("Expected token {expected:?} at {location}, got {got:?}")]
    UnexpectedToken {
        expected: TokenType,
        got: TokenType,
        location: usize,
    },
}

impl<'a, T: Iterator<Item = Result<Token<'a>, LexerError>>> Parser<'a, T> {
    fn parse_file(&mut self) -> Result<File<'a>, ParserError> {
        let term = self.parse_term()?;
        let location = term.location.clone();
        return Ok(File {
            name: location.filename.to_string(),
            expression: term,
            location,
        });
    }

    fn parse_term(&mut self) -> Result<Term<'a>, ParserError> {
        let token = self
            .tokens
            .peek()
            .ok_or(ParserError::UnexpectedEOF)?
            .as_ref()
            .map_err(|e| ParserError::from(*e))?;
        match &token.value {
            TokenType::Let => {
                let location = token.location.clone();
                self.tokens.next().unwrap().unwrap();
                let name = self.parse_parameter()?;
                self.expect_token(TokenType::Equal)?;
                let value = self.parse_term()?;
                let semicolon = self.optional_expect_token(TokenType::Semicolon)?;
                let next = if semicolon.is_some() {
                    Some(Box::new(self.parse_term()?))
                } else {
                    None
                };
                let location = location.with_end(
                    next.as_ref()
                        .map(|x| x.location.end)
                        .unwrap_or(value.location.end),
                );
                return Ok(Term {
                    value: TermType::Let {
                        name,
                        value: Box::new(value),
                        next,
                    },
                    location,
                });
            }
            TokenType::BoolLiteral { value } => {
                let location = token.location.clone();
                let value = *value;
                self.tokens.next().unwrap().unwrap();
                return Ok(Term {
                    value: TermType::Bool { value },
                    location,
                });
            }
            unexpected => todo!("Unexpected token {:?}", unexpected),
        }
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'a>, ParserError> {
        let token = self.tokens.next().ok_or(ParserError::UnexpectedEOF)??;
        match token.value {
            TokenType::Identifier { name } => Ok(Parameter {
                text: name.to_string(),
                location: token.location,
            }),
            unexpected => Err(ParserError::UnexpectedToken {
                got: unexpected,
                expected: TokenType::Identifier {
                    name: "".to_string(),
                },
                location: token.location.start,
            }),
        }
    }

    fn expect_token(&mut self, expected: TokenType) -> Result<Token<'a>, ParserError> {
        let token = self.tokens.next().ok_or(ParserError::UnexpectedEOF)??;
        if token.value == expected {
            return Ok(token);
        } else {
            return Err(ParserError::UnexpectedToken {
                got: token.value,
                expected,
                location: token.location.start,
            });
        }
    }

    fn optional_expect_token(
        &mut self,
        expected: TokenType,
    ) -> Result<Option<Token<'a>>, ParserError> {
        let Some(token) = self.tokens.peek() else {
            return Ok(None);
        };
        let token = token.as_ref().map_err(|e| ParserError::from(*e))?;
        if token.value == expected {
            let token = self.tokens.next().unwrap().unwrap();
            return Ok(Some(token));
        } else {
            return Ok(None);
        }
    }
}

pub fn parse_file<'a, T: Iterator<Item = Result<Token<'a>, LexerError>>>(
    tokens: T,
) -> Result<File<'a>, ParserError> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
    };
    return parser.parse_file();
}
