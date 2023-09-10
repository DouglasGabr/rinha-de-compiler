use std::iter::Peekable;

use thiserror::Error;

use crate::{
    common::{Either, Location},
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
struct Binary<'a> {
    lhs: Box<Term<'a>>,
    op: BinaryOp,
    rhs: Box<Term<'a>>,
}

#[derive(Debug)]
struct Call<'a> {
    callee: Box<Term<'a>>,
    arguments: Vec<Term<'a>>,
}

#[derive(Debug)]
pub enum TermType<'a> {
    Int {
        value: i32,
    },
    Str {
        value: String,
    },
    Call(Call<'a>),
    Binary(Binary<'a>),
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
    #[error("Expected token '{expected}' at {location}, got '{got}'")]
    UnexpectedToken {
        expected: TokenType,
        got: TokenType,
        location: usize,
    },
    #[error("Invalid token '{token}' at {location}")]
    InvalidToken { token: TokenType, location: usize },
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
                self.tokens.next();
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
                self.tokens.next();
                return Ok(Term {
                    value: TermType::Bool { value },
                    location,
                });
            }
            TokenType::If => {
                let location = token.location.clone();
                self.tokens.next();
                self.expect_token(TokenType::OpenParen)?;
                let condition = self.parse_term()?;
                self.expect_token(TokenType::CloseParen)?;
                self.expect_token(TokenType::OpenCurly)?;
                let then = self.parse_term()?;
                self.expect_token(TokenType::CloseCurly)?;
                self.expect_token(TokenType::Else)?;
                self.expect_token(TokenType::OpenCurly)?;
                let otherwise = self.parse_term()?;
                let close = self.expect_token(TokenType::CloseCurly)?;
                return Ok(Term {
                    value: TermType::If {
                        condition: Box::new(condition),
                        then: Box::new(then),
                        otherwise: Box::new(otherwise),
                    },
                    location: location.with_end(close.location.end),
                });
            }
            TokenType::Identifier { name } => {
                let name = name.clone();
                let location = token.location.clone();
                match name.as_ref() {
                    "print" => {
                        self.tokens.next();
                        self.expect_token(TokenType::OpenParen)?;
                        let value = self.parse_term()?;
                        let close = self.expect_token(TokenType::CloseParen)?;
                        return Ok(Term {
                            value: TermType::Print {
                                value: Box::new(value),
                            },
                            location: location.with_end(close.location.end),
                        });
                    }
                    "first" => {
                        self.tokens.next();
                        self.expect_token(TokenType::OpenParen)?;
                        let value = self.parse_term()?;
                        let close = self.expect_token(TokenType::CloseParen)?;
                        return Ok(Term {
                            value: TermType::First {
                                value: Box::new(value),
                            },
                            location: location.with_end(close.location.end),
                        });
                    }
                    "second" => {
                        self.tokens.next();
                        self.expect_token(TokenType::OpenParen)?;
                        let value = self.parse_term()?;
                        let close = self.expect_token(TokenType::CloseParen)?;
                        return Ok(Term {
                            value: TermType::Second {
                                value: Box::new(value),
                            },
                            location: location.with_end(close.location.end),
                        });
                    }
                    func_name => {
                        self.tokens.next();
                        let term = Term {
                            value: TermType::Var {
                                text: func_name.to_string(),
                            },
                            location,
                        };
                        let lhs = self.try_parse_call(term)?;
                        return self.try_parse_binary(lhs);
                    }
                }
            }
            TokenType::OpenParen => {
                let location = token.location.clone();
                self.tokens.next();
                let first = self.parse_term()?;
                let lhs = match self.optional_expect_token(TokenType::Comma)? {
                    Some(_) => {
                        let second = self.parse_term()?;
                        let close = self.expect_token(TokenType::CloseParen)?;
                        Term {
                            value: TermType::Tuple {
                                first: Box::new(first),
                                second: Box::new(second),
                            },
                            location: location.with_end(close.location.end),
                        }
                    }
                    None => first,
                };
                return self.try_parse_binary(lhs);
            }
            TokenType::StringLiteral { value } => {
                let location = token.location.clone();
                let value = value.clone();
                self.tokens.next();
                let lhs = Term {
                    value: TermType::Str { value },
                    location,
                };
                return self.try_parse_binary(lhs);
            }
            TokenType::IntLiteral { value } => {
                let location = token.location.clone();
                let value = *value;
                self.tokens.next();
                let lhs = Term {
                    value: TermType::Int { value },
                    location,
                };
                return self.try_parse_binary(lhs);
            }
            TokenType::Fn => {
                let location = token.location.clone();
                self.tokens.next();
                self.expect_token(TokenType::OpenParen)?;
                let mut parameters = vec![];
                while self.optional_expect_token(TokenType::CloseParen)?.is_none() {
                    parameters.push(self.parse_parameter()?);
                    if self.optional_expect_token(TokenType::Comma)?.is_none() {
                        self.expect_token(TokenType::CloseParen)?;
                        break;
                    }
                }
                self.expect_token(TokenType::ArrowRight)?;
                self.expect_token(TokenType::OpenCurly)?;
                let value = self.parse_term()?;
                let close = self.expect_token(TokenType::CloseCurly)?;
                return Ok(Term {
                    value: TermType::Function {
                        value: Box::new(value),
                        parameters,
                    },
                    location: location.with_end(close.location.end),
                });
            }
            TokenType::Else | TokenType::CloseParen => {
                let token = self.tokens.next().unwrap().unwrap();
                return Err(ParserError::InvalidToken {
                    token: token.value,
                    location: token.location.start,
                });
            }
            unexpected => todo!("token {:?}", unexpected),
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

    fn try_parse_binary(&mut self, lhs: Term<'a>) -> Result<Term<'a>, ParserError> {
        let op = match self.tokens.peek() {
            Some(Ok(token)) => match token.value {
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Sub,
                TokenType::Star => BinaryOp::Mul,
                TokenType::Slash => BinaryOp::Div,
                TokenType::Percent => BinaryOp::Rem,
                TokenType::DoubleEqual => BinaryOp::Eq,
                TokenType::NotEqual => BinaryOp::Neq,
                TokenType::LessThan => BinaryOp::Lt,
                TokenType::GreaterThan => BinaryOp::Gt,
                TokenType::LessOrEqualThan => BinaryOp::Lte,
                TokenType::GreaterOrEqualThan => BinaryOp::Gte,
                TokenType::And => BinaryOp::And,
                TokenType::Or => BinaryOp::Or,
                _ => return Ok(lhs),
            },
            Some(Err(e)) => return Err(ParserError::from(*e)),
            _ => return Ok(lhs),
        };
        self.tokens.next();
        let rhs = self.parse_term()?;
        let location = lhs.location.with_end(rhs.location.end);
        return Ok(Term {
            value: TermType::Binary(Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }),
            location,
        });
    }

    fn try_parse_call(&mut self, callee: Term<'a>) -> Result<Term<'a>, ParserError> {
        match self.tokens.peek() {
            Some(Ok(token)) => match token.value {
                TokenType::OpenParen => {
                    self.tokens.next();
                    let mut arguments = vec![];
                    let close = loop {
                        match self.optional_expect_token(TokenType::CloseParen)? {
                            Some(close) => break close,
                            None => {
                                arguments.push(self.parse_term()?);
                                if self.optional_expect_token(TokenType::Comma)?.is_none() {
                                    break self.expect_token(TokenType::CloseParen)?;
                                }
                            }
                        }
                    };
                    let location = callee.location.with_end(close.location.end);
                    return Ok(Term {
                        value: TermType::Call(Call {
                            callee: Box::new(callee),
                            arguments,
                        }),
                        location,
                    });
                }
                _ => return Ok(callee),
            },
            Some(Err(e)) => return Err(ParserError::from(*e)),
            _ => return Ok(callee),
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
