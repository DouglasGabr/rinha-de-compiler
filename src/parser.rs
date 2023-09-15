use std::{iter::Peekable, sync::Arc};

use thiserror::Error;

use crate::{
    common::Location,
    lexer::{LexerError, Token, TokenType},
};

#[derive(Debug, Clone)]
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

trait OpPrecedence {
    fn precedence(&self) -> u8;
}

impl OpPrecedence for BinaryOp {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Add => 3,
            BinaryOp::Sub => 3,
            BinaryOp::Mul => 4,
            BinaryOp::Div => 4,
            BinaryOp::Rem => 4,
            BinaryOp::Eq => 2,
            BinaryOp::Neq => 2,
            BinaryOp::Lt => 2,
            BinaryOp::Gt => 2,
            BinaryOp::Lte => 2,
            BinaryOp::Gte => 2,
            BinaryOp::And => 1,
            BinaryOp::Or => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub text: Arc<str>,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Term>,
    pub op: BinaryOp,
    pub rhs: Box<Term>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Term>,
    pub arguments: Vec<Term>,
}

#[derive(Debug, Clone)]
pub enum TermType {
    Int {
        value: i32,
    },
    Str {
        value: Arc<str>,
    },
    Call(Call),
    Binary(Binary),
    Function {
        value: Box<Term>,
        parameters: Vec<Parameter>,
    },
    Let {
        name: Parameter,
        value: Box<Term>,
        next: Option<Box<Term>>,
    },
    If {
        condition: Box<Term>,
        then: Box<Term>,
        otherwise: Box<Term>,
    },
    Print {
        value: Box<Term>,
    },
    Tuple {
        first: Box<Term>,
        second: Box<Term>,
    },
    First {
        value: Box<Term>,
    },
    Second {
        value: Box<Term>,
    },
    Bool {
        value: bool,
    },
    Var {
        text: Arc<str>,
    },
}

#[derive(Debug, Clone)]
pub struct Term {
    pub value: TermType,
    pub location: Location,
}

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub expression: Term,
    pub location: Location,
}

struct Parser<T: Iterator<Item = Result<Token, LexerError>>> {
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

impl<T: Iterator<Item = Result<Token, LexerError>>> Parser<T> {
    fn parse_file(&mut self) -> Result<File, ParserError> {
        let term = self.parse_term()?;
        let location = term.location.clone();
        return Ok(File {
            name: location.filename.to_string(),
            expression: term,
            location,
        });
    }

    fn parse_expression(&mut self, primary: bool) -> Result<Term, ParserError> {
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
                                text: func_name.into(),
                            },
                            location,
                        };
                        let lhs = self.try_parse_call(term)?;
                        if primary {
                            return Ok(lhs);
                        } else {
                            return self.try_parse_binary(lhs);
                        }
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
                        return Ok(Term {
                            value: TermType::Tuple {
                                first: Box::new(first),
                                second: Box::new(second),
                            },
                            location: location.with_end(close.location.end),
                        });
                    }
                    None => first,
                };

                let term = if primary {
                    lhs
                } else {
                    self.try_parse_binary(lhs)?
                };
                self.expect_token(TokenType::CloseParen)?;
                return Ok(term);
            }
            TokenType::StringLiteral { value } => {
                let location = token.location.clone();
                let value = value.clone();
                self.tokens.next();
                let lhs = Term {
                    value: TermType::Str { value },
                    location,
                };
                if primary {
                    return Ok(lhs);
                } else {
                    return self.try_parse_binary(lhs);
                }
            }
            TokenType::IntLiteral { value } => {
                let location = token.location.clone();
                let value = *value;
                self.tokens.next();
                let lhs = Term {
                    value: TermType::Int { value },
                    location,
                };
                if primary {
                    return Ok(lhs);
                } else {
                    return self.try_parse_binary(lhs);
                }
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

    fn parse_term(&mut self) -> Result<Term, ParserError> {
        return self.parse_expression(false);
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParserError> {
        let token = self.tokens.next().ok_or(ParserError::UnexpectedEOF)??;
        match token.value {
            TokenType::Identifier { name } => Ok(Parameter {
                text: name.into(),
                location: token.location,
            }),
            unexpected => Err(ParserError::UnexpectedToken {
                got: unexpected,
                expected: TokenType::Identifier { name: "".into() },
                location: token.location.start,
            }),
        }
    }

    fn peek_op(&mut self) -> Result<Option<BinaryOp>, ParserError> {
        match self.tokens.peek() {
            Some(Ok(token)) => match token.value {
                TokenType::Plus => Ok(Some(BinaryOp::Add)),
                TokenType::Minus => Ok(Some(BinaryOp::Sub)),
                TokenType::Star => Ok(Some(BinaryOp::Mul)),
                TokenType::Slash => Ok(Some(BinaryOp::Div)),
                TokenType::Percent => Ok(Some(BinaryOp::Rem)),
                TokenType::DoubleEqual => Ok(Some(BinaryOp::Eq)),
                TokenType::NotEqual => Ok(Some(BinaryOp::Neq)),
                TokenType::LessThan => Ok(Some(BinaryOp::Lt)),
                TokenType::GreaterThan => Ok(Some(BinaryOp::Gt)),
                TokenType::LessOrEqualThan => Ok(Some(BinaryOp::Lte)),
                TokenType::GreaterOrEqualThan => Ok(Some(BinaryOp::Gte)),
                TokenType::And => Ok(Some(BinaryOp::And)),
                TokenType::Or => Ok(Some(BinaryOp::Or)),
                _ => Ok(None),
            },
            Some(Err(e)) => Err(ParserError::from(*e)),
            _ => Ok(None),
        }
    }

    fn try_parse_binary(&mut self, lhs: Term) -> Result<Term, ParserError> {
        fn try_parse_binary_inner<T: Iterator<Item = Result<Token, LexerError>>>(
            parser: &mut Parser<T>,
            mut lhs: Term,
            min_precedence: u8,
        ) -> Result<Term, ParserError> {
            while let Some(op) = parser.peek_op()? {
                if op.precedence() < min_precedence {
                    break;
                }
                parser.tokens.next();
                let mut rhs = parser.parse_expression(true)?;
                while let Some(next_op) = parser.peek_op()? {
                    if next_op.precedence() <= op.precedence() {
                        break;
                    }
                    rhs = try_parse_binary_inner(parser, rhs, op.precedence() + 1)?;
                }
                let location = lhs.location.with_end(rhs.location.end);
                lhs = Term {
                    value: TermType::Binary(Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    }),
                    location,
                };
            }
            return Ok(lhs);
        }
        return try_parse_binary_inner(self, lhs, 0);
    }

    fn try_parse_call(&mut self, callee: Term) -> Result<Term, ParserError> {
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

    fn expect_token(&mut self, expected: TokenType) -> Result<Token, ParserError> {
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

    fn optional_expect_token(&mut self, expected: TokenType) -> Result<Option<Token>, ParserError> {
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

pub fn parse_file<T: Iterator<Item = Result<Token, LexerError>>>(
    tokens: T,
) -> Result<File, ParserError> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
    };
    return parser.parse_file();
}
