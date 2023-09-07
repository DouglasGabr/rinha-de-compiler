use std::{borrow::BorrowMut, iter::Peekable};

use crate::{
    common::Location,
    lexer::{Token, TokenType},
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

struct Parser<'a, T: Iterator<Item = Token<'a>>> {
    tokens: Peekable<T>,
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    fn parse(&mut self) -> File<'a> {
        let term = self.parse_term().expect("File must have at least one term");
        let location = term.location.clone();
        return File {
            name: location.filename.to_string(),
            expression: term,
            location,
        };
    }

    fn expect_optional(&mut self, expected: TokenType) -> Option<Token<'a>> {
        let token = self.tokens.peek()?;
        if token.value == expected {
            return self.tokens.next();
        }
        return None;
    }

    fn expect(&mut self, expected: TokenType) -> Token<'a> {
        let token = self
            .tokens
            .next()
            .expect(&format!("token {:?} expected, none found", expected));
        if token.value != expected {
            panic!("expected {:?}, got {:?}", expected, token.value);
        }
        return token;
    }

    fn parse_parameter(&mut self) -> Parameter<'a> {
        let token = self.tokens.next().expect("parameter expected");
        match token.value {
            TokenType::Identifier { name } => Parameter {
                text: name,
                location: token.location,
            },
            unexpected => panic!("unexpected token {:?}", unexpected),
        }
    }

    fn parse_term(&mut self) -> Option<Term<'a>> {
        let token = self.tokens.next()?;
        match token.value {
            TokenType::Let => {
                let name = self.parse_parameter();
                self.expect(TokenType::Equal);
                let value = self.parse_term().expect("value expected");
                let semi = self.expect_optional(TokenType::Semicolon);
                let next = self.parse_term();
                let location = token
                    .location
                    .with_end(semi.map(|s| s.location.end).unwrap_or(value.location.end));
                return Some(Term {
                    value: TermType::Let {
                        name,
                        value: Box::new(value),
                        next: next.map(Box::new),
                    },
                    location,
                });
            }
            TokenType::Fn => {
                self.expect(TokenType::OpenParen);
                let mut parameters = Vec::new();
                while let Some(token) = self.tokens.peek() {
                    if token.value == TokenType::CloseParen {
                        break;
                    }
                    parameters.push(self.parse_parameter());
                    if let Some(token) = self.tokens.peek() {
                        if token.value == TokenType::Comma {
                            self.tokens.next();
                        }
                    }
                }
                self.tokens.next();
                self.expect(TokenType::ArrowRight);
                self.expect(TokenType::OpenCurly);
                let value = self.parse_term().expect("value expected");
                let close = self.expect(TokenType::CloseCurly);
                return Some(Term {
                    value: TermType::Function {
                        value: Box::new(value),
                        parameters,
                    },
                    location: token.location.with_end(close.location.end),
                });
            }
            TokenType::If => todo!(),
            TokenType::Else => todo!(),
            TokenType::Identifier { name } => todo!(),
            TokenType::Equal => todo!(),
            TokenType::DoubleEqual => todo!(),
            TokenType::NotEqual => todo!(),
            TokenType::ArrowRight => todo!(),
            TokenType::OpenCurly => todo!(),
            TokenType::CloseCurly => todo!(),
            TokenType::OpenParen => todo!(),
            TokenType::CloseParen => todo!(),
            TokenType::IntLiteral { value } => todo!(),
            TokenType::StringLiteral { value } => todo!(),
            TokenType::LessThan => todo!(),
            TokenType::LessOrEqualThan => todo!(),
            TokenType::GreaterThan => todo!(),
            TokenType::GreaterOrEqualThan => todo!(),
            TokenType::Plus => todo!(),
            TokenType::Minus => todo!(),
            TokenType::Star => todo!(),
            TokenType::Semicolon => todo!(),
            TokenType::BoolLiteral { value } => todo!(),
            TokenType::Comma => todo!(),
        }
    }
}

pub fn parse_file<'a, T: Iterator<Item = Token<'a>>>(tokens: T) -> File<'a> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
    };
    return parser.parse();
}
