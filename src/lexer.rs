use std::{
    fmt::{Display, Formatter},
    iter::Peekable,
    sync::Arc,
};
use thiserror::Error;

use crate::common::Location;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Let,
    Fn,
    If,
    Else,
    Identifier { name: Arc<str> },
    IntLiteral { value: i32 },
    StringLiteral { value: Arc<str> },
    BoolLiteral { value: bool },
    Equal,
    DoubleEqual,
    NotEqual,
    ArrowRight,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    LessThan,
    LessOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Semicolon,
    Comma,
    Ampersand,
    Pipe,
    And,
    Or,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Let => write!(f, "let"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Identifier { name } => write!(f, "{}", name),
            TokenType::IntLiteral { value } => write!(f, "{}", value),
            TokenType::StringLiteral { value } => write!(f, "{}", value),
            TokenType::BoolLiteral { value } => write!(f, "{}", value),
            TokenType::Equal => write!(f, "="),
            TokenType::DoubleEqual => write!(f, "=="),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::ArrowRight => write!(f, "=>"),
            TokenType::OpenCurly => write!(f, "{{"),
            TokenType::CloseCurly => write!(f, "}}"),
            TokenType::OpenParen => write!(f, "("),
            TokenType::CloseParen => write!(f, ")"),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LessOrEqualThan => write!(f, "<="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::GreaterOrEqualThan => write!(f, ">="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Comma => write!(f, ","),
            TokenType::Slash => write!(f, "/"),
            TokenType::Percent => write!(f, "%"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::And => write!(f, "&&"),
            TokenType::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub value: TokenType,
    pub location: Location,
}

pub struct Lexer<T: Iterator<Item = char>> {
    filename: Arc<str>,
    inner: Peekable<T>,
    current_position: usize,
}

impl<T: Iterator<Item = char>> Lexer<T> {
    pub fn new(input: T, filename: &str) -> Self {
        Lexer {
            filename: filename.into(),
            inner: input.peekable(),
            current_position: 0,
        }
    }
}

#[derive(Error, Debug, Copy, Clone)]
pub enum LexerError {
    #[error("Expected '=' after '!' at {position}, found '{c}'")]
    UnexpectedCharAfterBang { c: char, position: usize },
    #[error("Unexpected EOF at {position}")]
    UnexpectedEOF { position: usize },
    #[error("Unknown char '{c}' at {position}")]
    UnknownChar { c: char, position: usize },
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut end = self.current_position;
        if let Some(c) = self.inner.next() {
            let value = match c {
                d if d.is_digit(10) => {
                    let mut value = String::new();
                    value.push(d);
                    while self.inner.peek().filter(|c| c.is_digit(10)).is_some() {
                        value.push(self.inner.next().unwrap());
                    }
                    end += value.len();
                    TokenType::IntLiteral {
                        value: value.parse().unwrap(),
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut name = String::new();
                    name.push(c);
                    while self
                        .inner
                        .peek()
                        .filter(|c| c.is_ascii_alphanumeric() || c == &&'_')
                        .is_some()
                    {
                        name.push(self.inner.next().unwrap());
                    }
                    end += name.len();
                    match name.as_str() {
                        "let" => TokenType::Let,
                        "fn" => TokenType::Fn,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "true" => TokenType::BoolLiteral { value: true },
                        "false" => TokenType::BoolLiteral { value: false },
                        _ => TokenType::Identifier { name: name.into() },
                    }
                }
                '"' => {
                    let mut value = String::new();
                    while self.inner.peek().filter(|c| c != &&'"').is_some() {
                        value.push(self.inner.next().unwrap());
                    }
                    self.inner.next();
                    end += value.len() + 2;
                    TokenType::StringLiteral {
                        value: value.into(),
                    }
                }
                '=' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'>') => {
                            self.inner.next();
                            end += 1;
                            TokenType::ArrowRight
                        }
                        Some(&'=') => {
                            self.inner.next();
                            end += 1;
                            TokenType::DoubleEqual
                        }
                        _ => TokenType::Equal,
                    }
                }
                '(' => {
                    end += 1;
                    TokenType::OpenParen
                }
                ')' => {
                    end += 1;
                    TokenType::CloseParen
                }
                '{' => {
                    end += 1;
                    TokenType::OpenCurly
                }
                '}' => {
                    end += 1;
                    TokenType::CloseCurly
                }
                '<' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'=') => {
                            self.inner.next();
                            end += 1;
                            TokenType::LessOrEqualThan
                        }
                        _ => TokenType::LessThan,
                    }
                }
                '>' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'=') => {
                            self.inner.next();
                            end += 1;
                            TokenType::GreaterOrEqualThan
                        }
                        _ => TokenType::GreaterThan,
                    }
                }
                '&' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'&') => {
                            self.inner.next();
                            end += 1;
                            TokenType::And
                        }
                        _ => TokenType::Ampersand,
                    }
                }
                '|' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'|') => {
                            self.inner.next();
                            end += 1;
                            TokenType::Or
                        }
                        _ => TokenType::Pipe,
                    }
                }
                '+' => {
                    end += 1;
                    TokenType::Plus
                }
                '-' => {
                    end += 1;
                    TokenType::Minus
                }
                ';' => {
                    end += 1;
                    TokenType::Semicolon
                }
                '*' => {
                    end += 1;
                    TokenType::Star
                }
                '/' => {
                    end += 1;
                    TokenType::Slash
                }
                '%' => {
                    end += 1;
                    TokenType::Percent
                }
                '!' => {
                    end += 1;
                    match self.inner.peek() {
                        Some(&'=') => {
                            self.inner.next();
                            end += 1;
                            TokenType::NotEqual
                        }
                        Some(&other) => {
                            return Some(Err(LexerError::UnexpectedCharAfterBang {
                                c: other,
                                position: end,
                            }))
                        }
                        None => return Some(Err(LexerError::UnexpectedEOF { position: end })),
                    }
                }
                ',' => {
                    end += 1;
                    TokenType::Comma
                }
                s if s.is_ascii_whitespace() => {
                    self.current_position += 1;
                    return self.next();
                }
                unknown => {
                    return Some(Err(LexerError::UnknownChar {
                        c: unknown,
                        position: end,
                    }))
                }
            };
            let token = Token {
                value,
                location: Location {
                    start: self.current_position,
                    end,
                    filename: self.filename.clone(),
                },
            };
            self.current_position = end;
            return Some(Ok(token));
        }
        return None;
    }
}

pub fn tokens<T: Iterator<Item = char>>(
    input: T,
    filename: &str,
) -> impl Iterator<Item = Result<Token, LexerError>> {
    let lexer = Lexer::new(input, filename);
    return lexer;
}
