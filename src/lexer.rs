use std::iter::Peekable;

use crate::common::Location;

#[derive(Debug)]
pub enum TokenType {
    Let,
    If,
    Else,
    Identifier { name: String },
    Equal,
    DoubleEqual,
    ArrowRight,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    IntLiteral { value: i64 },
    StringLiteral { value: String },
    LessThan,
    LessOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    Plus,
    Minus,
    Star,
    Semicolon,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub value: TokenType,
    pub location: Location<'a>,
}

struct Lexer<'a, T: Iterator<Item = char>> {
    filename: &'a str,
    inner: Peekable<T>,
    current_position: usize,
}

impl<'a, T: Iterator<Item = char>> Iterator for Lexer<'a, T> {
    type Item = Token<'a>;

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
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        _ => TokenType::Identifier { name },
                    }
                }
                '"' => {
                    let mut value = String::new();
                    while self.inner.peek().filter(|c| c != &&'"').is_some() {
                        value.push(self.inner.next().unwrap());
                    }
                    self.inner.next();
                    end += value.len() + 2;
                    TokenType::StringLiteral { value }
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
                s if s.is_ascii_whitespace() => {
                    self.current_position += 1;
                    return self.next();
                }
                unknown => panic!("Unknown char: {}", unknown),
            };
            let token = Token {
                value,
                location: Location {
                    start: self.current_position,
                    end,
                    filename: self.filename,
                },
            };
            self.current_position = end;
            return Some(token);
        }
        return None;
    }
}

pub fn tokens<T: Iterator<Item = char>>(input: T, filename: &str) -> impl Iterator<Item = Token> {
    let lexer = Lexer {
        filename,
        inner: input.peekable(),
        current_position: 0,
    };
    return lexer;
}
