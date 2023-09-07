use std::iter::Peekable;

#[derive(Debug)]
pub enum Token {
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

struct Lexer<T: Iterator<Item = char>> {
    inner: Peekable<T>,
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return self.inner.next().and_then(|c| match c {
            d if d.is_digit(10) => {
                let mut value = String::new();
                value.push(d);
                while self.inner.peek().filter(|c| c.is_digit(10)).is_some() {
                    value.push(self.inner.next().unwrap());
                }
                Some(Token::IntLiteral {
                    value: value.parse().unwrap(),
                })
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
                Some(match name.as_str() {
                    "let" => Token::Let,
                    "if" => Token::If,
                    "else" => Token::Else,
                    _ => Token::Identifier { name },
                })
            }
            '"' => {
                let mut value = String::new();
                while self.inner.peek().filter(|c| c != &&'"').is_some() {
                    value.push(self.inner.next().unwrap());
                }
                self.inner.next();
                Some(Token::StringLiteral { value })
            }
            '=' => Some(match self.inner.peek() {
                Some(&'>') => {
                    self.inner.next();
                    Token::ArrowRight
                }
                Some(&'=') => {
                    self.inner.next();
                    Token::DoubleEqual
                }
                _ => Token::Equal,
            }),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenCurly),
            '}' => Some(Token::CloseCurly),
            '<' => Some(match self.inner.peek() {
                Some(&'=') => {
                    self.inner.next();
                    Token::LessOrEqualThan
                }
                _ => Token::LessThan,
            }),
            '>' => Some(match self.inner.peek() {
                Some(&'=') => {
                    self.inner.next();
                    Token::GreaterOrEqualThan
                }
                _ => Token::GreaterThan,
            }),
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            s if s.is_ascii_whitespace() => self.next(),
            unknown => panic!("Unknown char: {}", unknown),
        });
    }
}

pub fn tokens<T: Iterator<Item = char>>(input: T) -> impl Iterator<Item = Token> {
    let lexer = Lexer {
        inner: input.peekable(),
    };
    return lexer;
}
