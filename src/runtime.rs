use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Rem, Sub},
    sync::Arc,
};

use crate::parser::{Binary, BinaryOp, Call, File, Term, TermType};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Str(String),
    Bool(bool),
    Tuple(Box<Value>, Box<Value>),
    Function(Vec<Arc<str>>, Box<Term>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Tuple(l0, l1), Self::Tuple(r0, r1)) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.partial_cmp(r0),
            (Self::Str(l0), Self::Str(r0)) => l0.partial_cmp(r0),
            (Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(r0),
            (Self::Tuple(l0, l1), Self::Tuple(r0, r1)) => match l0.partial_cmp(r0) {
                Some(std::cmp::Ordering::Equal) => l1.partial_cmp(r1),
                other => other,
            },
            _ => None,
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(val) => val.to_string(),
            Value::Str(val) => val.to_string(),
            Value::Bool(val) => val.to_string(),
            Value::Tuple(a, b) => format!("({}, {})", a.to_string(), b.to_string()),
            Value::Function(_, _) => "function".to_string(),
        }
    }
}

impl Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Str(a), b) => Value::Str(a + b.to_string().as_str()),
            (a, Value::Str(b)) => Value::Str(a.to_string() + b.as_str()),
            _ => panic!("cannot add these values"),
        }
    }
}
impl Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            _ => panic!("cannot subtract these values"),
        }
    }
}
impl Mul<Value> for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            _ => panic!("cannot multiply these values"),
        }
    }
}
impl Rem<Value> for Value {
    type Output = Value;

    fn rem(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            _ => panic!("cannot rem these values"),
        }
    }
}
impl Div<Value> for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            _ => panic!("cannot divide these values"),
        }
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}
impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(val) => val,
            _ => panic!("cannot convert to bool"),
        }
    }
}

struct StackFrame {
    variables: HashMap<Arc<str>, Value>,
}

pub struct Runtime {
    stack: Vec<StackFrame>,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime { stack: vec![] }
    }

    pub fn run(&mut self, file: &File) -> Value {
        self.stack.push(StackFrame {
            variables: HashMap::new(),
        });
        let value = self.run_expression(&file.expression);
        self.stack.pop();
        value
    }

    fn run_expression(&mut self, expression: &Term) -> Value {
        match &expression.value {
            TermType::Int { value } => Value::Int(*value),
            TermType::Str { value } => Value::Str(value.to_string()),
            TermType::Call(Call { callee, arguments }) => {
                let callee = self.run_expression(callee);
                let arguments: Vec<Value> =
                    arguments.iter().map(|a| self.run_expression(a)).collect();
                match callee {
                    Value::Function(parameters, body) => {
                        self.stack.push(StackFrame {
                            variables: parameters.iter().cloned().zip(arguments).collect(),
                        });
                        let value = self.run_expression(&body);
                        self.stack.pop();
                        value
                    }
                    _ => panic!("cannot call non-function"),
                }
            }
            TermType::Binary(Binary { lhs, op, rhs }) => {
                let lhs = self.run_expression(lhs);
                let rhs = self.run_expression(rhs);
                match op {
                    BinaryOp::Add => lhs + rhs,
                    BinaryOp::Sub => lhs - rhs,
                    BinaryOp::Mul => lhs * rhs,
                    BinaryOp::Div => lhs / rhs,
                    BinaryOp::Rem => lhs % rhs,
                    BinaryOp::Eq => (lhs == rhs).into(),
                    BinaryOp::Neq => (lhs != rhs).into(),
                    BinaryOp::Lt => (lhs < rhs).into(),
                    BinaryOp::Gt => (lhs > rhs).into(),
                    BinaryOp::Lte => (lhs <= rhs).into(),
                    BinaryOp::Gte => (lhs >= rhs).into(),
                    BinaryOp::And => (lhs.into() && rhs.into()).into(),
                    BinaryOp::Or => (lhs.into() || rhs.into()).into(),
                }
            }
            TermType::Function { value, parameters } => {
                let parameters = parameters.iter().cloned().map(|p| p.text).collect();
                Value::Function(parameters, value.clone())
            }
            TermType::Let { name, value, next } => {
                let value = self.run_expression(&value);
                let frame = self.stack.last_mut().unwrap();
                frame.variables.insert(name.text.clone(), value);
                match next {
                    Some(next) => self.run_expression(next),
                    None => Value::Int(0),
                }
            }
            TermType::If {
                condition,
                then,
                otherwise,
            } => {
                if self.run_expression(&condition).into() {
                    self.run_expression(&then)
                } else {
                    self.run_expression(&otherwise)
                }
            }
            TermType::Print { value } => {
                println!("{}", self.run_expression(&value).to_string());
                Value::Int(0)
            }
            TermType::Tuple { first, second } => Value::Tuple(
                Box::new(self.run_expression(first)),
                Box::new(self.run_expression(second)),
            ),
            TermType::First { value } => {
                let value = self.run_expression(&value);
                match value {
                    Value::Tuple(first, _) => *first,
                    _ => panic!("cannot get first of non-tuple"),
                }
            }
            TermType::Second { value } => {
                let value = self.run_expression(&value);
                match value {
                    Value::Tuple(_, second) => *second,
                    _ => panic!("cannot get second of non-tuple"),
                }
            }
            TermType::Bool { value } => Value::Bool(*value),
            TermType::Var { text } => {
                let frame = self.stack.last().expect("no stack frame");
                frame
                    .variables
                    .get(text)
                    .expect(format!("no variable `{text}`").as_str())
                    .clone()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::tokens, parser::parse_file};

    use super::*;

    #[test]
    fn test_binary() {
        let mut runtime = Runtime::new();
        assert_eq!(
            runtime.run(&parse_file(tokens("2".chars(), "")).unwrap()),
            Value::Int(2)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 + 2".chars(), "")).unwrap()),
            Value::Int(4)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 + 2 * 3".chars(), "")).unwrap()),
            Value::Int(8)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 + 2 == 4".chars(), "")).unwrap()),
            Value::Bool(true)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 + 2 > 4".chars(), "")).unwrap()),
            Value::Bool(false)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("\"2\" + 2".chars(), "")).unwrap()),
            Value::Str("22".to_string())
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 + \"2\"".chars(), "")).unwrap()),
            Value::Str("22".to_string())
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 > 1 && 3 > 2".chars(), "")).unwrap()),
            Value::Bool(true)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 > 1 && 3 < 2".chars(), "")).unwrap()),
            Value::Bool(false)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 > 5 || 3 > 2".chars(), "")).unwrap()),
            Value::Bool(true)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("2 > 2 || 3 < 2".chars(), "")).unwrap()),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_if() {
        let mut runtime = Runtime::new();
        assert_eq!(
            runtime.run(&parse_file(tokens("if (2 > 1) { 1 } else { 2 }".chars(), "")).unwrap()),
            Value::Int(1)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("if (2 < 1) { 1 } else { 2 }".chars(), "")).unwrap()),
            Value::Int(2)
        );
    }

    #[test]
    fn test_let() {
        let mut runtime = Runtime::new();
        assert_eq!(
            runtime.run(&parse_file(tokens("let x = 1; x".chars(), "")).unwrap()),
            Value::Int(1)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("let x = 1; let y = 2; x + y".chars(), "")).unwrap()),
            Value::Int(3)
        );
        assert_eq!(
            runtime
                .run(&parse_file(tokens("let x = 1; let x = x + 1; x + x".chars(), "")).unwrap()),
            Value::Int(4)
        );
    }

    #[test]
    fn test_tuples() {
        let mut runtime = Runtime::new();
        assert_eq!(
            runtime.run(&parse_file(tokens("(1, 2)".chars(), "")).unwrap()),
            Value::Tuple(Box::new(Value::Int(1)), Box::new(Value::Int(2)))
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("first((1, 2))".chars(), "")).unwrap()),
            Value::Int(1)
        );
        assert_eq!(
            runtime.run(&parse_file(tokens("second((1, 2))".chars(), "")).unwrap()),
            Value::Int(2)
        );
    }
}
