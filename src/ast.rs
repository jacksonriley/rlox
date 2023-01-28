use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;

#[derive(Debug)]
pub(crate) enum Statement {
    Print(Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub(crate) enum Expr {
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    Str(String),
    False,
    True,
    Nil,
}

impl TryFrom<&TokenType> for Literal {
    type Error = String;

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::False => Ok(Self::False),
            TokenType::True => Ok(Self::True),
            TokenType::Nil => Ok(Self::Nil),
            TokenType::Number(n) => Ok(Self::Number(*n)),
            TokenType::Str(s) => Ok(Self::Str(s.clone())), // TODO: Sad clone
            _ => Err(format!("Token type {value:?} is not a literal.")),
        }
    }
}

#[derive(Debug)]
pub(crate) enum UnaryOp {
    Bang,
    Minus,
}

impl TryFrom<&TokenType> for UnaryOp {
    type Error = String;

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Bang => Ok(Self::Bang),
            TokenType::Minus => Ok(Self::Minus),
            _ => Err(format!("Token type {value:?} is not a unary operation.")),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum BinaryOp {
    // Equality
    BangEqual,
    EqualEqual,
    // Comparison
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Term
    Plus,
    Minus,
    // Factor
    Slash,
    Star,
}

impl TryFrom<&TokenType> for BinaryOp {
    type Error = String;

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Minus => Ok(Self::Minus),
            TokenType::Plus => Ok(Self::Plus),
            TokenType::Slash => Ok(Self::Slash),
            TokenType::Star => Ok(Self::Star),
            TokenType::BangEqual => Ok(Self::BangEqual),
            TokenType::EqualEqual => Ok(Self::EqualEqual),
            TokenType::Greater => Ok(Self::Greater),
            TokenType::GreaterEqual => Ok(Self::GreaterEqual),
            TokenType::Less => Ok(Self::Less),
            TokenType::LessEqual => Ok(Self::LessEqual),
            // TODO: Allocating here and in analogous places is hit on the
            //       critical path - perhaps just turn into a `()`?
            _ => Err(format!("Token type {value:?} is not a binary operation.")),
        }
    }
}

pub(crate) struct Parser<T>
where
    T: Iterator,
{
    toks: Peekable<T>,
    errors: Vec<ParseError>,
}

#[derive(Debug)]
pub struct ParseError {
    token: Option<Token>,
    message: String,
}

impl<T> Parser<T>
where
    T: Iterator,
{
    pub(crate) fn new(val: T) -> Self {
        Parser {
            toks: val.peekable(),
            errors: Default::default(),
        }
    }
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub(crate) fn parse(mut self) -> Result<Vec<Statement>, (Vec<Statement>, Vec<ParseError>)> {
        // Parse a list of statements
        let mut res = vec![];
        let mut errors = vec![];
        while matches!(self.toks.peek(), Some(_)) {
            match self.statement() {
                Ok(stmt) => res.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        if errors.is_empty() {
            Ok(res)
        } else {
            Err((res, errors))
        }
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        let res = match self.toks.peek() {
            Some(tok) => match &tok.kind {
                TokenType::Print => {
                    // Consume the "print"
                    self.consume(TokenType::Print)?;
                    Statement::Print(self.expression())
                }
                _ => Statement::Expr(self.expression()),
            },
            None => {
                return Err(ParseError {
                    token: None,
                    message: "Got end of tokens when expected a statement".into(),
                })
            }
        };
        self.consume(TokenType::Semicolon)?;
        Ok(res)
    }

    // expression     → equality ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    // unary          → ( "!" | "-" ) unary
    //                  | primary ;
    // primary        → NUMBER | STRING | "Nil" | "false" | "nil"
    //                  | "(" expression ")" ;
    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while let Some(tok) = self.toks.peek() {
            match BinaryOp::try_from(&tok.kind) {
                Ok(op) if matches!(op, BinaryOp::BangEqual | BinaryOp::EqualEqual) => {
                    // Consume
                    let _ = self.toks.next().unwrap();
                    let right = self.comparison();
                    expr = Expr::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while let Some(tok) = self.toks.peek() {
            match BinaryOp::try_from(&tok.kind) {
                Ok(op)
                    if matches!(
                        op,
                        BinaryOp::Greater
                            | BinaryOp::GreaterEqual
                            | BinaryOp::Less
                            | BinaryOp::LessEqual
                    ) =>
                {
                    // Consume
                    let _ = self.toks.next().unwrap();
                    let right = self.term();
                    expr = Expr::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while let Some(tok) = self.toks.peek() {
            match BinaryOp::try_from(&tok.kind) {
                Ok(op) if matches!(op, BinaryOp::Plus | BinaryOp::Minus) => {
                    // Consume
                    let _ = self.toks.next().unwrap();
                    let right = self.factor();
                    expr = Expr::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while let Some(tok) = self.toks.peek() {
            match BinaryOp::try_from(&tok.kind) {
                Ok(op) if matches!(op, BinaryOp::Slash | BinaryOp::Star) => {
                    // Consume
                    let _ = self.toks.next().unwrap();
                    let right = self.unary();
                    expr = Expr::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if let Some(tok) = self.toks.peek() {
            match UnaryOp::try_from(&tok.kind) {
                Ok(op) if matches!(op, UnaryOp::Minus | UnaryOp::Bang) => {
                    // Consume
                    let _ = self.toks.next().unwrap();
                    let right = self.unary();
                    return Expr::Unary(op, Box::new(right));
                }
                _ => {}
            }
        }

        self.primary()
    }

    fn primary(&mut self) -> Expr {
        match self.toks.next() {
            Some(tok) => match Literal::try_from(&tok.kind) {
                Ok(l) => Expr::Literal(l),
                _ => match tok.kind {
                    TokenType::LeftParen => {
                        let expr = self.expression();
                        match self.toks.next() {
                            Some(t) if matches!(t.kind, TokenType::RightParen) => {
                                Expr::Grouping(Box::new(expr))
                            }
                            _ => panic!("Expect ')' after expression"),
                        }
                    }
                    _ => panic!("Expected '(', got {tok:?}"),
                },
            },
            None => panic!("Got end of tokens when expected a primary"),
        }
    }

    fn consume(&mut self, tt: TokenType) -> Result<(), ParseError> {
        match self.toks.next() {
            Some(t) if t.kind == tt => Ok(()),
            other => {
                let message = format!("Expected token type {tt:?}, got {other:?}");
                Err(ParseError {
                    token: other,
                    message: message,
                })
            }
        }
    }
}
