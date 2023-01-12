use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    Str(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug)]
pub(crate) struct Token {
    kind: TokenType,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct Span {
    // Start of a token (inclusive)
    start: usize,
    // End of a token (inclusive)
    end: usize,
}

/// The Lox lexer.
pub(crate) struct Scanner<'a> {
    /// The underlying source characters
    raw_source: &'a str,
    /// The underlying source characters
    source: Peekable<Enumerate<Chars<'a>>>,
    /// Current token
    current: String,
}
impl Scanner<'_> {
    pub fn new<'a>(source: &'a str) -> Scanner<'a> {
        Scanner {
            raw_source: source,
            source: source.chars().enumerate().peekable(),
            current: Default::default(),
        }
    }

    fn calculate_syntax_err(&self, idx: usize, message: String) -> SyntaxError {
        let line = self
            .raw_source
            .chars()
            .take(idx)
            .filter(|&c| c == '\n')
            .count()
            + 1;
        let column = idx
            - self
                .raw_source
                .lines()
                .take(line - 1)
                .map(|l| l.len() + 1)
                .sum::<usize>();
        SyntaxError {
            line,
            column,
            message,
        }
    }

    fn single(tt: TokenType, idx: usize) -> Token {
        Token {
            kind: tt,
            span: Span {
                start: idx,
                end: idx,
            },
        }
    }

    fn double(tt: TokenType, start_idx: usize) -> Token {
        Token {
            kind: tt,
            span: Span {
                start: start_idx,
                end: start_idx + 1,
            },
        }
    }

    fn produce_next_token(&mut self) -> Option<Result<Token, SyntaxError>> {
        use TokenType::*;
        Some(Ok(match self.source.next()? {
            (idx, c) => match c {
                '(' => Self::single(LeftParen, idx),
                ')' => Self::single(RightParen, idx),
                '{' => Self::single(LeftBrace, idx),
                '}' => Self::single(RightBrace, idx),
                ',' => Self::single(Comma, idx),
                '.' => Self::single(Dot, idx),
                '-' => Self::single(Minus, idx),
                '+' => Self::single(Plus, idx),
                ';' => Self::single(Semicolon, idx),
                '*' => Self::single(Star, idx),
                '!' => {
                    match self.source.peek() {
                        Some((_, '=')) => {
                            // Consume
                            self.source.next().unwrap();
                            Self::double(BangEqual, idx)
                        }
                        _ => Self::single(Bang, idx),
                    }
                }
                '=' => {
                    match self.source.peek() {
                        Some((_, '=')) => {
                            // Consume
                            self.source.next().unwrap();
                            Self::double(EqualEqual, idx)
                        }
                        _ => Self::single(Equal, idx),
                    }
                }
                '<' => {
                    match self.source.peek() {
                        Some((_, '=')) => {
                            // Consume
                            self.source.next().unwrap();
                            Self::double(LessEqual, idx)
                        }
                        _ => Self::single(Less, idx),
                    }
                }
                '>' => {
                    match self.source.peek() {
                        Some((_, '=')) => {
                            // Consume
                            self.source.next().unwrap();
                            Self::double(GreaterEqual, idx)
                        }
                        _ => Self::single(Greater, idx),
                    }
                }
                '/' => {
                    match self.source.peek() {
                        Some((_, '/')) => {
                            // This is a comment! Consume until the end of the line.
                            while !matches!(self.source.next(), Some((_, '\n')) | None) {}
                            return self.produce_next_token();
                        }
                        _ => Self::single(Slash, idx),
                    }
                }
                ' ' | '\r' | '\t' | '\n' => return self.produce_next_token(),
                c => {
                    return Some(Err(self.calculate_syntax_err(
                        idx,
                        format!("Invalid syntax: got unexpected character '{c}'"),
                    )))
                }
            },
        }))
    }
}

impl Iterator for Scanner<'_> {
    type Item = Result<Token, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.produce_next_token()
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
    column: usize,
    message: String,
}
