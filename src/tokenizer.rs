use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
    // Tokens
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
    pub(crate) kind: TokenType,
    pub(crate) span: Span,
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
}
impl Scanner<'_> {
    pub fn new(source: &'_ str) -> Scanner<'_> {
        Scanner {
            raw_source: source,
            source: source.chars().enumerate().peekable(),
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
        let (idx, c) = self.source.next()?;
        Some(Ok(match c {
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
            '"' => return Some(self.string(idx)),
            c if c.is_ascii_digit() => return Some(self.number(idx, c)),
            c if c.is_ascii_alphabetic() || c == '_' => return Some(self.identifier(idx, c)),
            c => {
                return Some(Err(self.calculate_syntax_err(
                    idx,
                    format!("Invalid syntax: got unexpected character '{c}'"),
                )))
            }
        }))
    }

    fn identifier(&mut self, idx: usize, initial: char) -> Result<Token, SyntaxError> {
        let mut result = initial.to_string();
        while let Some((_, c)) = self.source.peek() {
            if c.is_ascii_alphanumeric() || *c == '_' {
                result.push(*c);
                self.source.next();
            } else {
                break;
            }
        }

        let len = result.len();

        use TokenType::*;
        let tt = match &result[..] {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier(result),
        };
        Ok(Token {
            kind: tt,
            span: Span {
                start: idx,
                end: idx - 1 + len,
            },
        })
    }

    fn string(&mut self, idx: usize) -> Result<Token, SyntaxError> {
        // The leading '"' has already been consumed, so read until the next one.
        let mut result = String::new();
        loop {
            match self.source.next() {
                Some((_, c)) => {
                    if c == '"' {
                        // We're done
                        let len = result.len();
                        return Ok(Token {
                            kind: TokenType::Str(result),
                            span: Span {
                                start: idx,
                                end: idx + 1 + len,
                            },
                        });
                    } else {
                        result.push(c);
                    }
                }
                None => return Err(self.calculate_syntax_err(idx, r#"Unmatched '"'"#.into())),
            }
        }
    }

    fn number(&mut self, idx: usize, initial: char) -> Result<Token, SyntaxError> {
        let mut result = initial.to_string();
        let mut passed_decimal = false;
        loop {
            match self.source.peek().copied() {
                Some((_, c)) => {
                    if c.is_ascii_digit() {
                        result.push(self.source.next().unwrap().1);
                    } else if c == '.' && !passed_decimal {
                        result.push(self.source.next().unwrap().1);
                        passed_decimal = true;
                    } else {
                        // We're done
                        return Ok(Token {
                            kind: TokenType::Number(result.parse().unwrap()),
                            span: Span {
                                start: idx,
                                end: idx - 1 + result.len(),
                            },
                        });
                    }
                }
                None => {
                    return Ok(Token {
                        kind: TokenType::Number(result.parse().unwrap()),
                        span: Span {
                            start: idx,
                            end: idx - 1 + result.len(),
                        },
                    })
                }
            }
        }
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
