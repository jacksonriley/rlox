use std::io::{self, BufRead, Write};
use std::path::Path;
use thiserror::Error;
mod ast;
mod interpreter;
mod tokenizer;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("Syntax errors: {0:?}")]
    SyntaxErrors(Vec<tokenizer::SyntaxError>),
    #[error("Parse errors: {0:?}")]
    ParseErrors(Vec<ast::ParseError>),
    #[error("{0}")]
    RuntimeError(#[from] interpreter::RuntimeError),
}

pub fn run_file<P: AsRef<Path>>(file: P) -> Result<(), LoxError> {
    let file_contents = std::fs::read_to_string(file.as_ref())?;

    run(&file_contents)
}

pub fn run_prompt() -> Result<(), LoxError> {
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush()?;
        match stdin.lock().lines().next() {
            Some(line) => match run(&line?) {
                Ok(_) => {}
                Err(e) => println!("{}", e),
            },
            None => break Ok(()),
        };
    }
}

fn run(s: &str) -> Result<(), LoxError> {
    let scanner = tokenizer::Scanner::new(s);
    let (tokens, errors): (Vec<_>, Vec<_>) = scanner.partition(Result::is_ok);
    if !errors.is_empty() {
        return Err(LoxError::SyntaxErrors(
            errors.into_iter().map(Result::unwrap_err).collect(),
        ));
    }
    let tokens = tokens.into_iter().map(Result::unwrap);
    let parser = ast::Parser::new(tokens);
    let parsed = parser.parse().map_err(|t| LoxError::ParseErrors(t.1))?;
    let interpreted = interpreter::Interpreter {}.interpret(parsed)?;
    println!("{}", interpreted);
    Ok(())
}
