use std::io::{self, BufRead, Write};
use std::path::Path;
use thiserror::Error;
mod tokenizer;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("")]
    Io(#[from] io::Error),
    #[error("Syntax errors: {0:?}")]
    SyntaxErrors(Vec<tokenizer::SyntaxError>),
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
    for token in scanner {
        println!("{:?}", token);
    }
    Ok(())
}
