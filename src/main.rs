fn main() -> Result<(), rlox::LoxError> {
    match std::env::args().nth(1) {
        Some(file) => rlox::run_file(file),
        None => rlox::run_prompt(),
    }
}
