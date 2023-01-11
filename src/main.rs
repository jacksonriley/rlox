fn main() -> Result<(), rlox::LoxError> {
    match std::env::args().skip(1).next() {
        Some(file) => rlox::run_file(file),
        None => rlox::run_prompt(),
    }
}
