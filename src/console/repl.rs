use crate::token::{lexer::Lexer, types::Token};

pub struct Repl {}

impl Repl {
    pub fn new() -> Self {
        Repl {}
    }

    pub fn run(&self, line: &str) -> Vec<Token> {
        let lexer = Lexer::new(line);
        lexer.collect()
    }
}
