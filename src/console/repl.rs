use crate::{
    parse::{ast::Program, parser::Parser},
    token::lexer::Lexer,
};

pub struct Repl {}

impl Repl {
    pub fn new() -> Self {
        Repl {}
    }

    pub fn run(&self, line: &str) -> Program {
        let mut parser = Parser::new(Lexer::new(line));

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            panic!("Errors encountered: {:#?}", parser.errors);
        }
        return program;
    }
}
