use std::{cell::RefCell, rc::Rc};

use crate::{
    object::{
        evaluator::eval,
        macro_expansion::{define_macros, expand_macros},
        types::{Environment, Object},
    },
    parse::parser::Parser,
    token::lexer::Lexer,
};

pub struct Interpreter {
    eval_env: Rc<RefCell<Environment>>,
    macro_env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            eval_env: Rc::new(RefCell::new(Environment::new())),
            macro_env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn run(&self, line: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(line));
        let mut program = parser.parse_program();

        if !parser.errors.is_empty() {
            panic!("Errors encountered: {:#?}", parser.errors);
        }

        define_macros(&mut program, self.macro_env.clone());
        let expanded = expand_macros(&mut program, self.macro_env.clone());

        return eval(expanded, self.eval_env.clone());
    }
}
