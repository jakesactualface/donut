use std::iter::Peekable;

use crate::token::{lexer::Lexer, types::Token};

use super::ast::Program;

pub struct Parser<'a> {
    lexer: &'a Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a Lexer<'a>) -> Self {
        Parser { lexer }
    }

    fn parse_program(&self) -> Program {
        Program {
            statements: todo!(),
        }
    }
}
