use std::iter::Peekable;

use crate::token::lexer::Lexer;
use crate::token::types::Token;

use super::ast::{LetStatement, Program, Statement};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();

        while let Some(current_token) = self.lexer.next() {
            match current_token {
                Token::Let => statements.push(self.parse_let_statement(current_token)),
                Token::Return => todo!(),
                Token::If => todo!(),
                _ => continue,
            }
        }

        Program { statements }
    }

    fn parse_let_statement(&mut self, token: Token) -> Box<dyn Statement> {
        let ident: String;
        if let Some(Token::Identifier(x)) = self.lexer.peek() {
            ident = x.to_string();
        } else {
            todo!("Need to return None here");
        }
        Box::new(LetStatement {
            token,
            name: Token::Identifier(ident),
            value: todo!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::ast::{Expression, LetStatement},
        token::{lexer::Lexer, types::Token},
    };
    use pretty_assertions::assert_eq;

    use super::Parser;

    #[test]
    fn let_statements_only() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let expected_identifiers = vec!["x", "y", "foobar"];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(3, program.statements.len());
        for (i, statement) in program.statements.iter().enumerate() {
            let &expected_identifier = expected_identifiers.get(i).unwrap();
            match statement.node().token() {
                Some(Token::Let) => continue,
                Some(Token::Identifier(x)) => assert_eq!(expected_identifier, x),
                s => panic!("Panicked when given {:?}", s),
            }
        }
    }
}
