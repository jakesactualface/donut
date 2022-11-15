use std::iter::Peekable;

use crate::token::lexer::Lexer;
use crate::token::types::Token;

use super::ast::{Expression, Program, Statement};

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
        let mut statements: Vec<Statement> = Vec::new();

        while let Some(current_token) = self.lexer.next() {
            if let Some(statement) = self.parse_statement(current_token) {
                statements.push(statement);
            }
        }

        Program { statements }
    }

    fn parse_statement(&mut self, current_token: Token) -> Option<Statement> {
        match current_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name: String;

        if let Some(Token::Identifier(ident)) = self.lexer.peek() {
            name = ident.to_string();
            self.lexer.next();
        } else {
            return None;
        }

        if let Some(&Token::Assignment) = self.lexer.peek() {
            self.lexer.next();
        } else {
            panic!("Expected equal sign, found: {:?}", self.lexer.next());
        }

        // TODO: Skip until the semicolon
        loop {
            match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => self.lexer.next(),
            };
        }

        Some(Statement::Let(name.clone(), Expression::Identifier(name)))
    }

    fn parse_identifier(&mut self) -> Expression {
        match self.lexer.next() {
            Some(Token::Identifier(name)) => {
                return Expression::Identifier(name);
            }
            _ => panic!("Expected identifier not found"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse::ast::Statement, token::lexer::Lexer};
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
            match statement {
                Statement::Let(name, expression) => {
                    assert_eq!(expected_identifier, name);
                    // TODO: Assert expression
                }
                _ => {
                    panic!("Statement was not LetStatement!");
                }
            }
        }
    }
}
