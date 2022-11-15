use std::iter::Peekable;

use crate::token::lexer::Lexer;
use crate::token::types::Token;

use super::ast::{Expression, Program, Statement};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
            errors: Vec::new(),
        }
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "Expected next token to be {:?}, got {:?} instead",
            token,
            self.lexer.peek().unwrap()
        ));
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
            self.peek_error(Token::Identifier(String::new()));
            return None;
        }

        if let Some(Token::Assignment) = self.lexer.peek() {
            self.lexer.next();
        } else {
            self.peek_error(Token::Assignment);
        }

        // TODO: Skip until the semicolon
        loop {
            match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => self.lexer.next(),
            };
        }

        Some(Statement::Let {
            name: name.clone(),
            value: Expression::Identifier { name },
        })
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        match self.lexer.next() {
            Some(Token::Identifier(name)) => {
                return Some(Expression::Identifier { name });
            }
            _ => {
                self.errors
                    .push(String::from("Expected identifier not found"));
                return None;
            }
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
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(3, program.statements.len());
        for (i, statement) in program.statements.iter().enumerate() {
            let &expected_identifier = expected_identifiers.get(i).unwrap();
            match statement {
                Statement::Let { name, value } => {
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
