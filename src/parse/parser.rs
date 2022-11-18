use std::{
    collections::HashMap,
    iter::Peekable,
    mem::{discriminant, Discriminant},
};

use crate::token::lexer::Lexer;
use crate::token::types::Token;

use super::ast::{Expression, Program, Statement};

type PrefixParse = fn(&mut Parser) -> Option<Expression>;
type PostfixParse = fn(&mut Parser, Expression) -> Option<Expression>;

enum Precendence {
    Lowest,
    Equals,      // ==
    Lessgreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // my_function(X)
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    current: Option<Token>,
    errors: Vec<String>,
    prefix_functions: HashMap<Discriminant<Token>, PrefixParse>,
    postfix_functions: HashMap<Discriminant<Token>, PostfixParse>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer: lexer.peekable(),
            current: None,
            errors: Vec::new(),
            prefix_functions: HashMap::new(),
            postfix_functions: HashMap::new(),
        };
        parser.register_prefix(Token::Identifier(String::default()), move |p| {
            p.parse_identifier()
        });
        parser.register_prefix(Token::Integer(usize::default()), move |p| {
            p.parse_integer_literal()
        });
        return parser;
    }

    fn next(&mut self) -> Option<Token> {
        self.current = self.lexer.next();
        return self.current.clone();
    }

    fn register_prefix(&mut self, token: Token, function: PrefixParse) {
        self.prefix_functions.insert(discriminant(&token), function);
    }

    fn register_postfix(&mut self, token: Token, function: PostfixParse) {
        self.postfix_functions
            .insert(discriminant(&token), function);
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

        while let Some(_) = self.next() {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name: String;

        if let Some(Token::Identifier(ident)) = self.lexer.peek() {
            name = ident.to_string();
            self.next();
        } else {
            self.peek_error(Token::Identifier(String::default()));
            return None;
        }

        if let Some(Token::Assignment) = self.lexer.peek() {
            self.next();
        } else {
            self.peek_error(Token::Assignment);
        }

        // TODO: Skip until the semicolon
        loop {
            match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => self.next(),
            };
        }

        Some(Statement::Let {
            name: name.clone(),
            value: Expression::Identifier { name },
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        // TODO: Skip until the semicolon
        loop {
            match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => self.next(),
            };
        }

        Some(Statement::Return {
            value: Expression::Identifier {
                name: String::default(),
            },
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let statement = self.parse_expression(Precendence::Lowest);

        if statement.is_none() {
            return None;
        }

        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next();
        }

        Some(Statement::Expression {
            value: statement.unwrap(),
        })
    }

    fn parse_expression(&mut self, precedence: Precendence) -> Option<Expression> {
        let next_token: &Token;

        if let Some(token) = &self.current {
            next_token = token;
        } else {
            return None;
        }

        if let Some(prefix) = self.prefix_functions.get(&discriminant(next_token)) {
            return prefix(self);
        }
        None
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        if let Some(Token::Identifier(name)) = &self.current {
            return Some(Expression::Identifier {
                name: name.to_string(),
            });
        }
        return None;
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let Some(Token::Integer(value)) = &self.current {
            return Some(Expression::Integer {
                value: value.to_owned(),
            });
        }
        return None;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::ast::{Expression, Statement},
        token::lexer::Lexer,
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
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(3, program.statements.len());
        for (i, statement) in program.statements.iter().enumerate() {
            let &expected_identifier = expected_identifiers.get(i).unwrap();
            match statement {
                Statement::Let { name, value } => {
                    assert_eq!(expected_identifier, name);
                    // TODO: Assert value expression
                }
                _ => {
                    panic!("Statement was not LetStatement!");
                }
            }
        }
    }

    #[test]
    fn return_statements_only() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(3, program.statements.len());
        for statement in program.statements.iter() {
            match statement {
                Statement::Return { value } => {
                    // TODO: Assert value expression
                }
                _ => panic!("Statement was not Return statement!"),
            }
        }
    }

    #[test]
    fn single_identifier() {
        let input = "foobar;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(1, program.statements.len());
        let statement = program.statements.get(0).unwrap();
        match statement {
            Statement::Expression {
                value: Expression::Identifier { name },
            } => {
                assert_eq!("foobar", name);
            }
            _ => panic!("Statement was not a standalone expression!"),
        }
    }

    #[test]
    fn single_integer_literal() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(1, program.statements.len());
        let statement = program.statements.get(0).unwrap();
        match statement {
            Statement::Expression {
                value: Expression::Integer { value },
            } => {
                assert_eq!(5, value.to_owned());
            }
            _ => panic!("Statement was not a standalone integer!"),
        }
    }
}
