// TODO: Remove this!
#![allow(dead_code)]

use std::{
    collections::HashMap,
    iter::Peekable,
    mem::{discriminant, Discriminant},
};

use crate::token::types::Token;
use crate::token::{lexer::Lexer, types::Precedence};

use super::ast::{Expression, Program, Statement};

type PrefixParse = fn(&mut Parser) -> Option<Expression>;
type InfixParse = fn(&mut Parser, Box<Expression>) -> Option<Expression>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    current: Option<Token>,
    errors: Vec<String>,
    prefix_functions: HashMap<Discriminant<Token>, PrefixParse>,
    infix_functions: HashMap<Discriminant<Token>, InfixParse>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer: lexer.peekable(),
            current: None,
            errors: Vec::new(),
            prefix_functions: HashMap::new(),
            infix_functions: HashMap::new(),
        };
        parser.register_prefix(Token::Identifier(String::default()), move |p| {
            p.parse_identifier()
        });
        parser.register_prefix(Token::Integer(usize::default()), move |p| {
            p.parse_integer_literal()
        });
        parser.register_prefix(Token::True, move |p| p.parse_boolean());
        parser.register_prefix(Token::False, move |p| p.parse_boolean());
        parser.register_prefix(Token::Bang, move |p| p.parse_prefix_expression());
        parser.register_prefix(Token::Minus, move |p| p.parse_prefix_expression());
        parser.register_prefix(Token::LParen, move |p| p.parse_grouped_expression());
        parser.register_infix(Token::Equal, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::NotEqual, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::LT, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::GT, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Plus, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Minus, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Slash, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Asterisk, move |p, e| p.parse_infix_expression(e));
        return parser;
    }

    fn next(&mut self) -> Option<Token> {
        self.current = self.lexer.next();
        return self.current.clone();
    }

    fn register_prefix(&mut self, token: Token, function: PrefixParse) {
        self.prefix_functions.insert(discriminant(&token), function);
    }

    fn register_infix(&mut self, token: Token, function: InfixParse) {
        self.infix_functions.insert(discriminant(&token), function);
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
            match self.next() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => (),
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
            match self.next() {
                Some(Token::Semicolon) => break,
                None => panic!("Missing semicolon"),
                _ => (),
            };
        }

        Some(Statement::Return {
            value: Expression::Identifier {
                name: String::default(),
            },
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let statement = self.parse_expression(Precedence::Lowest);

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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let current: &Token;

        if let Some(token) = &self.current {
            current = token;
        } else {
            self.errors.push(String::from("No token in scope!"));
            return None;
        }

        let mut expression: Option<Expression>;
        if let Some(prefix) = self.prefix_functions.get(&discriminant(current)) {
            expression = prefix(self);
        } else {
            self.errors.push(format!(
                "No prefix parse function found for {:?}",
                self.current
            ));
            return None;
        }

        loop {
            match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                Some(token) => {
                    if precedence >= token.precedence().clone() {
                        break;
                    }
                    let infix = self.infix_functions.get(&discriminant(token));
                    if infix.is_none() {
                        return expression;
                    }
                    self.current = self.lexer.peek().cloned();
                    self.lexer.next();
                    expression = infix.unwrap()(self, Box::new(expression.unwrap()));
                }
                _ => break,
            }
        }
        return expression;
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        if let Some(Token::Identifier(name)) = &self.current {
            return Some(Expression::Identifier {
                name: name.to_string(),
            });
        }
        None
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let Some(Token::Integer(value)) = &self.current {
            return Some(Expression::Integer {
                value: value.to_owned(),
            });
        }
        None
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        match &self.current {
            Some(Token::True) => Some(Expression::Boolean { value: true }),
            Some(Token::False) => Some(Expression::Boolean { value: false }),
            _ => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next();
        let expression = self.parse_expression(Precedence::Lowest);
        match self.lexer.peek() {
            Some(Token::RParen) => {
                self.next();
            }
            _ => self.peek_error(Token::RParen),
        };
        return expression;
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator: Token;
        let value: Box<Expression>;
        if let Some(token) = &self.current {
            operator = token.clone();
            self.next();
        } else {
            self.errors.push(format!(
                "No prefix expression found for token {:?}",
                &self.current
            ));
            return None;
        }
        if let Some(expression) = self.parse_expression(Precedence::Prefix) {
            value = Box::new(expression);
        } else {
            self.errors.push(String::from("Missing expression!"));
            return None;
        }
        return Some(Expression::PrefixExpression { operator, value });
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Option<Expression> {
        let operator: Token;
        if let Some(token) = &self.current {
            operator = token.clone();
            self.next();
        } else {
            self.errors.push(format!(
                "No infix expression found for token {:?}",
                &self.current
            ));
            return None;
        }
        if let Some(right) = self.parse_expression(operator.precedence().clone()) {
            return Some(Expression::InfixExpression {
                left,
                operator,
                right: Box::new(right),
            });
        } else {
            self.errors.push(String::from("Missing expression!"));
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::ast::{Expression, Statement},
        token::{
            lexer::Lexer,
            types::Token::{self, Asterisk, Bang, Equal, Minus, NotEqual, Plus, Slash, GT, LT},
        },
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
                Statement::Let { name, value: _ } => {
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
                Statement::Return { value: _ } => {
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

    fn assert_integer_expression(expression: &Expression, expected: usize) {
        match expression {
            Expression::Integer { value } => {
                assert_eq!(expected, value.to_owned());
            }
            _ => panic!("Expression was not an integer!"),
        }
    }

    fn assert_boolean_expression(expression: &Expression, expected: bool) {
        match expression {
            Expression::Boolean { value } => {
                assert_eq!(expected, value.to_owned());
            }
            _ => panic!("Expression was not an boolean!"),
        }
    }

    #[test]
    fn prefix_expressions() {
        let scenarios = vec![("!5;", Bang, 5), ("-15;", Minus, 15)];

        for scenario in scenarios.iter() {
            let lexer = Lexer::new(scenario.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(Vec::<String>::new(), parser.errors);
            assert_eq!(1, program.statements.len());
            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression {
                    value: Expression::PrefixExpression { operator, value },
                } => {
                    // Complicated dereferencing required to traverse into box
                    assert_integer_expression(&*value, scenario.2);
                    assert_eq!(&scenario.1, operator);
                }
                _ => panic!("Statement was not a prefix expression!"),
            }
        }
    }

    #[test]
    fn infix_expressions() {
        let scenarios = vec![
            ("5 + 5", 5, Plus, 5),
            ("5 - 5", 5, Minus, 5),
            ("5 * 5", 5, Asterisk, 5),
            ("5 / 5", 5, Slash, 5),
            ("5 > 5", 5, GT, 5),
            ("5 < 5", 5, LT, 5),
            ("5 == 5", 5, Equal, 5),
            ("5 != 5", 5, NotEqual, 5),
        ];

        for scenario in scenarios.iter() {
            let lexer = Lexer::new(scenario.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(Vec::<String>::new(), parser.errors);
            assert_eq!(1, program.statements.len());
            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression {
                    value:
                        Expression::InfixExpression {
                            left,
                            operator,
                            right,
                        },
                } => {
                    // Complicated dereferencing required to traverse into box
                    assert_integer_expression(&*left, scenario.1);
                    assert_eq!(&scenario.2, operator);
                    assert_integer_expression(&*right, scenario.3);
                }
                _ => panic!("Statement was not a prefix expression!"),
            }
        }
    }

    #[test]
    fn fixed_boolean_expressions() {
        let prefix_scenarios = vec![("!true;", Bang, true), ("!false;", Bang, false)];

        for scenario in prefix_scenarios.iter() {
            let lexer = Lexer::new(scenario.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(Vec::<String>::new(), parser.errors);
            assert_eq!(1, program.statements.len());
            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression {
                    value: Expression::PrefixExpression { operator, value },
                } => {
                    // Complicated dereferencing required to traverse into box
                    assert_boolean_expression(&*value, scenario.2);
                    assert_eq!(&scenario.1, operator);
                }
                _ => panic!("Statement was not a prefix expression!"),
            }
        }

        let infix_scenarios = vec![
            ("true == true", true, Equal, true),
            ("true != false", true, NotEqual, false),
            ("false == false", false, Equal, false),
        ];

        for scenario in infix_scenarios.iter() {
            let lexer = Lexer::new(scenario.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(Vec::<String>::new(), parser.errors);
            assert_eq!(1, program.statements.len());
            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression {
                    value:
                        Expression::InfixExpression {
                            left,
                            operator,
                            right,
                        },
                } => {
                    // Complicated dereferencing required to traverse into box
                    assert_boolean_expression(&*left, scenario.1);
                    assert_eq!(&scenario.2, operator);
                    assert_boolean_expression(&*right, scenario.3);
                }
                _ => panic!("Statement was not a prefix expression!"),
            }
        }
    }

    #[test]
    fn operator_precedence() {
        let scenarios = vec![
            (
                "-a * b",
                vec![infix(prefix(Minus, ident("a")), Asterisk, ident("b"))],
            ),
            ("!-a", vec![prefix(Bang, prefix(Minus, ident("a")))]),
            (
                "a + b + c",
                vec![infix(infix(ident("a"), Plus, ident("b")), Plus, ident("c"))],
            ),
            (
                "a + b - c",
                vec![infix(
                    infix(ident("a"), Plus, ident("b")),
                    Minus,
                    ident("c"),
                )],
            ),
            (
                "a * b * c",
                vec![infix(
                    infix(ident("a"), Asterisk, ident("b")),
                    Asterisk,
                    ident("c"),
                )],
            ),
            (
                "a * b / c",
                vec![infix(
                    infix(ident("a"), Asterisk, ident("b")),
                    Slash,
                    ident("c"),
                )],
            ),
            (
                "a + b * c + d / e - f",
                vec![infix(
                    infix(
                        infix(ident("a"), Plus, infix(ident("b"), Asterisk, ident("c"))),
                        Plus,
                        infix(ident("d"), Slash, ident("e")),
                    ),
                    Minus,
                    ident("f"),
                )],
            ),
            (
                "3 + 4; -5 * 5",
                vec![
                    infix(int(3), Plus, int(4)),
                    infix(prefix(Minus, int(5)), Asterisk, int(5)),
                ],
            ),
            (
                "5 > 4 != 3 < 4",
                vec![infix(
                    infix(int(5), GT, int(4)),
                    NotEqual,
                    infix(int(3), LT, int(4)),
                )],
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                vec![infix(
                    infix(int(3), Plus, infix(int(4), Asterisk, int(5))),
                    Equal,
                    infix(
                        infix(int(3), Asterisk, int(1)),
                        Plus,
                        infix(int(4), Asterisk, int(5)),
                    ),
                )],
            ),
            ("true", vec![bool(true)]),
            ("false", vec![bool(false)]),
            (
                "3 > 5 == false",
                vec![infix(infix(int(3), GT, int(5)), Equal, bool(false))],
            ),
            (
                "3 < 5 == true",
                vec![infix(infix(int(3), LT, int(5)), Equal, bool(true))],
            ),
            (
                "1 + (2 + 3) + 4",
                vec![infix(
                    infix(int(1), Plus, infix(int(2), Plus, int(3))),
                    Plus,
                    int(4),
                )],
            ),
            (
                "(5 + 5) * 2",
                vec![infix(infix(int(5), Plus, int(5)), Asterisk, int(2))],
            ),
            (
                "2 / (5 + 5)",
                vec![infix(int(2), Slash, infix(int(5), Plus, int(5)))],
            ),
            ("-(5 + 5)", vec![prefix(Minus, infix(int(5), Plus, int(5)))]),
            (
                "!(true == true)",
                vec![prefix(Bang, infix(bool(true), Equal, bool(true)))],
            ),
        ];

        for (scenario, expected) in scenarios.iter() {
            let lexer = Lexer::new(scenario);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            assert_eq!(Vec::<String>::new(), parser.errors);
            assert_eq!(expected.len(), program.statements.len());
            for (i, statement) in program.statements.iter().enumerate() {
                let expected_statement = Statement::Expression {
                    value: expected.get(i).unwrap().clone(),
                };
                assert_eq!(
                    expected_statement, *statement,
                    "Failure on scenario {}, expected: {:?}, actual: {:?}",
                    scenario, expected_statement, *statement
                );
            }
        }
    }

    fn ident(name: &str) -> Expression {
        Expression::Identifier {
            name: String::from(name),
        }
    }

    fn int(value: usize) -> Expression {
        Expression::Integer { value }
    }

    fn bool(value: bool) -> Expression {
        Expression::Boolean { value }
    }

    fn prefix(operator: Token, value: Expression) -> Expression {
        Expression::PrefixExpression {
            operator,
            value: Box::new(value),
        }
    }

    fn infix(left: Expression, operator: Token, right: Expression) -> Expression {
        Expression::InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    #[test]
    fn boolean_expressions() {
        let input = "true;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(1, program.statements.len());
        let statement = program.statements.get(0).unwrap();
        match statement {
            Statement::Expression {
                value: Expression::Boolean { value },
            } => {
                assert_eq!(true, value.to_owned());
            }
            _ => panic!("Statement was not a standalone expression!"),
        }
    }
}
