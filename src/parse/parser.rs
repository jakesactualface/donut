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
        parser.register_prefix(Token::If, move |p| p.parse_if_expression());
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

    fn parse_if_expression(&mut self) -> Option<Expression> {
        // TODO: Clean this up
        match self.lexer.peek() {
            Some(Token::LParen) => {
                self.next();
            }
            _ => {
                self.peek_error(Token::LParen);
                return None;
            }
        };
        self.next();
        let condition = self.parse_expression(Precedence::Lowest).unwrap();
        match self.lexer.peek() {
            Some(Token::RParen) => {
                self.next();
            }
            _ => {
                self.peek_error(Token::RParen);
                return None;
            }
        };
        match self.lexer.peek() {
            Some(Token::LBrace) => {
                self.next();
            }
            _ => {
                self.peek_error(Token::LBrace);
                return None;
            }
        };
        let consequence = self.parse_block_statement().unwrap();

        let mut alternative = None;
        if let Some(Token::Else) = self.lexer.peek() {
            self.next();
            match self.lexer.peek() {
                Some(Token::LBrace) => {
                    self.next();
                }
                _ => {
                    self.peek_error(Token::LBrace);
                    return None;
                }
            };
            alternative = Some(Box::new(self.parse_block_statement().unwrap()));
        }

        return Some(Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        });
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut statements: Vec<Statement> = vec![];
        loop {
            match self.next() {
                Some(Token::RBrace) => break,
                Some(_) => {
                    if let Some(statement) = self.parse_statement() {
                        statements.push(statement);
                    }
                }
                _ => {
                    self.errors.push(String::from("Expected end of block!"));
                    return None;
                }
            }
        }
        Some(Statement::Block { statements })
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

    fn assert_expression_scenarios(scenario: &&str, expected_expressions: &Vec<Expression>) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(expected_expressions.len(), program.statements.len());
        for (i, statement) in program.statements.iter().enumerate() {
            let expected_statement = Statement::Expression {
                value: expected_expressions.get(i).unwrap().clone(),
            };
            assert_eq!(
                expected_statement, *statement,
                "Failure on scenario {}, expected: {:?}, actual: {:?}",
                scenario, expected_statement, *statement
            );
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

    fn conditional(
        condition: Expression,
        consequences: Vec<Statement>,
        alternatives: Option<Vec<Statement>>,
    ) -> Expression {
        let map_statements_to_expressions = |s: Vec<Statement>| {
            s.iter()
                .map(|c| match c {
                    Statement::Expression { value } => Statement::Expression {
                        value: value.clone(),
                    },
                    _ => panic!("Expected expression but found {:?}", c),
                })
                .collect()
        };
        let alt = match alternatives {
            Some(statement) => Some(Box::new(Statement::Block {
                statements: map_statements_to_expressions(statement),
            })),
            _ => None,
        };
        Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(Statement::Block {
                statements: map_statements_to_expressions(consequences),
            }),
            alternative: alt,
        }
    }

    #[test]
    fn prefix_expressions() {
        let scenarios = vec![
            ("!5;", vec![prefix(Bang, int(5))]),
            ("-15;", vec![prefix(Minus, int(15))]),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn infix_expressions() {
        let scenarios = vec![
            ("5 + 5", vec![infix(int(5), Plus, int(5))]),
            ("5 - 5", vec![infix(int(5), Minus, int(5))]),
            ("5 * 5", vec![infix(int(5), Asterisk, int(5))]),
            ("5 / 5", vec![infix(int(5), Slash, int(5))]),
            ("5 > 5", vec![infix(int(5), GT, int(5))]),
            ("5 < 5", vec![infix(int(5), LT, int(5))]),
            ("5 == 5", vec![infix(int(5), Equal, int(5))]),
            ("5 != 5", vec![infix(int(5), NotEqual, int(5))]),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
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
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn boolean_expressions() {
        let scenarios = vec![
            ("true;", vec![bool(true)]),
            ("!true;", vec![prefix(Bang, bool(true))]),
            ("!false;", vec![prefix(Bang, bool(false))]),
            ("true == true", vec![infix(bool(true), Equal, bool(true))]),
            (
                "true != false",
                vec![infix(bool(true), NotEqual, bool(false))],
            ),
            (
                "false == false",
                vec![infix(bool(false), Equal, bool(false))],
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn if_expressions() {
        let scenarios = vec![
            (
                "if (x < y) { x }",
                vec![conditional(
                    infix(ident("x"), LT, ident("y")),
                    vec![Statement::Expression { value: ident("x") }],
                    None,
                )],
            ),
            (
                "if (x < y) { x } else { y }",
                vec![conditional(
                    infix(ident("x"), LT, ident("y")),
                    vec![Statement::Expression { value: ident("x") }],
                    Some(vec![Statement::Expression { value: ident("y") }]),
                )],
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }
}
