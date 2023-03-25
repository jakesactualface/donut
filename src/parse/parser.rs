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
    pub errors: Vec<String>,
    prefix_functions: HashMap<Discriminant<Token>, PrefixParse>,
    infix_functions: HashMap<Discriminant<Token>, InfixParse>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
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
        parser.register_prefix(Token::Integer(i64::default()), move |p| {
            p.parse_integer_literal()
        });
        parser.register_prefix(Token::String(String::default()), move |p| {
            p.parse_string_literal()
        });
        parser.register_prefix(Token::True, move |p| p.parse_boolean());
        parser.register_prefix(Token::False, move |p| p.parse_boolean());
        parser.register_prefix(Token::Bang, move |p| p.parse_prefix_expression());
        parser.register_prefix(Token::Minus, move |p| p.parse_prefix_expression());
        parser.register_prefix(Token::LParen, move |p| p.parse_grouped_expression());
        parser.register_prefix(Token::If, move |p| p.parse_if_expression());
        parser.register_prefix(Token::While, move |p| p.parse_while_expression());
        parser.register_prefix(Token::LBracket, move |p| p.parse_array_literal());
        parser.register_prefix(Token::LBrace, move |p| p.parse_hash_literal());
        parser.register_prefix(Token::Function, move |p| p.parse_function_literal());
        parser.register_prefix(Token::Macro, move |p| p.parse_macro_literal());
        parser.register_prefix(Token::Mutate, move |p| p.parse_mutate_expression());
        parser.register_infix(Token::Equal, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::NotEqual, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::And, move |p, e| p.parse_short_circuit_expression(e));
        parser.register_infix(Token::Or, move |p, e| p.parse_short_circuit_expression(e));
        parser.register_infix(Token::LT, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::GT, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Plus, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Minus, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Slash, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::Asterisk, move |p, e| p.parse_infix_expression(e));
        parser.register_infix(Token::LParen, move |p, e| p.parse_call_expression(e));
        parser.register_infix(Token::LBracket, move |p, e| p.parse_index_expression(e));
        parser
    }

    fn next(&mut self) -> Option<Token> {
        self.current = self.lexer.next();
        self.current.clone()
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        match self.lexer.peek() {
            Some(p) if *p == token => {
                self.next();
                true
            }
            _ => {
                self.peek_error(token);
                false
            }
        }
    }

    fn register_prefix(&mut self, token: Token, function: PrefixParse) {
        self.prefix_functions.insert(discriminant(&token), function);
    }

    fn register_infix(&mut self, token: Token, function: InfixParse) {
        self.infix_functions.insert(discriminant(&token), function);
    }

    fn peek_error(&mut self, token: Token) {
        let peeked = self.lexer.peek();
        if peeked.is_none() {
            self.errors.push(String::from("Expected additional token!"));
            return;
        }
        self.errors.push(format!(
            "Expected next token to be {:?}, got {:?} instead",
            token,
            peeked.unwrap()
        ));
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();

        while self.next().is_some() {
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

        if !self.expect_peek(Token::Assignment) {
            return None;
        }

        self.next();
        let value = self.parse_expression(Precedence::Lowest);

        if value.is_none() {
            self.errors
                .push(format!("Expected assignment value for name: {name}"));
            return None;
        }

        if Some(&Token::Semicolon) == self.lexer.peek() {
            self.next();
        }

        Some(Statement::Let {
            name,
            value: value.unwrap(),
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next();
        let value = self.parse_expression(Precedence::Lowest);

        if value.is_none() {
            self.errors.push(String::from("Expected return value!"));
            return None;
        }

        if Some(&Token::Semicolon) == self.lexer.peek() {
            self.next();
        }

        Some(Statement::Return {
            value: value.unwrap(),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let statement = self.parse_expression(Precedence::Lowest);

        statement.as_ref()?;

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
                    if expression.is_none() {
                        self.errors.push(String::from("Expected expression!"));
                        return None;
                    }

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
        expression
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

    fn parse_string_literal(&mut self) -> Option<Expression> {
        if let Some(Token::String(value)) = &self.current {
            return Some(Expression::String {
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

    fn parse_array_literal(&mut self) -> Option<Expression> {
        Some(Expression::Array {
            elements: self.parse_expression_list(Token::RBracket),
        })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut pairs: Vec<(Expression, Expression)> = vec![];

        while Some(&Token::RBrace) != self.lexer.peek() {
            self.next();
            let key = self.parse_expression(Precedence::Lowest);
            if key.is_none() {
                self.errors.push(String::from("Expected hash key!"));
                return None;
            }

            if !self.expect_peek(Token::Colon) {
                return None;
            }

            self.next();
            let value = self.parse_expression(Precedence::Lowest);
            if value.is_none() {
                self.errors
                    .push(format!("Expected value for hash key: {:?}", key.unwrap()));
                return None;
            }
            pairs.push((key.unwrap(), value.unwrap()));

            if Some(&Token::RBrace) != self.lexer.peek() && !self.expect_peek(Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(Token::RBrace) {
            return None;
        }

        Some(Expression::Hash { pairs })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        let block = self.parse_block_statement();
        if block.is_none() {
            self.errors.push(String::from("Expected function body!"));
            return None;
        }
        Some(Expression::Function {
            parameters,
            body: Box::new(block.unwrap()),
        })
    }

    fn parse_macro_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        let block = self.parse_block_statement();
        if block.is_none() {
            self.errors.push(String::from("Expected macro body!"));
            return None;
        }
        Some(Expression::Macro {
            parameters,
            body: Box::new(block.unwrap()),
        })
    }

    fn parse_mutate_expression(&mut self) -> Option<Expression> {
        let target: Expression;

        self.next();
        match self.parse_expression(Precedence::Prefix) {
            Some(Expression::Identifier { name }) => {
                target = Expression::Identifier { name };
            }
            Some(Expression::Index { value, index }) => {
                target = Expression::Index { value, index };
            }
            Some(token) => {
                self.errors
                    .push(format!("Mutation not supported for token {token:?}"));
                return None;
            }
            None => {
                self.errors.push("Expected mutation target!".to_string());
                return None;
            }
        }

        if !self.expect_peek(Token::Assignment) {
            return None;
        }

        self.next();
        let value = self.parse_expression(Precedence::Lowest);
        if value.is_none() {
            self.errors
                .push(String::from("Expected value in mutation statement!"));
            return None;
        }
        Some(Expression::Mutation {
            target: Box::new(target),
            value: Box::new(value.unwrap()),
        })
    }

    fn parse_function_parameters(&mut self) -> Vec<Expression> {
        let mut parameters = vec![];
        if let Some(Token::RParen) = self.next() {
            return parameters;
        }

        let current_to_identifier = |c: &Option<Token>| match c {
            Some(Token::Identifier(name)) => {
                Some(Expression::Identifier {
                    name: name.to_owned(),
                })
            }
            _ => {
                None
            }
        };

        let first_param = current_to_identifier(&self.current);
        if first_param.is_none() {
            self.errors.push(String::from("Expected parameter list!"));
            return parameters;
        }
        parameters.push(first_param.unwrap());

        while Some(&Token::Comma) == self.lexer.peek() {
            self.next();
            self.next();
            let next_arg = current_to_identifier(&self.current);
            if next_arg.is_none() {
                self.errors
                    .push(String::from("Expected parameter after comma!"));
                return parameters;
            }
            parameters.push(next_arg.unwrap());
        }

        if !self.expect_peek(Token::RParen) {
            return vec![];
        }

        parameters
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next();
        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        expression
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
        Some(Expression::PrefixExpression { operator, value })
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        self.next();
        let condition = self.parse_expression(Precedence::Lowest);

        condition.as_ref()?;

        if !self.expect_peek(Token::RParen) {
            return None;
        }
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        let consequence = self.parse_block_statement();

        consequence.as_ref()?;

        let mut alternative = None;
        if let Some(Token::Else) = self.lexer.peek() {
            self.next();
            if !self.expect_peek(Token::LBrace) {
                return None;
            }
            if let Some(statement) = self.parse_block_statement() {
                alternative = Some(Box::new(statement));
            } else {
                return None;
            }
        }

        Some(Expression::IfExpression {
            condition: Box::new(condition.unwrap()),
            consequence: Box::new(consequence.unwrap()),
            alternative,
        })
    }

    fn parse_while_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        self.next();
        let condition = self.parse_expression(Precedence::Lowest);

        condition.as_ref()?;

        if !self.expect_peek(Token::RParen) {
            return None;
        }
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        let body = self.parse_block_statement();

        body.as_ref()?;

        Some(Expression::WhileExpression {
            condition: Box::new(condition.unwrap()),
            body: Box::new(body.unwrap()),
        })
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
            Some(Expression::InfixExpression {
                left,
                operator,
                right: Box::new(right),
            })
        } else {
            self.errors.push(String::from("Missing expression!"));
            None
        }
    }

    fn parse_short_circuit_expression(&mut self, left: Box<Expression>) -> Option<Expression> {
        let operator: Token;
        if let Some(token) = &self.current {
            operator = token.clone();
            self.next();
        } else {
            self.errors.push(format!(
                "No short circuit expression found for token {:?}",
                &self.current
            ));
            return None;
        }
        if let Some(right) = self.parse_expression(operator.precedence().clone()) {
            Some(Expression::ShortCircuitExpression {
                left,
                operator,
                right: Box::new(right),
            })
        } else {
            self.errors.push(String::from("Missing expression!"));
            None
        }
    }

    fn parse_call_expression(&mut self, function: Box<Expression>) -> Option<Expression> {
        Some(Expression::Call {
            function,
            arguments: self.parse_expression_list(Token::RParen),
        })
    }

    fn parse_expression_list(&mut self, terminator: Token) -> Vec<Expression> {
        let mut arguments = vec![];
        if self.next() == Some(terminator.clone()) {
            return arguments;
        }

        let first_arg = self.parse_expression(Precedence::Lowest);
        if first_arg.is_none() {
            self.errors.push(String::from("Expected argument list!"));
            return arguments;
        }
        arguments.push(first_arg.unwrap());

        while Some(&Token::Comma) == self.lexer.peek() {
            self.next();
            self.next();
            let list_item = self.parse_expression(Precedence::Lowest);
            if list_item.is_none() {
                self.errors
                    .push(String::from("Missing expression after comma!"));
                return arguments;
            }
            arguments.push(list_item.unwrap());
        }

        if !self.expect_peek(terminator) {
            return vec![];
        }

        arguments
    }

    fn parse_index_expression(&mut self, value: Box<Expression>) -> Option<Expression> {
        self.next();

        let index = self.parse_expression(Precedence::Lowest);
        if index.is_none() {
            self.errors.push(String::from("Missing index expression!"));
            return None;
        }

        if !self.expect_peek(Token::RBracket) {
            self.errors.push(String::from("Missing ending bracket!"));
            return None;
        }

        Some(Expression::Index {
            value,
            index: Box::new(index.unwrap()),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::ast::{Expression, Statement},
        token::{
            lexer::Lexer,
            types::Token::{
                self, And, Asterisk, Bang, Equal, Minus, NotEqual, Or, Plus, Slash, GT, LT,
            },
        },
    };
    use pretty_assertions::assert_eq;

    use super::Parser;

    #[test]
    fn let_statements_only() {
        let scenarios = vec![
            (
                "let x = y;",
                Statement::Let {
                    name: String::from("x"),
                    value: ident("y"),
                },
            ),
            (
                "let y = 10;",
                Statement::Let {
                    name: String::from("y"),
                    value: int(10),
                },
            ),
            (
                "let foobar = 838383;",
                Statement::Let {
                    name: String::from("foobar"),
                    value: int(838383),
                },
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_statement_scenarios(scenario, expected);
        }
    }

    #[test]
    fn return_statements_only() {
        let scenarios = vec![
            ("return 5;", Statement::Return { value: int(5) }),
            ("return 10;", Statement::Return { value: int(10) }),
            ("return 993322;", Statement::Return { value: int(993322) }),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_statement_scenarios(scenario, expected);
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

    fn assert_statement_scenarios(scenario: &&str, expected_statement: &Statement) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        assert_eq!(1, program.statements.len());
        let statement = program.statements.get(0).unwrap();
        assert_eq!(
            expected_statement, statement,
            "Failure on scenario {}, expected: {:#?}, actual: {:#?}",
            scenario, expected_statement, statement
        );
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
                "Failure on scenario {}, expected: {:#?}, actual: {:#?}",
                scenario, expected_statement, *statement
            );
        }
    }

    fn ident(name: &str) -> Expression {
        Expression::Identifier {
            name: String::from(name),
        }
    }

    fn int(value: i64) -> Expression {
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

    fn short_circuit(left: Expression, operator: Token, right: Expression) -> Expression {
        Expression::ShortCircuitExpression {
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
        let alt = alternatives.map(|statement| Box::new(Statement::Block {
                statements: statement,
            }));
        Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(Statement::Block {
                statements: consequences,
            }),
            alternative: alt,
        }
    }

    fn while_literal(condition: Expression, body: Vec<Statement>) -> Expression {
        Expression::WhileExpression {
            condition: Box::new(condition),
            body: Box::new(Statement::Block { statements: body }),
        }
    }

    fn array(elements: Vec<Expression>) -> Expression {
        Expression::Array { elements }
    }

    fn hash(pairs: Vec<(Expression, Expression)>) -> Expression {
        Expression::Hash { pairs }
    }

    fn index(value: Expression, index: Expression) -> Expression {
        Expression::Index {
            value: Box::new(value),
            index: Box::new(index),
        }
    }

    fn fn_literal(parameters: Vec<Expression>, body: Vec<Statement>) -> Expression {
        Expression::Function {
            parameters,
            body: Box::new(Statement::Block { statements: body }),
        }
    }

    fn string(value: &str) -> Expression {
        Expression::String {
            value: String::from(value),
        }
    }

    fn call(function: Expression, arguments: Vec<Expression>) -> Expression {
        Expression::Call {
            function: Box::new(function),
            arguments,
        }
    }

    fn macro_literal(parameters: Vec<Expression>, body: Vec<Statement>) -> Expression {
        Expression::Macro {
            parameters,
            body: Box::new(Statement::Block { statements: body }),
        }
    }

    fn mutation(target: Expression, value: Expression) -> Expression {
        Expression::Mutation {
            target: Box::new(target),
            value: Box::new(value),
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
            (
                "a + add(b * c) + d",
                vec![infix(
                    infix(
                        ident("a"),
                        Plus,
                        call(ident("add"), vec![infix(ident("b"), Asterisk, ident("c"))]),
                    ),
                    Plus,
                    ident("d"),
                )],
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                vec![call(
                    ident("add"),
                    vec![
                        ident("a"),
                        ident("b"),
                        int(1),
                        infix(int(2), Asterisk, int(3)),
                        infix(int(4), Plus, int(5)),
                        call(ident("add"), vec![int(6), infix(int(7), Asterisk, int(8))]),
                    ],
                )],
            ),
            (
                "add(a + b + c * d / f + g)",
                vec![call(
                    ident("add"),
                    vec![infix(
                        infix(
                            infix(ident("a"), Plus, ident("b")),
                            Plus,
                            infix(infix(ident("c"), Asterisk, ident("d")), Slash, ident("f")),
                        ),
                        Plus,
                        ident("g"),
                    )],
                )],
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                vec![infix(
                    infix(
                        ident("a"),
                        Asterisk,
                        index(
                            array(vec![int(1), int(2), int(3), int(4)]),
                            infix(ident("b"), Asterisk, ident("c")),
                        ),
                    ),
                    Asterisk,
                    ident("d"),
                )],
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                vec![call(
                    ident("add"),
                    vec![
                        infix(ident("a"), Asterisk, index(ident("b"), int(2))),
                        index(ident("b"), int(1)),
                        infix(int(2), Asterisk, index(array(vec![int(1), int(2)]), int(1))),
                    ],
                )],
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
    fn short_circuit_expressions() {
        let scenarios = vec![
            (
                "true && true",
                vec![short_circuit(bool(true), And, bool(true))],
            ),
            (
                "false || false",
                vec![short_circuit(bool(false), Or, bool(false))],
            ),
            (
                "(1 < 2) && true",
                vec![short_circuit(infix(int(1), LT, int(2)), And, bool(true))],
            ),
            (
                "false || 1 < 2",
                vec![short_circuit(bool(false), Or, infix(int(1), LT, int(2)))],
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

    #[test]
    fn while_expressions() {
        let scenarios = vec![(
            "while (x < y) { x }",
            vec![while_literal(
                infix(ident("x"), LT, ident("y")),
                vec![Statement::Expression { value: ident("x") }],
            )],
        )];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn function_literals() {
        let scenarios = vec![
            (
                "fn(x, y) { x + y };",
                vec![fn_literal(
                    vec![ident("x"), ident("y")],
                    vec![Statement::Expression {
                        value: infix(ident("x"), Plus, ident("y")),
                    }],
                )],
            ),
            ("fn() {};", vec![fn_literal(vec![], vec![])]),
            ("fn(x) {};", vec![fn_literal(vec![ident("x")], vec![])]),
            (
                "fn(x, y, z) {};",
                vec![fn_literal(vec![ident("x"), ident("y"), ident("z")], vec![])],
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn call_expressions() {
        let scenarios = vec![
            (
                "add(1, 2 * 3, 4 + 5);",
                vec![call(
                    ident("add"),
                    vec![
                        int(1),
                        infix(int(2), Asterisk, int(3)),
                        infix(int(4), Plus, int(5)),
                    ],
                )],
            ),
            ("add();", vec![call(ident("add"), vec![])]),
            ("add(1);", vec![call(ident("add"), vec![int(1)])]),
            ("add(a);", vec![call(ident("add"), vec![ident("a")])]),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn string_literals() {
        let scenarios = vec![
            ("\"hello world\"", vec![string("hello world")]),
            ("\"foo\\\"bar\"", vec![string("foo\"bar")]),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn array_literals() {
        let scenarios = vec![
            (
                "[1, 2 * 2, 3 + 3]",
                vec![array(vec![
                    int(1),
                    infix(int(2), Asterisk, int(2)),
                    infix(int(3), Plus, int(3)),
                ])],
            ),
            ("[]", vec![array(vec![])]),
            ("[\"test\"]", vec![array(vec![string("test")])]),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn index_expressions() {
        let scenarios = vec![(
            "myArray[1 + 1]",
            vec![index(ident("myArray"), infix(int(1), Plus, int(1)))],
        )];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn hash_literals() {
        let scenarios = vec![
            (
                "{\"one\": 1, \"two\": 2, \"three\": 3}",
                vec![hash(vec![
                    (string("one"), int(1)),
                    (string("two"), int(2)),
                    (string("three"), int(3)),
                ])],
            ),
            ("{}", vec![hash(vec![])]),
            (
                "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
                vec![hash(vec![
                    (string("one"), infix(int(0), Plus, int(1))),
                    (string("two"), infix(int(10), Minus, int(8))),
                    (string("three"), infix(int(15), Slash, int(5))),
                ])],
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn macro_literals() {
        let scenarios = vec![(
            "macro(x, y) { x + y; }",
            vec![macro_literal(
                vec![ident("x"), ident("y")],
                vec![Statement::Expression {
                    value: infix(ident("x"), Plus, ident("y")),
                }],
            )],
        )];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }

    #[test]
    fn mutation_expressions() {
        let scenarios = vec![
            ("mut a = 1;", vec![mutation(ident("a"), int(1))]),
            ("mut a = b;", vec![mutation(ident("a"), ident("b"))]),
            (
                "mut a = (1 + 1);",
                vec![mutation(ident("a"), infix(int(1), Plus, int(1)))],
            ),
        ];
        for (scenario, expected) in scenarios.iter() {
            assert_expression_scenarios(scenario, expected);
        }
    }
}
