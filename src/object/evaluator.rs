// TODO: Remove this
#![allow(unused_variables)]
use std::mem::discriminant;

use crate::{
    parse::ast::{Expression, Node, Statement, ToNode},
    token::types::Token,
};

use super::types::{
    Environment,
    Object::{self, Boolean, Error, Integer, Null, Return},
};

const NULL: Object = Null;
const TRUE: Object = Boolean(true);
const FALSE: Object = Boolean(false);

pub fn eval(node: impl ToNode, env: &mut Environment) -> Object {
    match node.to_node() {
        Node::Program(statements) => {
            let mut evaluated = NULL;
            for statement in statements.into_iter() {
                evaluated = eval(statement, env);
                match evaluated {
                    // Statement was a "return" statement, return unwrapped
                    Return(object) => {
                        return *object;
                    }
                    // Statement was an "error" statement, propagate
                    Error(_) => {
                        return evaluated;
                    }
                    _ => continue,
                };
            }
            return evaluated;
        }
        Node::Statement(s) => eval_statement(s, env),
        Node::Expression(e) => eval_expression(e, env),
    }
}

fn eval_statement(statement: Statement, env: &mut Environment) -> Object {
    match statement {
        Statement::Let { name, value } => {
            let evaluated = eval(value, env);
            return match evaluated {
                Error(_) => evaluated,
                _ => env.set(name, evaluated),
            };
        }
        Statement::Return { value } => {
            let evaluated = eval(value, env);
            return match evaluated {
                Error(_) => evaluated,
                _ => Return(Box::new(evaluated)),
            };
        }
        Statement::Expression { value } => eval_expression(value, env),
        Statement::Block { statements } => {
            let mut evaluated = NULL;
            for statement in statements.into_iter() {
                evaluated = eval_statement(statement, env);
                match evaluated {
                    // Statement was a "return" statement, propagate
                    Return(_) => {
                        return evaluated;
                    }
                    // Statement was an "error" statement, propagate
                    Error(_) => {
                        return evaluated;
                    }
                    _ => continue,
                };
            }
            return evaluated;
        }
    }
}

fn eval_expression(expression: Expression, env: &mut Environment) -> Object {
    match expression {
        Expression::Identifier { name } => eval_identifier(name, env),
        Expression::Integer { value } => Integer(value),
        Expression::Boolean { value } => native_bool_to_boolean(value),
        Expression::PrefixExpression { operator, value } => {
            let evaluated = eval(*value, env);
            return match evaluated {
                Error(_) => evaluated,
                _ => eval_prefix_expression(operator, evaluated, env),
            };
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => match (eval(*left, env), eval(*right, env)) {
            (Error(left), _) => {
                return Error(left);
            }
            (_, Error(right)) => {
                return Error(right);
            }
            (left, right) => {
                return eval_infix_expression(operator, left, right, env);
            }
        },
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            return eval_if_expression(*condition, consequence, alternative, env);
        }
        Expression::Function { parameters, body } => todo!(),
        Expression::Call {
            function,
            arguments,
        } => todo!(),
    }
}

fn native_bool_to_boolean(input: bool) -> Object {
    if input {
        return TRUE;
    }
    return FALSE;
}

fn eval_identifier(name: String, env: &mut Environment) -> Object {
    if let Some(object) = env.get(&name) {
        return object.to_owned();
    }
    return Error(format!("identifier not found: {name}"));
}

fn eval_prefix_expression(operator: Token, value: Object, env: &mut Environment) -> Object {
    match operator {
        Token::Bang => match value {
            TRUE => FALSE,
            FALSE => TRUE,
            Integer(0) => TRUE,
            NULL => TRUE,
            _ => FALSE,
        },
        Token::Minus => match value {
            Integer(i) => Integer(-i),
            value => Error(format!("unknown operator: {:?}{:?}", operator, value)),
        },
        operator => return Error(format!("unknown operator: {:?}{:?}", operator, value)),
    }
}

fn eval_infix_expression(
    operator: Token,
    left: Object,
    right: Object,
    env: &mut Environment,
) -> Object {
    match (operator, left, right) {
        (operator, Integer(l), Integer(r)) => match operator {
            Token::Plus => Integer(l + r),
            Token::Minus => Integer(l - r),
            Token::Asterisk => Integer(l * r),
            Token::Slash => Integer(l / r),
            Token::LT => native_bool_to_boolean(l < r),
            Token::GT => native_bool_to_boolean(l > r),
            Token::Equal => native_bool_to_boolean(l == r),
            Token::NotEqual => native_bool_to_boolean(l != r),
            operator => Error(format!("unknown operator: {:?} {:?} {:?}", l, operator, r)),
        },
        (Token::Equal, l, r) => native_bool_to_boolean(l == r),
        (Token::NotEqual, l, r) => native_bool_to_boolean(l != r),
        (operator, l, r) if discriminant(&l) != discriminant(&r) => {
            Error(format!("type mismatch: {:?} {:?} {:?}", l, operator, r))
        }
        (operator, l, r) => Error(format!("unknown operator: {:?} {:?} {:?}", l, operator, r)),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Box<Statement>,
    alternative: Option<Box<Statement>>,
    env: &mut Environment,
) -> Object {
    let truthy = match eval(condition, env) {
        Integer(0) => false,
        Integer(1) => true,
        TRUE => true,
        FALSE => false,
        NULL => false,
        _ => true,
    };

    if truthy {
        return eval(*consequence, env);
    } else if alternative.is_some() {
        return eval(*alternative.unwrap(), env);
    }
    return NULL;
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{
            evaluator::eval,
            types::{
                Environment,
                Object::{self, Boolean, Error, Integer, Null},
            },
        },
        parse::parser::Parser,
        token::lexer::Lexer,
    };
    use pretty_assertions::assert_eq;

    fn assert_object_scenario(scenario: &str, expected: Object) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();
        assert_eq!(Vec::<String>::new(), parser.errors);

        let evaluated = eval(program, &mut env);
        assert_eq!(
            expected, evaluated,
            "Failure on scenario {}, expected: {:#?}, actual: {:#?}",
            scenario, expected, evaluated
        );
    }

    #[test]
    fn integer_expressions() {
        let scenarios = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
        }
    }

    #[test]
    fn boolean_expressions() {
        let scenarios = vec![
            ("true", true),
            ("false", false),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Boolean(expected));
        }
    }

    #[test]
    fn if_else_expressions() {
        let scenarios = vec![
            ("if (true) { 10 }", Integer(10)),
            ("if (false) { 10 }", Null),
            ("if (1) { 10 }", Integer(10)),
            ("if (1 < 2) { 10 }", Integer(10)),
            ("if (1 > 2) { 10 }", Null),
            ("if (1 > 2) { 10 } else { 20 }", Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Integer(10)),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn return_statements() {
        let scenarios = vec![
            ("return 10;", Integer(10)),
            ("return 10; 9;", Integer(10)),
            ("return 2 * 5; 9;", Integer(10)),
            ("9; return 2 * 5; 9;", Integer(10)),
            (
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
                ",
                Integer(10),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn error_handling() {
        let scenarios = vec![
            ("5 + true;", "type mismatch: Integer(5) Plus Boolean(true)"),
            (
                "5 + true; 5;",
                "type mismatch: Integer(5) Plus Boolean(true)",
            ),
            ("-true", "unknown operator: MinusBoolean(true)"),
            (
                "true + false;",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "if ( 10 > 1) { true + false; }",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }
                ",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Error(String::from(expected)));
        }
    }

    #[test]
    fn let_statements() {
        let scenarios = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
        }
    }
}
