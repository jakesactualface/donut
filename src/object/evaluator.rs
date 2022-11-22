// TODO: Remove this
#![allow(unused_variables)]
use crate::{
    parse::ast::{Expression, Node, Statement, ToNode},
    token::types::Token,
};

use super::types::Object::{self, Boolean, Integer, Null};

const NULL: Object = Null;
const TRUE: Object = Boolean(true);
const FALSE: Object = Boolean(false);

pub fn eval(node: impl ToNode) -> Object {
    match node.to_node() {
        Node::Statement(s) => eval_statement(s),
        Node::Expression(e) => eval_expression(e),
    }
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Let { name, value } => todo!(),
        Statement::Return { value } => todo!(),
        Statement::Expression { value } => eval_expression(value),
        Statement::Block { statements } => {
            let mut return_object = Null;
            for statement in statements.into_iter() {
                return_object = eval_statement(statement);
            }
            return return_object;
        }
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Identifier { name } => todo!(),
        Expression::Integer { value } => Integer(value),
        Expression::Boolean { value } => native_bool_to_boolean(value),
        Expression::PrefixExpression { operator, value } => {
            let evaluated = eval(*value);
            return eval_prefix_expression(operator, evaluated);
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => {
            let evaluated_left = eval(*left);
            let evaluated_right = eval(*right);
            return eval_infix_expression(operator, evaluated_left, evaluated_right);
        }
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => todo!(),
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

fn eval_prefix_expression(operator: Token, value: Object) -> Object {
    match operator {
        Token::Bang => match value {
            Boolean(true) => FALSE,
            Boolean(false) => TRUE,
            Integer(0) => TRUE,
            Null => TRUE,
            _ => FALSE,
        },
        Token::Minus => match value {
            Integer(i) => Integer(-i),
            _ => NULL,
        },
        _ => return NULL,
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Object {
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
            _ => return Null,
        },
        (Token::Equal, l, r) => native_bool_to_boolean(l == r),
        (Token::NotEqual, l, r) => native_bool_to_boolean(l != r),
        _ => return Null,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{
            evaluator::eval,
            types::Object::{self, Boolean, Integer},
        },
        parse::parser::Parser,
        token::lexer::Lexer,
    };
    use pretty_assertions::assert_eq;

    fn assert_object_scenario(scenario: &&str, expected: &Object) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);

        let evaluated = eval(program);
        assert_eq!(
            *expected, evaluated,
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
        for (scenario, expected) in scenarios.iter() {
            assert_object_scenario(scenario, &Integer(*expected));
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
        for (scenario, expected) in scenarios.iter() {
            assert_object_scenario(scenario, &Boolean(*expected));
        }
    }
}
