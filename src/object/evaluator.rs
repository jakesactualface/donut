use crate::parse::ast::{Expression, Node, Statement, ToNode};

use super::types::Object::{self, Integer, Null};

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
        Expression::Boolean { value } => todo!(),
        Expression::PrefixExpression { operator, value } => todo!(),
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => todo!(),
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
            scenario, *expected, evaluated
        );
    }

    #[test]
    fn integer_expressions() {
        let scenarios = vec![("5", Integer(5)), ("10", Integer(10))];
        for (scenario, expected) in scenarios.iter() {
            assert_object_scenario(scenario, expected);
        }
    }
}
