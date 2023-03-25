use std::{cell::RefCell, rc::Rc};

use crate::parse::ast::{Expression, Node, Statement, ToNode};

use super::types::Environment;

pub type ModifierFunction = fn(Node, Rc<RefCell<Environment>>) -> Node;

pub trait Modifiable {
    fn modify(self, modifier: ModifierFunction, env: Rc<RefCell<Environment>>) -> Self;
}

impl Modifiable for Statement {
    fn modify(self, modifier: ModifierFunction, env: Rc<RefCell<Environment>>) -> Self {
        let this_statement: Statement;
        if let Node::Statement(statement) = modifier(self.to_node(), env.clone()) {
            this_statement = statement;
        } else {
            panic!("Expected Statement node!");
        }

        match this_statement {
            Statement::Expression { value } => Statement::Expression {
                value: value.modify(modifier, env),
            },
            Statement::Block { statements } => {
                let mut modified_statements: Vec<Statement> = vec![];
                for statement in statements.into_iter() {
                    modified_statements.push(statement.modify(modifier, env.clone()));
                }
                Statement::Block {
                    statements: modified_statements,
                }
            }
            Statement::Return { value } => Statement::Return {
                value: value.modify(modifier, env),
            },
            Statement::Let { name, value } => Statement::Let {
                name,
                value: value.modify(modifier, env),
            },
        }
    }
}

impl Modifiable for Expression {
    fn modify(self, modifier: ModifierFunction, env: Rc<RefCell<Environment>>) -> Self {
        let this_expression: Expression;
        if let Node::Expression(expression) = modifier(self.to_node(), env.clone()) {
            this_expression = expression;
        } else {
            panic!("Expected Expression node!");
        }

        match this_expression {
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => Expression::InfixExpression {
                left: Box::new(left.modify(modifier, env.clone())),
                operator,
                right: Box::new(right.modify(modifier, env)),
            },
            Expression::PrefixExpression { operator, value } => Expression::PrefixExpression {
                operator,
                value: Box::new(value.modify(modifier, env)),
            },
            Expression::Index { value, index } => Expression::Index {
                value: Box::new(value.modify(modifier, env.clone())),
                index: Box::new(index.modify(modifier, env)),
            },
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let modified_alternative: Option<Box<Statement>>;
                if let Some(a) = alternative {
                    modified_alternative = Some(Box::new(a.modify(modifier, env.clone())));
                } else {
                    modified_alternative = None;
                }
                Expression::IfExpression {
                    condition: Box::new(condition.modify(modifier, env.clone())),
                    consequence: Box::new(consequence.modify(modifier, env)),
                    alternative: modified_alternative,
                }
            }
            Expression::Function { parameters, body } => {
                let mut modified_parameters: Vec<Expression> = vec![];
                for parameter in parameters.into_iter() {
                    modified_parameters.push(parameter.modify(modifier, env.clone()));
                }
                Expression::Function {
                    parameters: modified_parameters,
                    body: Box::new(body.modify(modifier, env)),
                }
            }
            Expression::Array { elements } => {
                let mut modified_elements: Vec<Expression> = vec![];
                for element in elements.into_iter() {
                    modified_elements.push(element.modify(modifier, env.clone()));
                }
                Expression::Array {
                    elements: modified_elements,
                }
            }
            Expression::Hash { pairs } => {
                let mut modified_pairs: Vec<(Expression, Expression)> = vec![];
                for (key, value) in pairs.into_iter() {
                    modified_pairs.push((
                        key.modify(modifier, env.clone()),
                        value.modify(modifier, env.clone()),
                    ));
                }
                Expression::Hash {
                    pairs: modified_pairs,
                }
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let mut modified_arguments: Vec<Expression> = vec![];
                for argument in arguments.into_iter() {
                    modified_arguments.push(argument.modify(modifier, env.clone()));
                }
                Expression::Call {
                    function,
                    arguments: modified_arguments,
                }
            }
            _ => this_expression,
        }
    }
}

pub fn modify(node: Node, modifier: ModifierFunction, env: Rc<RefCell<Environment>>) -> Node {
    match node {
        Node::Statement(statement) => statement.modify(modifier, env).to_node(),
        Node::Expression(expression) => expression.modify(modifier, env).to_node(),
        Node::Program(statements) => {
            let mut modified_statements: Vec<Statement> = vec![];
            for statement in statements.into_iter() {
                modified_statements.push(statement.modify(modifier, env.clone()));
            }
            Node::Program(modified_statements)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use super::{modify, ModifierFunction};
    use crate::object::types::Environment;
    use crate::parse::ast::{Expression, Node, Statement, ToNode};
    use crate::token::types::Token::{self, Minus, Plus};

    use pretty_assertions::assert_eq;

    fn statement(expression: Expression) -> Statement {
        Statement::Expression { value: expression }
    }

    fn int(value: i64) -> Expression {
        Expression::Integer { value }
    }

    fn infix(left: Expression, operator: Token, right: Expression) -> Expression {
        Expression::InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn prefix(operator: Token, value: Expression) -> Expression {
        Expression::PrefixExpression {
            operator,
            value: Box::new(value),
        }
    }

    fn index(value: Expression, index: Expression) -> Expression {
        Expression::Index {
            value: Box::new(value),
            index: Box::new(index),
        }
    }

    fn if_expression(
        condition: Expression,
        consequence: Statement,
        alternative: Option<Box<Statement>>,
    ) -> Expression {
        Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        }
    }

    fn block(statements: Vec<Statement>) -> Statement {
        Statement::Block { statements }
    }

    fn turn_one_into_two(node: Node, _env: Rc<RefCell<Environment>>) -> Node {
        if let Node::Expression(Expression::Integer { value: 1 }) = node {
            return Node::Expression(Expression::Integer { value: 2 });
        }
        node
    }

    fn assert_modifier(actual: impl ToNode, expected: impl ToNode, modifier: ModifierFunction) {
        let expected_node = expected.to_node();
        let modified = modify(
            actual.to_node(),
            modifier,
            Rc::new(RefCell::new(Environment::new())),
        );
        assert_eq!(
            expected_node, modified,
            "Expected: {:#?}, actual: {:#?}",
            expected_node, modified
        );
    }

    #[test]
    fn modifiers() {
        let scenarios = vec![
            (statement(int(1)), statement(int(2)), turn_one_into_two),
            (statement(int(2)), statement(int(2)), turn_one_into_two),
            (
                statement(infix(int(1), Plus, int(2))),
                statement(infix(int(2), Plus, int(2))),
                turn_one_into_two,
            ),
            (
                statement(infix(int(2), Plus, int(1))),
                statement(infix(int(2), Plus, int(2))),
                turn_one_into_two,
            ),
            (
                statement(prefix(Minus, int(1))),
                statement(prefix(Minus, int(2))),
                turn_one_into_two,
            ),
            (
                statement(index(int(1), int(1))),
                statement(index(int(2), int(2))),
                turn_one_into_two,
            ),
            (
                statement(if_expression(
                    int(1),
                    block(vec![Statement::Expression { value: int(1) }]),
                    Some(Box::new(block(vec![Statement::Expression {
                        value: int(1),
                    }]))),
                )),
                statement(if_expression(
                    int(2),
                    block(vec![Statement::Expression { value: int(2) }]),
                    Some(Box::new(block(vec![Statement::Expression {
                        value: int(2),
                    }]))),
                )),
                turn_one_into_two,
            ),
            (
                Statement::Return { value: int(1) },
                Statement::Return { value: int(2) },
                turn_one_into_two,
            ),
            (
                Statement::Let {
                    name: String::from("a"),
                    value: int(1),
                },
                Statement::Let {
                    name: String::from("a"),
                    value: int(2),
                },
                turn_one_into_two,
            ),
            (
                statement(Expression::Function {
                    parameters: vec![],
                    body: Box::new(block(vec![Statement::Expression { value: int(1) }])),
                }),
                statement(Expression::Function {
                    parameters: vec![],
                    body: Box::new(block(vec![Statement::Expression { value: int(2) }])),
                }),
                turn_one_into_two,
            ),
            (
                statement(Expression::Array {
                    elements: vec![int(1), int(1)],
                }),
                statement(Expression::Array {
                    elements: vec![int(2), int(2)],
                }),
                turn_one_into_two,
            ),
            (
                statement(Expression::Hash {
                    pairs: vec![(int(1), int(1))],
                }),
                statement(Expression::Hash {
                    pairs: vec![(int(2), int(2))],
                }),
                turn_one_into_two,
            ),
        ];
        for (scenario, expected, modifier) in scenarios.into_iter() {
            assert_modifier(scenario, expected, modifier);
        }
    }
}
