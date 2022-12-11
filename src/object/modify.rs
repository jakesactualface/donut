use crate::parse::ast::{Expression, Node, Statement, ToNode};

pub type ModifierFunction = fn(Node) -> Node;

pub fn modify(node: Node, modifier: ModifierFunction) -> Node {
    return match node {
        Node::Statement(statement) => modify_statement(statement, modifier),
        Node::Expression(expression) => modify_expression(expression, modifier),
        Node::Program(_) => todo!(),
    };
}

pub fn modify_statement(statement: Statement, modifier: ModifierFunction) -> Node {
    match statement {
        Statement::Expression { value } => modifier(value.to_node()),
        _ => todo!(),
    }
}

pub fn modify_expression(expression: Expression, modifier: ModifierFunction) -> Node {
    return match expression {
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => match (
            modify(left.to_node(), modifier),
            modify(right.to_node(), modifier),
        ) {
            (Node::Expression(l), Node::Expression(r)) => {
                Node::Expression(Expression::InfixExpression {
                    left: Box::new(l),
                    operator,
                    right: Box::new(r),
                })
            }
            _ => todo!(),
        },
        Expression::PrefixExpression { operator, value } => match modify(value.to_node(), modifier)
        {
            Node::Expression(e) => Node::Expression(Expression::PrefixExpression {
                operator,
                value: Box::new(e),
            }),
            _ => todo!(),
        },
        Expression::Integer { value } => modifier(Node::Expression(Expression::Integer { value })),
        Expression::Index { value, index } => match (
            modify(value.to_node(), modifier),
            modify(index.to_node(), modifier),
        ) {
            (Node::Expression(v), Node::Expression(i)) => Node::Expression(Expression::Index {
                value: Box::new(v),
                index: Box::new(i),
            }),
            _ => todo!(),
        },
        e => todo!("No implementation for expression type: {:?}", e),
    };
}

#[cfg(test)]
mod tests {
    use super::{modify, ModifierFunction};
    use crate::parse::ast::{Expression, Node, ToNode};
    use crate::token::types::Token::{self, Minus, Plus};

    use pretty_assertions::assert_eq;

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

    fn turn_one_into_two(node: Node) -> Node {
        if let Node::Expression(Expression::Integer { value: 1 }) = node {
            return Node::Expression(Expression::Integer { value: 2 });
        }
        return node;
    }

    fn assert_modifier(actual: impl ToNode, expected: impl ToNode, modifier: ModifierFunction) {
        let expected_node = expected.to_node();
        let modified = modify(actual.to_node(), modifier);
        assert_eq!(
            expected_node, modified,
            "Expected: {:#?}, actual: {:#?}",
            expected_node, modified
        );
    }

    #[test]
    fn modifiers() {
        let scenarios = vec![
            (int(1), int(2), turn_one_into_two),
            (int(2), int(2), turn_one_into_two),
            (
                infix(int(1), Plus, int(2)),
                infix(int(2), Plus, int(2)),
                turn_one_into_two,
            ),
            (
                infix(int(2), Plus, int(1)),
                infix(int(2), Plus, int(2)),
                turn_one_into_two,
            ),
            (
                prefix(Minus, int(1)),
                prefix(Minus, int(2)),
                turn_one_into_two,
            ),
            (
                index(int(1), int(1)),
                index(int(2), int(2)),
                turn_one_into_two,
            ),
        ];
        for (scenario, expected, modifier) in scenarios.into_iter() {
            assert_modifier(scenario, expected, modifier);
        }
    }
}
