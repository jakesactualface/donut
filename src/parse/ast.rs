use crate::token::types::Token;

pub enum Node {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression { value: Expression },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier {
        name: String,
    },
    Integer {
        value: usize,
    },
    Boolean {
        value: bool,
    },
    PrefixExpression {
        operator: Token,
        value: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
}

pub struct Program {
    pub statements: Vec<Statement>,
}
