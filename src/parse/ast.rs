use crate::token::types::Token;

pub enum Node {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression { value: Expression },
    Block { statements: Vec<Statement> },
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
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<Program>,
        alternative: Option<Box<Program>>,
    },
    Function {
        parameters: Vec<Expression>,
        body: Box<Program>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
