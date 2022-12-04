use crate::token::types::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Vec<Statement>),
}

pub trait ToNode {
    fn to_node(self: Self) -> Node;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression { value: Expression },
    Block { statements: Vec<Statement> },
}

impl ToNode for Statement {
    fn to_node(self) -> Node {
        match self {
            s => Node::Statement(s),
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier {
        name: String,
    },
    Integer {
        value: i64,
    },
    String {
        value: String,
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
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Function {
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl ToNode for Expression {
    fn to_node(self) -> Node {
        match self {
            s => Node::Expression(s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl ToNode for Program {
    fn to_node(self: Self) -> Node {
        Node::Program(self.statements)
    }
}
