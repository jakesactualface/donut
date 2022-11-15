pub enum NodeType {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

pub enum Statement {
    Let(String, Expression),
}

pub enum Expression {
    Identifier(String),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
