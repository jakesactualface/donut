pub enum Node {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

pub enum Statement {
    Let { name: String, value: Expression },
}

pub enum Expression {
    Identifier { name: String },
}

pub struct Program {
    pub statements: Vec<Statement>,
}
