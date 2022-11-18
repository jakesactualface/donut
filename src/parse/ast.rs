pub enum Node {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression { value: Expression },
}

#[derive(Debug)]
pub enum Expression {
    Identifier { name: String },
    Integer { value: usize },
}

pub struct Program {
    pub statements: Vec<Statement>,
}
