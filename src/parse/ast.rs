use crate::token::types::Token;

pub trait Node {
    fn token(&self) -> Option<Token>;
}

pub trait Statement {
    fn node(&self) -> Box<dyn Node>;
    fn statement(&self);
}

pub trait Expression {
    fn node(&self) -> Box<dyn Node>;
    fn expression(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement + 'static>>,
}

impl Node for Program {
    fn token(&self) -> Option<Token> {
        match self.statements.get(0) {
            Some(s) => s.node().token(),
            None => None,
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Token,
    pub value: Box<dyn Expression>,
}

impl Statement for LetStatement {
    fn node(&self) -> Box<dyn Node> {
        self.value.node()
    }

    fn statement(&self) {}
}

impl Node for LetStatement {
    fn token(&self) -> Option<Token> {
        Some(self.token.clone())
    }
}
