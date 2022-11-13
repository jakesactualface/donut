use crate::token::types::Token;

pub trait Node {
    fn token(&self) -> Option<Token>;
}

pub trait Statement {
    fn node(&self) -> Box<dyn Node>;
    fn statement(&self) -> Box<dyn Statement>;
}

pub trait Expression {
    fn node(&self) -> Box<dyn Node>;
    fn expression(&self) -> Box<dyn Expression>;
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
    token: Token,
    name: Token,
    value: dyn Expression,
}

impl Statement for LetStatement {
    fn node(&self) -> Box<dyn Node> {
        todo!()
    }

    fn statement(&self) -> Box<dyn Statement> {
        todo!()
    }
}

impl Node for LetStatement {
    fn token(&self) -> Option<Token> {
        Some(self.token.clone())
    }
}
