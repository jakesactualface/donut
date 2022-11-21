use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Identifier(String),
    Integer(i64),
    Assignment,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    LT,
    GT,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Illegal,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // my_function(X)
}

lazy_static! {
    static ref PRECEDENCES: HashMap<Token, Precedence> = HashMap::from([
        (Token::Equal, Precedence::Equals),
        (Token::NotEqual, Precedence::Equals),
        (Token::LT, Precedence::LessGreater),
        (Token::GT, Precedence::LessGreater),
        (Token::Plus, Precedence::Sum),
        (Token::Minus, Precedence::Sum),
        (Token::Slash, Precedence::Product),
        (Token::Asterisk, Precedence::Product),
        (Token::LParen, Precedence::Call),
    ]);
}

impl Token {
    pub fn precedence(&self) -> &Precedence {
        if let Some(precedence) = PRECEDENCES.get(&self) {
            return precedence;
        }
        &Precedence::Lowest
    }
}
