use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Identifier(String),
    Integer(i64),
    String(String),
    Assignment,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    And,
    Or,
    LT,
    GT,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Function,
    Let,
    Mutate,
    True,
    False,
    If,
    Else,
    While,
    Return,
    Illegal,
    Macro,
    Sprinkle,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    AndOr,       // && or ||
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // my_function(X)
    Index,       // array[index]
}

lazy_static! {
    static ref PRECEDENCES: HashMap<Token, Precedence> = HashMap::from([
        (Token::And, Precedence::AndOr),
        (Token::Or, Precedence::AndOr),
        (Token::Equal, Precedence::Equals),
        (Token::NotEqual, Precedence::Equals),
        (Token::LT, Precedence::LessGreater),
        (Token::GT, Precedence::LessGreater),
        (Token::Plus, Precedence::Sum),
        (Token::Minus, Precedence::Sum),
        (Token::Slash, Precedence::Product),
        (Token::Asterisk, Precedence::Product),
        (Token::LParen, Precedence::Call),
        (Token::LBracket, Precedence::Index),
    ]);
}

impl Token {
    pub fn precedence(&self) -> &Precedence {
        if let Some(precedence) = PRECEDENCES.get(self) {
            return precedence;
        }
        &Precedence::Lowest
    }
}
