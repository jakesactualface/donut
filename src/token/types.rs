#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Integer(usize),
    Assignment,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    Illegal,
}
