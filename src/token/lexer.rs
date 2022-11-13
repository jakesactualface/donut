use std::{iter::Peekable, str::Chars};

use super::types::Token;

pub struct Lexer<'a> {
    pub input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn take_until(&mut self, condition: impl Fn(&char) -> bool) -> Vec<char> {
        let mut taken = Vec::new();
        while let Some(next) = self.input.next_if(&condition) {
            taken.push(next);
        }
        return taken;
    }

    fn pull_next_token(&mut self, c: char) -> Token {
        match c {
            '=' => Token::Assignment,
            '+' => Token::Plus,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            _ => Token::Illegal,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            Some(c) => Some(self.pull_next_token(c)),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::types::Token;

    use super::Lexer;

    #[test]
    fn symbols_only() {
        let input = "=+(){},;";
        let expected = vec![
            Token::Assignment,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];
        let mut lexer = Lexer::new(input);
        let mut counter = 0;
        for expected_token in expected.iter() {
            let actual_token = lexer.next().unwrap();
            assert_eq!(expected_token, &actual_token);
            counter += 1;
        }
        assert_eq!(expected.len(), counter);
    }
}
