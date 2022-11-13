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
        while let Some(next) = self.input.next_if(|c| !condition(c)) {
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
            c => {
                if c.is_alphabetic() {
                    // Capture remainder of alphanumeric word
                    let mut word = vec![c];
                    let rest = self.take_until(|x| !is_keyword_char(x));
                    word.extend(rest.iter());
                    let word_string: String = word.into_iter().collect();
                    return map_word_to_token(&word_string);
                }
                if c.is_digit(10) {
                    // Capture remainder of number
                    todo!()
                }
                Token::Illegal
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.take_until(|x| !x.is_whitespace());
        match self.input.next() {
            Some(c) => Some(self.pull_next_token(c)),
            None => None,
        }
    }
}

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let
};

fn is_keyword_char(c: &char) -> bool {
    c.is_alphanumeric() || c.eq(&'_')
}

fn map_word_to_token(word: &str) -> Token {
    if KEYWORDS.contains_key(word) {
        return KEYWORDS.get(word).unwrap().clone();
    }
    Token::Identifier(String::from(word))
}

#[cfg(test)]
mod tests {
    use crate::token::types::Token;
    use pretty_assertions::assert_eq;

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
        assert_tokens(expected, Lexer::new(input));
    }

    #[test]
    fn symbols_and_keywords() {
        let input = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
        ";
        let expected = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assignment,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assignment,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assignment,
            Token::Function,
            Token::LParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assignment,
            Token::Identifier(String::from("add")),
            Token::LParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
        ];
        assert_tokens(expected, Lexer::new(input));
    }

    fn assert_tokens(expected: Vec<Token>, actual: impl Iterator<Item = Token>) {
        assert_eq!(expected, actual.into_iter().collect::<Vec<Token>>());
    }
}
