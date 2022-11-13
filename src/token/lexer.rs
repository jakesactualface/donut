use std::{iter::Peekable, str::Chars};

use super::types::Token;

pub struct Lexer<'a> {
    pub input: Peekable<Chars<'a>>,
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

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn take_until(&mut self, condition: impl Fn(&char) -> bool) -> Vec<char> {
        let mut taken = Vec::new();
        while let Some(next) = self.input.next_if(|c| !condition(c)) {
            taken.push(next);
        }
        return taken;
    }

    fn concat_until(&mut self, c: &char, condition: impl Fn(&char) -> bool) -> String {
        let mut concat_string = vec![c];
        let rest = self.take_until(condition);
        concat_string.extend(rest.iter());
        concat_string.into_iter().collect()
    }

    fn pull_next_token(&mut self, c: char) -> Token {
        match c {
            '=' => Token::Assignment,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => Token::Bang,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            c => {
                if c.is_alphabetic() {
                    // Capture remainder of alphanumeric word
                    let word_string = self.concat_until(&c, |x| !is_word_char(x));
                    return map_word_to_token(&word_string);
                }
                if c.is_digit(10) {
                    // Capture remainder of number
                    let number_string = self.concat_until(&c, |x| !x.is_digit(10));
                    match usize::from_str_radix(&number_string, 10) {
                        Ok(x) => return Token::Integer(x),
                        Err(e) => panic!("Error parsing integer: {}", e),
                    }
                }
                Token::Illegal
            }
        }
    }
}

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
};

fn is_word_char(c: &char) -> bool {
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
        let input = "=+(){},;-/*<>";
        let expected = vec![
            Token::Assignment,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::LT,
            Token::GT,
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
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }
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
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LT,
            Token::Integer(10),
            Token::GT,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Integer(5),
            Token::LT,
            Token::Integer(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
        ];
        assert_tokens(expected, Lexer::new(input));
    }

    fn assert_tokens(expected: Vec<Token>, actual: impl Iterator<Item = Token>) {
        assert_eq!(expected, actual.into_iter().collect::<Vec<Token>>());
    }
}
