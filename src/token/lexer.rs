use std::{iter::Peekable, str::Chars};

use super::types::Token;

pub struct Lexer<'a> {
    pub input: Peekable<Chars<'a>>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.take_until(|x| !x.is_whitespace());
        self.input.next().map(|c| self.pull_next_token(c))
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
        taken
    }

    fn concat_until(&mut self, c: &char, condition: impl Fn(&char) -> bool) -> String {
        let mut concat_string = vec![c];
        let rest = self.take_until(condition);
        concat_string.extend(rest.iter());
        concat_string.into_iter().collect()
    }

    fn pull_next_token(&mut self, c: char) -> Token {
        match c {
            '=' => match self.input.peek() {
                Some('=') => {
                    // Consume evaluated equal sign
                    self.next();
                    Token::Equal
                }
                _ => Token::Assignment,
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => match self.input.peek() {
                Some('=') => {
                    // Consume evaluated bang
                    self.next();
                    Token::NotEqual
                }
                _ => Token::Bang,
            },
            '&' => match self.input.peek() {
                Some('&') => {
                    // Consume evaluated ampersand
                    self.next();
                    Token::And
                }
                _ => Token::Illegal,
            },
            '|' => match self.input.peek() {
                Some('|') => {
                    // Consume evaluated bar
                    self.next();
                    Token::Or
                }
                _ => Token::Illegal,
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ':' => Token::Colon,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '"' => Token::String(self.read_string()),
            c => {
                if c.is_alphabetic() {
                    // Capture remainder of alphanumeric word
                    let word_string = self.concat_until(&c, |x| !is_word_char(x));
                    return map_word_to_token(&word_string);
                }
                if c.is_ascii_digit() {
                    // Capture remainder of number
                    let number_string = self.concat_until(&c, |x| !x.is_ascii_digit());
                    match i64::from_str_radix(&number_string, 10) {
                        Ok(x) => return Token::Integer(x),
                        Err(e) => panic!("Error parsing integer: {}", e),
                    }
                }
                Token::Illegal
            }
        }
    }

    fn read_string(&mut self) -> String {
        let mut chars: Vec<char> = vec![];
        let mut is_escaped_char = false;
        loop {
            match self.input.next() {
                Some('\\') => {
                    if is_escaped_char {
                        chars.push('\\');
                    }
                    is_escaped_char = !is_escaped_char;
                }
                Some('t') => {
                    if is_escaped_char {
                        chars.push('\t');
                        is_escaped_char = false;
                    } else {
                        chars.push('t');
                    }
                }
                Some('n') => {
                    if is_escaped_char {
                        chars.push('\n');
                        is_escaped_char = false;
                    } else {
                        chars.push('n');
                    }
                }
                Some('"') => {
                    if is_escaped_char {
                        chars.push('"');
                        is_escaped_char = false;
                    } else {
                        break;
                    }
                }
                Some(c) => chars.push(c),
                None => panic!("Expected string termination character!"),
            }
        }
        chars.into_iter().collect()
    }
}

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let,
    "mut" => Token::Mutate,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "while" => Token::While,
    "return" => Token::Return,
    "macro" => Token::Macro,
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

            10 == 10;
            10 != 9;
            \"foobar\"
            \"foo bar\"
            \"foo\\\"bar\"
            \"foo\\tbar\"
            \"foo\\nbar\"
            [1, 2];
            {\"foo\": \"bar\"}
            macro(x, y) { x + y; };
            mut a = 10;
            while (true) { mut a = a + 1; };
            true && true;
            false || false;
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
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
            Token::String(String::from("foobar")),
            Token::String(String::from("foo bar")),
            Token::String(String::from("foo\"bar")),
            Token::String(String::from("foo\tbar")),
            Token::String(String::from("foo\nbar")),
            Token::LBracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::RBracket,
            Token::Semicolon,
            Token::LBrace,
            Token::String(String::from("foo")),
            Token::Colon,
            Token::String(String::from("bar")),
            Token::RBrace,
            Token::Macro,
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
            Token::Mutate,
            Token::Identifier(String::from("a")),
            Token::Assignment,
            Token::Integer(10),
            Token::Semicolon,
            Token::While,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::LBrace,
            Token::Mutate,
            Token::Identifier(String::from("a")),
            Token::Assignment,
            Token::Identifier(String::from("a")),
            Token::Plus,
            Token::Integer(1),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::True,
            Token::And,
            Token::True,
            Token::Semicolon,
            Token::False,
            Token::Or,
            Token::False,
            Token::Semicolon,
        ];
        assert_tokens(expected, Lexer::new(input));
    }

    fn assert_tokens(expected: Vec<Token>, actual: impl Iterator<Item = Token>) {
        assert_eq!(expected, actual.into_iter().collect::<Vec<Token>>());
    }
}
