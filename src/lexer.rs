use crate::token::Token;
use std::{num::ParseIntError, str};

pub struct Lexer {
    input: Vec<char>,
    position: usize,      // 入力における現在の位置（現在の文字を指し示す）
    read_position: usize, // これから読み込む位置（現在の文字の次）
    ch: char,             // 現在検査中の文字
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position as usize >= self.input.len() {
            self.ch = '\0';
        } else {
            // as usize は気軽に使ってもよい？
            // https://zenn.dev/link/comments/c7c06ecec0337a
            self.ch = self.input[self.read_position as usize]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position as usize]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let t = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::Lt,
            '>' => Token::Gt,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '\0' => Token::Eof,
            _ => {
                if self.is_letter() {
                    match self.read_identifier() {
                        Some(s) => return self.lookup_ident(&s),
                        None => return Token::Illegal,
                    }
                } else if self.is_digit() {
                    match self.read_number() {
                        Ok(n) => return Token::Int(n),
                        Err(_) => return Token::Illegal,
                    }
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        t
    }

    fn read_identifier(&mut self) -> Option<String> {
        let from = self.position;
        while self.is_letter() {
            self.read_char();
        }

        self.input
            .get(from..self.position)
            .map(|s| s.iter().collect::<String>())
    }

    fn is_letter(&self) -> bool {
        self.ch.is_alphabetic() || self.ch == '_'
    }

    fn lookup_ident(&self, ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            ident => Token::Ident(ident.to_string()),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn is_digit(&self) -> bool {
        self.ch.is_digit(10)
    }

    fn read_number(&mut self) -> Result<i32, ParseIntError> {
        let from = self.position;

        while self.is_digit() {
            self.read_char();
        }

        let s = self
            .input
            .get(from..self.position)
            .map(|s| s.iter().collect::<String>())
            .unwrap();
        s.parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        let mut l = Lexer::new(input);

        for t in tests {
            let token = l.next_token();
            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_code() {
        let input = r#"let five = 5;
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
        "#;

        let tests = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
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
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::Neq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);

        for t in tests {
            let token = l.next_token();
            assert_eq!(t, token);
        }
    }
}
