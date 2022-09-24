use anyhow::Result;
use thiserror::Error;

use crate::{
    ast::{Expression, Identifer, Program, Statement},
    lexer::Lexer,
    token::Token,
};

type R<T> = Result<T, ParserError>;

pub struct Parser {
    l: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: ParserErrors,
}

#[derive(Error, Debug, Clone)]
#[error("{}", self.0.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n"))]
pub struct ParserErrors(Vec<ParserError>);

#[derive(Error, Debug, Clone)]
enum ParserError {
    #[error("expect token (expected {expected:?}, found {found:?})")]
    ExpectToken { expected: String, found: String },
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: ParserErrors(Vec::new()),
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserErrors> {
        let mut program = Program::default();
        while self.current_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(e) => self.errors.0.push(e),
            }
            self.next_token();
        }

        if !self.errors.0.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> R<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            // TODO 適当にしてある
            _ => Ok(Statement::Let(
                Identifer("test".to_string()),
                Expression::Ident(Identifer("".to_string())),
            )),
        }
    }

    fn parse_let_statement(&mut self) -> R<Statement> {
        self.expect_peek(Token::Ident("ident".to_string()))?;

        let name = Identifer(match &self.current_token {
            Token::Ident(ident) => ident.to_string(),
            _ => unreachable!(),
        });

        self.expect_peek(Token::Assign)?;

        while !self.current_token_is(&Token::Eof) {
            self.next_token();
        }

        Ok(Statement::Let(
            name,
            Expression::Ident(Identifer("".to_string())),
        ))
    }

    fn parse_identifier() {}

    fn expect_peek(&mut self, token: Token) -> R<()> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::ExpectToken {
                expected: format!("{}", token),
                found: format!("{}", self.peek_token),
            })
        }
    }

    fn current_token_is(&self, token: &Token) -> bool {
        self.current_token == *token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match token {
            Token::Ident(_) => match self.peek_token {
                Token::Ident(_) => true,
                _ => false,
            },
            Token::Int(_) => match self.peek_token {
                Token::Int(_) => true,
                _ => false,
            },
            token => self.peek_token == *token,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Program, ParserErrors> {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    #[test]
    fn test_let_statement() {
        let inputs = vec![
            ("let x = 5;", String::from("x"), 5),
            ("let y = 10;", String::from("y"), 10),
            ("let foobar = 838383;", String::from("foobar"), 838383),
        ];

        for (input, expect_ident_value, expect_literal_value) in inputs {
            match parse(input) {
                Err(errors) => {
                    println!("{}", errors);
                    panic!();
                }
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Let(ident, exp)) = target {
                        assert_eq!(*ident, Identifer(expect_ident_value));
                    }
                }
            };
        }
    }
}
