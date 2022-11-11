use anyhow::Result;
use thiserror::Error;

use crate::{
    ast::{BlockStatement, Expression, Identifier, Literal, Operator, Program, Statement},
    lexer::Lexer,
    precedence::{FromToken, Precedence},
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
    #[error("parse to operator error. {0}")]
    ParseOperator(String),
    #[error("unable to parse integer litral. {0}")]
    UnableParseIntegerLiteral(String),
    #[error("unable to parse boolean litral. {0}")]
    UnableParseBooleanLiteral(String),
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
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> R<Statement> {
        self.expect_peek(Token::Ident("ident".to_string()))?;

        let name = Identifier(match &self.current_token {
            Token::Ident(ident) => ident.to_string(),
            _ => unreachable!(),
        });

        self.expect_peek(Token::Assign)?;
        self.next_token();
        let literal = self.parse_expression(Precedence::Lowest)?;
        while self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(name, literal))
    }

    fn parse_return_statement(&mut self) -> R<Statement> {
        self.next_token();
        let literal = self.parse_expression(Precedence::Lowest)?;
        while self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(literal))
    }

    fn parse_expression_statement(&mut self) -> R<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> R<Expression> {
        let mut left = match self.current_token {
            Token::Ident(_) => self.parse_identifier()?,
            Token::Int(_) => self.parse_integer_literal()?,
            Token::True | Token::False => self.parse_boolean_literal()?,
            Token::Bang | Token::Minus => self.parse_prefix_expression()?,
            Token::LParen => self.parse_group_expression()?,
            Token::If => self.parse_if_expression()?,
            Token::Function => self.parse_function_literal()?,
            _ => {
                return Err(ParserError::ExpectToken {
                    expected: format!("Prefix token"),
                    found: format!("{}", self.current_token),
                })
            }
        };

        while !self.peek_token_is(&Token::Semicolon)
            && precedence < Precedence::from_token(&self.peek_token)
        {
            match &self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::Neq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    left = self.parse_infix_expression(Box::new(left))?;
                }
                Token::LParen => {
                    self.next_token();
                    left = self.parse_call_expression(Box::new(left))?;
                }
                _ => break,
            };
        }

        Ok(left)
    }

    fn parse_identifier(&self) -> R<Expression> {
        match &self.current_token {
            Token::Ident(ident) => Ok(Expression::Lit(Literal::IdentLiteral(ident.to_string()))),
            _ => Err(ParserError::ExpectToken {
                expected: format!("{}", Token::Ident("".to_string())),
                found: format!("{}", self.current_token),
            }),
        }
    }

    fn parse_integer_literal(&self) -> R<Expression> {
        let value: u32 =
            self.current_token.to_string().parse().map_err(|_| {
                ParserError::UnableParseIntegerLiteral(self.current_token.to_string())
            })?;

        Ok(Expression::Lit(Literal::IntegerLiteral(value)))
    }

    fn parse_boolean_literal(&self) -> R<Expression> {
        match &self.current_token {
            Token::True | Token::False => Ok(Expression::Lit(Literal::BooleanLiteral(
                self.current_token_is(&Token::True),
            ))),
            _ => Err(ParserError::UnableParseBooleanLiteral(
                self.current_token.to_string(),
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> R<Expression> {
        let token = self.current_token.clone();

        let operator = parse_to_operator(&token)?;
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> R<Expression> {
        let token = self.current_token.clone();
        let operator = parse_to_operator(&token)?;
        let precedence = Precedence::from_token(&token);
        self.next_token();
        let right = Box::new(self.parse_expression(precedence)?);
        Ok(Expression::Infix {
            token,
            left,
            operator,
            right,
        })
    }

    fn parse_group_expression(&mut self) -> R<Expression> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::RParen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> R<Expression> {
        let token = self.current_token.clone();
        self.expect_peek(Token::LParen)?;
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::RParen)?;
        self.expect_peek(Token::LBrace)?;
        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();
            self.expect_peek(Token::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> R<Expression> {
        let token = self.current_token.clone();
        self.expect_peek(Token::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::LBrace)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::Function {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> R<Vec<Identifier>> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();
        let ident = Identifier(self.current_token.to_string());
        identifiers.push(ident);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            let ident = Identifier(self.current_token.to_string());
            identifiers.push(ident);
        }

        self.expect_peek(Token::RParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, left: Box<Expression>) -> R<Expression> {
        let token = self.current_token.clone();
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call {
            token,
            function: left,
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> R<Vec<Box<Expression>>> {
        let mut args = Vec::new();

        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(Box::new(self.parse_expression(Precedence::Lowest)?));

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(Box::new(self.parse_expression(Precedence::Lowest)?));
        }

        self.expect_peek(Token::RParen)?;
        Ok(args)
    }

    fn parse_block_statement(&mut self) -> R<BlockStatement> {
        let mut statements = Vec::new();
        self.next_token();

        while !self.current_token_is(&Token::RBrace) && !self.current_token_is(&Token::Eof) {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

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

fn parse_to_operator(token: &Token) -> R<Operator> {
    Ok(match token {
        Token::Assign => Operator::Assign,
        Token::Plus => Operator::Plus,
        Token::Minus => Operator::Minus,
        Token::Bang => Operator::Bang,
        Token::Asterisk => Operator::Asterisk,
        Token::Slash => Operator::Slash,
        Token::Eq => Operator::Eq,
        Token::Neq => Operator::Neq,
        Token::Lt => Operator::Lt,
        Token::Gt => Operator::Gt,
        _ => return Err(ParserError::ParseOperator(token.to_string())),
    })
}

#[cfg(test)]
mod tests {
    use crate::ast::Operator;

    use super::*;

    fn parse(input: &str) -> Result<Program, ParserErrors> {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            ("let x = 5;", String::from("x"), Literal::IntegerLiteral(5)),
            (
                "let y = 10;",
                String::from("y"),
                Literal::IntegerLiteral(10),
            ),
            (
                "let foobar = 838383;",
                String::from("foobar"),
                Literal::IntegerLiteral(838383),
            ),
        ];

        for (input, expect_ident_value, expect_literal_value) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Let(ident, exp)) = target {
                        assert_eq!(*ident, Identifier(expect_ident_value));
                        test_literal_expression(exp, expect_literal_value);
                    }
                }
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let inputs = vec![
            ("return 5;", Literal::IntegerLiteral(5)),
            ("return 10;", Literal::IntegerLiteral(10)),
            ("return 993322;", Literal::IntegerLiteral(993322)),
        ];

        for (input, expect_literal_value) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Return(exp)) = target {
                        test_literal_expression(exp, expect_literal_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = ("foobar;", "foobar");
        match parse(input.0) {
            Err(errors) => check_parser_errors(&errors),
            Ok(program) => {
                let target = program.statements.get(0);
                if let Some(Statement::Expression(Expression::Lit(Literal::IdentLiteral(
                    identifier,
                )))) = target
                {
                    assert_eq!(*identifier, input.1);
                }
            }
        };
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = ("5;", Literal::IntegerLiteral(5));
        match parse(input.0) {
            Err(errors) => check_parser_errors(&errors),
            Ok(program) => {
                let target = program.statements.get(0);
                if let Some(Statement::Expression(Expression::Lit(v))) = target {
                    assert_eq!(*v, input.1);
                }
            }
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let inputs = vec![
            ("true;", Literal::BooleanLiteral(true)),
            ("false;", Literal::BooleanLiteral(false)),
        ];
        for (input, expect_literal_value) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Expression(Expression::Lit(v))) = target {
                        assert_eq!(*v, expect_literal_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs = vec![
            ("!5;", "!", Literal::IntegerLiteral(5)),
            ("-15;", "-", Literal::IntegerLiteral(15)),
            ("!true", "!", Literal::BooleanLiteral(true)),
            ("!false", "!", Literal::BooleanLiteral(false)),
        ];

        for (input, expect_operator, expect_literal_value) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Expression(Expression::Prefix {
                        operator, right, ..
                    })) = target
                    {
                        test_operator(operator, expect_operator);
                        test_literal_expression(right, expect_literal_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let inputs = vec![
            (
                "5 + 5;",
                Literal::IntegerLiteral(5),
                "+",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 - 5;",
                Literal::IntegerLiteral(5),
                "-",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 * 5;",
                Literal::IntegerLiteral(5),
                "*",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 / 5;",
                Literal::IntegerLiteral(5),
                "/",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 > 5;",
                Literal::IntegerLiteral(5),
                ">",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 < 5;",
                Literal::IntegerLiteral(5),
                "<",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 == 5;",
                Literal::IntegerLiteral(5),
                "==",
                Literal::IntegerLiteral(5),
            ),
            (
                "5 != 5;",
                Literal::IntegerLiteral(5),
                "!=",
                Literal::IntegerLiteral(5),
            ),
            (
                "true == true",
                Literal::BooleanLiteral(true),
                "==",
                Literal::BooleanLiteral(true),
            ),
            (
                "true != false",
                Literal::BooleanLiteral(true),
                "!=",
                Literal::BooleanLiteral(false),
            ),
            (
                "false == false",
                Literal::BooleanLiteral(false),
                "==",
                Literal::BooleanLiteral(false),
            ),
        ];
        for (input, expect_left, expect_operator, expect_right) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Expression(infix)) = target {
                        test_infix_expression(infix, expect_left, expect_operator, expect_right);
                    }
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    assert_eq!(&format!("{}", program), expected);
                }
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        match parse(input) {
            Err(errors) => check_parser_errors(&errors),
            Ok(program) => {
                let target = program.statements.get(0);
                if let Some(Statement::Expression(Expression::If {
                    condition,
                    consequence,
                    alternative,
                    ..
                })) = target
                {
                    test_infix_expression(
                        condition,
                        Literal::IdentLiteral("x".to_string()),
                        &"<",
                        Literal::IdentLiteral("y".to_string()),
                    );

                    let statements = &consequence.statements;
                    assert!(statements.len() > 0);

                    if let Some(Statement::Expression(Expression::Lit(Literal::IdentLiteral(
                        identifier,
                    )))) = statements.get(0)
                    {
                        assert_eq!(identifier, "x");
                    }

                    assert!(alternative.is_none());
                }
            }
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        match parse(input) {
            Err(errors) => check_parser_errors(&errors),
            Ok(program) => {
                let target = program.statements.get(0);
                if let Some(Statement::Expression(Expression::Function {
                    parameters, body, ..
                })) = target
                {
                    assert_eq!(parameters.get(0), Some(&Identifier("x".to_string())));
                    assert_eq!(parameters.get(1), Some(&Identifier("y".to_string())));

                    if let Some(Statement::Expression(exp)) = body.statements.get(0) {
                        test_infix_expression(
                            exp,
                            Literal::IdentLiteral("x".to_string()),
                            "+",
                            Literal::IdentLiteral("y".to_string()),
                        )
                    } else {
                        panic!(
                            "function body parse error: {}",
                            body.statements.get(0).unwrap()
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let inputs = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in inputs {
            match parse(input) {
                Err(errors) => check_parser_errors(&errors),
                Ok(program) => {
                    let target = program.statements.get(0);
                    if let Some(Statement::Expression(Expression::Function {
                        parameters, ..
                    })) = target
                    {
                        parameters.iter().enumerate().for_each(|(i, parameter)| {
                            let expect_param = expected_params[i];
                            assert_eq!(parameter, &Identifier(expect_param.to_string()));
                        })
                    }
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        match parse(input) {
            Err(errors) => check_parser_errors(&errors),
            Ok(program) => {
                let target = program.statements.get(0);
                if let Some(Statement::Expression(Expression::Call {
                    function,
                    arguments,
                    ..
                })) = target
                {
                    test_literal_expression(function, Literal::IdentLiteral("add".to_string()));
                    test_literal_expression(&arguments[0], Literal::IntegerLiteral(1));
                    test_infix_expression(
                        &arguments[1],
                        Literal::IntegerLiteral(2),
                        "*",
                        Literal::IntegerLiteral(3),
                    );
                    test_infix_expression(
                        &arguments[2],
                        Literal::IntegerLiteral(4),
                        "+",
                        Literal::IntegerLiteral(5),
                    );
                }
            }
        }
    }

    fn test_literal_expression(exp: &Expression, expect: Literal) {
        match exp {
            Expression::Lit(literal) => assert_eq!(literal, &expect),
            _ => unreachable!(),
        }
    }

    fn test_infix_expression(
        exp: &Expression,
        expect_left: Literal,
        expect_operator: &str,
        expect_right: Literal,
    ) {
        match exp {
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => {
                test_literal_expression(left, expect_left);
                test_operator(operator, expect_operator);
                test_literal_expression(right, expect_right);
            }
            _ => unreachable!(),
        }
    }

    fn test_operator(operator: &Operator, expect_operator: &str) {
        assert_eq!(operator.to_string(), expect_operator);
    }

    fn check_parser_errors(errors: &ParserErrors) {
        for error in errors.0.iter() {
            println!("{}", error);
        }

        assert_eq!(errors.0.len(), 0);
    }
}
