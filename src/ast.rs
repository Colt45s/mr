use std::fmt::{Display, Formatter, Result};

use crate::token::Token;

#[derive(Default, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(identifier, expression) => {
                write!(f, "let {} = {};", identifier, expression)
            }
            Statement::Return(expression) => write!(f, "return {};", expression),
            Statement::Expression(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Lit(Literal),
    Prefix {
        token: Token,
        operator: Operator,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        token: Token,
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    Call {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Lit(literal) => write!(f, "{}", literal),
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => write!(
                f,
                "if {} {}{}",
                condition,
                consequence,
                match alternative {
                    Some(statement) => format!("else {}", statement),
                    None => "".to_string(),
                }
            ),
            Expression::Function {
                parameters, body, ..
            } => write!(
                f,
                "fn {} {}",
                parameters
                    .iter()
                    .map(|parameter| parameter.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body
            ),
            Expression::Call {
                function,
                arguments,
                ..
            } => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(|parameter| parameter.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    IdentLiteral(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::IntegerLiteral(v) => write!(f, "{}", v),
            Literal::BooleanLiteral(v) => write!(f, "{}", v),
            Literal::IdentLiteral(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    Neq,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let v = match self {
            Operator::Assign => "=",
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Bang => "!",
            Operator::Asterisk => "*",
            Operator::Slash => "/",
            Operator::Lt => "<",
            Operator::Gt => ">",
            Operator::Eq => "==",
            Operator::Neq => "!=",
        };

        write!(f, "{}", v)
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}
