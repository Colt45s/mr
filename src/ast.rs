use crate::token::Token;

#[derive(Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    Let(Identifer, Expression),
}

#[derive(Debug, PartialEq)]
pub struct Identifer(pub String);

pub enum Expression {
    Ident(Identifer),
}
