#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident(String),
    Int(u32),

    // 演算子
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Eq,
    Neq,

    Lt,
    Gt,

    // デリミタ
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
