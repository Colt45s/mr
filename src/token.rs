use core::fmt;

#[derive(Debug, PartialEq, Clone)]
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(name) => write!(f, "{}", name),
            Token::Int(val) => write!(f, "{}", val),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
            Token::Semicolon => write!(f, ";"),
            Token::Assign => write!(f, "="),
            Token::Function => write!(f, "fn"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            tok => write!(f, "{:?}", tok),
        }
    }
}
