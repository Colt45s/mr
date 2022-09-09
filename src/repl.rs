use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

pub fn start() -> io::Result<()> {
    loop {
        print!(">> ");
        let mut stdout = io::stdout();
        stdout.flush().expect("failed flush");

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let mut lexer = Lexer::new(&input);

        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            if tok == Token::Eof {
                break;
            }
        }
    }
}
