use std::io;

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() -> io::Result<()> {
    repl::start()?;
    Ok(())
}
