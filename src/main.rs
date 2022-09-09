use std::io;

mod lexer;
mod repl;
mod token;

fn main() -> io::Result<()> {
    repl::start()?;
    Ok(())
}
