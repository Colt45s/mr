mod ast;
mod lexer;
mod parser;
mod precedence;
mod repl;
mod token;

use anyhow::Result;

fn main() -> Result<()> {
    repl::start()?;
    Ok(())
}
