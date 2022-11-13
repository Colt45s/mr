mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod precedence;
mod repl;
mod token;

use anyhow::Result;

fn main() -> Result<()> {
    repl::start()?;
    Ok(())
}
