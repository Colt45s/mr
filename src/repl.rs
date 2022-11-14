use std::io::{stdin, stdout, Write};

use crate::{evaluator::eval, lexer::Lexer, object::Environment, parser::Parser};
use anyhow::Result;

static MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----”
"#;

pub fn start() -> Result<()> {
    let stdin = stdin();
    let mut stdout = stdout();
    let env = Environment::new();

    loop {
        print!(">> ");
        stdout.flush().expect("failed flush");
        let mut input = String::new();
        stdin.read_line(&mut input)?;

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => {
                let evaluated = eval(&program, &env);
                writeln!(stdout, "{}", evaluated)?;
            }
            Err(errors) => {
                writeln!(stdout, "{}", MONKEY_FACE)?;
                writeln!(stdout, "Woops! We ran into some monket business here!")?;
                writeln!(stdout, "parser errors: {}", errors)?;
            }
        };

        stdout.flush()?;
    }
}
