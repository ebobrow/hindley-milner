use scanner::Scanner;
use typer::Typer;

use crate::parser::Parser;

mod parser;
mod scanner;
mod typer;

fn main() {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline("λ ");
        match readline {
            Ok(line) => {
                let tokens = Scanner::scan(line).unwrap();
                let expr = Parser::parse(tokens).unwrap();
                let ty = Typer::type_expr(expr);
                println!("{}", ty.to_string());
            }
            Err(_) => {
                println!("Connection terminated");
                break;
            }
        }
    }
}
