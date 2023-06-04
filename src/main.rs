use scanner::Scanner;

use crate::parser::Parser;

mod parser;
mod scanner;

fn main() {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline("λ ");
        match readline {
            Ok(line) => {
                let tokens = Scanner::scan(line).unwrap();
                let expr = Parser::parse(tokens);
                println!("{:#?}", expr);
            }
            Err(_) => {
                println!("Connection terminated");
                break;
            }
        }
    }
}
