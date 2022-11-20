use std::io::prelude::*;
use std::io::BufWriter;

use donut::console::repl::Repl;

const PROMPT: &'static str = ">>";

fn main() {
    let repl = Repl::new();
    let mut writer = BufWriter::new(std::io::stdout());

    println!("Welcome to the Donut REPL!");
    println!("Use command 'exit' to exit the prompt.");
    println!();

    loop {
        print!("{}", PROMPT);
        writer.flush().ok();

        if let Some(Ok(ref line)) = std::io::stdin().lines().next() {
            if is_exit(line) {
                break;
            }

            let output = repl.run(line);
            for statement in output.statements {
                println!("{:#?}", statement);
            }
        }
    }
}

fn is_exit(line: &str) -> bool {
    "exit".eq(line)
}
