use std::cell::RefCell;
use std::io::prelude::*;
use std::io::BufWriter;
use std::rc::Rc;
use std::thread;

use donut::console::repl::Repl;
use donut::object::evaluator::eval;
use donut::object::macro_expansion::define_macros;
use donut::object::macro_expansion::expand_macros;
use donut::object::types::Environment;

const STACK_SIZE: usize = 4 * 1024 * 1024;
const PROMPT: &'static str = ">>";

fn run() {
    let args: Vec<_> = std::env::args().collect();

    let repl = Repl::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    let macro_env = Rc::new(RefCell::new(Environment::new()));
    let mut writer = BufWriter::new(std::io::stdout());
    let mut input: String = String::default();

    if let Some(pipe) = args.get(1) {
        let mut output = repl.run(&pipe);
        define_macros(&mut output, macro_env.clone());
        let expanded = expand_macros(&mut output, macro_env.clone());

        println!("{:?}", eval(expanded, env.clone()));
    } else {
        println!("Welcome to the Donut REPL!");
        println!("Use command 'exit' to exit the prompt.");
        println!();
    }

    loop {
        print!("{}", PROMPT);
        writer.flush().ok();

        if let Some(Ok(ref line)) = std::io::stdin().lines().next() {
            if is_exit(line) {
                break;
            }

            input += line;

            if is_unfinished(line) {
                // Remove the escape character
                input.pop();
                continue;
            }

            let mut output = repl.run(&input);
            define_macros(&mut output, macro_env.clone());
            let expanded = expand_macros(&mut output, macro_env.clone());

            input = String::default();
            println!("{:?}", eval(expanded, env.clone()));
        }
    }
}

fn is_exit(line: &str) -> bool {
    "exit".eq(line)
}

fn is_unfinished(line: &str) -> bool {
    if let Some('\\') = line.chars().last() {
        return true;
    }
    return false;
}

fn main() {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap_or_default();
}
