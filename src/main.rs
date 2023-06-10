mod token;
mod scanner;
mod expr;
mod parser;

#[macro_use]
extern crate lazy_static;

use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;
use std::process::exit;
use clap::Parser;
use scanner::Scanner;
use token::Token;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
   file_path: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(file) = args.file_path {
        run_file(file);
    } else {
        run_prompt();
    }
}

fn run_file(file: String) {
    let path = Path::new(&file);
    let contents: String = fs::read_to_string(path).expect("Unable to read file");
    if !run(contents) {
        exit(65);
    }
}

fn run_prompt() {
    print!("> ");
    io::stdout().flush().expect("Error in REPL");

    let mut stdin = io::stdin().lock().lines();

    while let Some(line) = stdin.next() {
        let instruction = line.unwrap();

        if instruction.is_empty() {
            break;
        }

        if !run(instruction) {
            println!("Invalid command.");
        }

        print!("> ");
        io::stdout().flush().expect("Error in REPL");
    }
}

fn run(source: String) -> bool {
    let mut scanner: Scanner = Scanner::from(source);
    let tokens: &Vec<Token> = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token.type_token);
    }

    true
}

fn error(line: usize, message: String) {
    report(line, None, message);
}

fn report(line: usize, position: Option<String>, message: String) {
    if let Some(pos) = position {
        eprintln!("[line {}] Error {}: {}", line, pos, message);
    } else {
        eprintln!("[line {}] Error: {}", line, message);
    }
}
