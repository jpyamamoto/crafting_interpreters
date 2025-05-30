mod lox;

#[macro_use]
extern crate lazy_static;

use clap::Parser;
use lox::error::Error;
use lox::stmt::Stmt;
use lox::token::Token;
use lox::{interpreter, parser, resolver, scanner};
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;
use std::process::exit;

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

    let stdin = io::stdin().lock().lines();

    for line in stdin {
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
    match run_or_err(source) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("{}", err);
            false
        }
    }
}

fn run_or_err(source: String) -> Result<bool, Error> {
    let tokens: Vec<Token> = scanner::scan_tokens(source)?;
    let statements: Vec<Stmt> = parser::parse(tokens)?;

    let locals = resolver::resolve(&statements)?;

    interpreter::interpret(statements, locals)?;

    Ok(true)
}
