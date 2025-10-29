#![allow(unused)]

use::std::env;

mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() == 1 {
		repl::start();
	}
}
