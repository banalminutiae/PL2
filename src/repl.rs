use std::io;
use std::io::{Write};
use crate::token::{Token, TokenType};
use crate::lexer::Lexer;

pub fn start() {
	println!(">> PL2, v0.0.1");
	print!(">>");
	let _ = io::stdout().flush();

	let mut src = String::new();
    let _ = io::stdin().read_line(&mut src);
	let mut lexer = Lexer::new(&src);

	loop {
		let token = lexer.next_token();
		println!("{:?}", token);
		if token == Token::new(TokenType::Eof, " ".to_string()) {
			break;
		}
	}
}
