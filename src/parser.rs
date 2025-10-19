use crate::token::{Token};
use crate::lexer::{Lexer};
use crate::ast;

pub struct Parser {
	lexer: Lexer,
	curr_token: Token,
	next_token: Token,
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
		Self {
			lexer: lexer,
			curr_token: lexer.next_token(),
			next_token: lexer.next_token(),
		}
	}

	fn next_token(&self) {
		self.current_token = self.peek_token();
		self.next_token = self.lexer.next_token();
	}
	
	fn parse_program(&self) {
		null
	}
}
			 
