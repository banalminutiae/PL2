use crate::token::{Token, TokenType};
use crate::lexer::{Lexer};
use crate::ast::{Program, Statement, LetStatement, Identifier};

pub struct Parser<'a> {
	lexer: Lexer<'a>,
	curr_token: Token,
	next_token: Token,
}

impl<'a> Parser<'a> {
	pub fn new(mut lexer: Lexer<'a>) -> Self {
		let curr_token = lexer.next_token();
		let next_token = lexer.next_token();
		Self {
			lexer: lexer.clone(),
			curr_token,
			next_token,
		}
	}

	fn next_token(&mut self) {
		self.curr_token = self.next_token.clone();
		self.next_token = self.lexer.next_token();
	}

	fn parse_program(&mut self) -> Program {
		let mut program = Program { statements: Vec::new() };
		while self.curr_token.token_type != TokenType::EOF {
			if let Some(statement) = self.parse_statement() {
				program.statements.push(statement);
			}
			self.next_token();
		}
		program
	}
	
	fn parse_statement(&mut self) -> Option<Statement> {
		match self.curr_token.token_type {
			TokenType::LET => {
				self.parse_let_statement().map(Statement::Let)
			}
			TokenType::RETURN => {
				None
			}
			_ => None
		}
	}

	fn parse_let_statement(&mut self) -> Option<LetStatement> {
		let current_token = self.curr_token.clone();
		
		if !self.expect_peek(TokenType::IDENTIFIER) {
			return None;
		}

		let statement = LetStatement {
			token: current_token.clone(),
			name: Identifier {
				token: self.curr_token.clone(),
				value: self.curr_token.literal.clone(),
			},
			// Expression here later
		};
		
		if !self.expect_peek(TokenType::ASSIGN) {
			return None;
		}

		// Temporarily skip expression parsing
		while self.curr_token.token_type != TokenType::SEMICOLON {
			self.next_token()
		}

		Some(statement)
	}

	fn expect_peek(&mut self, expected_type: TokenType) -> bool {
		if self.next_token.token_type == expected_type {
			self.next_token();
			true
		} else {
			false
		}
	}
}



#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn test_parser() {
		let source = r#"
           let x = 5;
           let y = 10;

           let foobar = 828282;
        "#;

		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		assert_eq!(program.statements.len(), 3);
	}
}



