use crate::ast::{Identifier, LetStatement, ExpressionStatement, ReturnStatement, IfExpression, BlockStatement, Statement, Expression, IntegerLiteral, Boolean, Prefix, Infix, Program};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    next_token: Token,
    errors: Vec<String>,
}

#[repr(u8)]
#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
	Lowest = 0,
	Equals = 1,
	LessGreater = 2,
	Sum = 3,
	Product = 4,
	Prefix = 5,
	Call = 6
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_token = lexer.next_token();
        let next_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            next_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.curr_token.token_type != TokenType::Eof {
            match self.parse_statement() {
                Some(statement) => program.statements.push(statement),
				None => self.synchronize_statement(),
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token.token_type {
            TokenType::Let => self.parse_let_statement().map(Statement::Let),
            TokenType::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
        } 
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.peek_and_consume_on_match(TokenType::Identifier) {
            return None;
        }

		let ident_token = self.curr_token.clone();

        if !self.peek_and_consume_on_match(TokenType::Equals) {
            return None;
        }

		self.next_token();

		let value = self.parse_expression(Precedence::Lowest);

		let statement = LetStatement {
            name: Identifier {
                value: ident_token.literal.clone(),
            },
			value: value?,
        };

		
		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
		self.next_token();
		
		let return_value = self.parse_expression(Precedence::Lowest);

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}
		
        let statement = ReturnStatement {
			value: return_value?,
        };
		
        Some(statement)
    }

	fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
		let exp = self.parse_expression(Precedence::Lowest)?;

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let statement = ExpressionStatement { expression: exp };
		Some(statement)
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
		let mut left = match self.curr_token.token_type {
			TokenType::Identifier => {
				Some(Expression::Identifier(self.parse_identifier()))
			}
			TokenType::Integer => {
				Some(Expression::IntegerLiteral(self.parse_integer_literal()))
			}
			TokenType::Minus | TokenType::Exclamation | TokenType::Tilde => {
				Some(Expression::Prefix(Box::new(self.parse_prefix_expression()?))) 
			}
			TokenType::True | TokenType::False => {
				Some(Expression::Boolean(self.parse_boolean()))
			}
			TokenType::Lparen => {
				Some(self.parse_grouped_expression()?)
			}
			TokenType::If => {
				Some(Expression::IfExpression(Box::new(self.parse_if_expression()?)))
			}
			_ => {
				self.no_prefix_parser_error(self.curr_token.token_type.clone());
				None
			}
		};

		while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
			println!("Peeked token: {:?}, Precedence: {:?}", self.next_token.token_type, self.peek_precedence());
			left = match self.next_token.token_type {
				TokenType::Plus
					| TokenType::Minus
					| TokenType::Asterisk
					| TokenType::Slash
					| TokenType::Equals_Equals
					| TokenType::Not_Equals
					| TokenType::Lt
					| TokenType::Lteq
					| TokenType::Gt
					| TokenType::Gteq => {
						self.next_token();
						Some(Expression::Infix(Box::new(self.parse_infix_expression(left?)?)))
					}
				_ => return None
			};
		}
		left
	}

	fn parse_prefix_expression(&mut self) -> Option<Prefix> {
		let current_token = self.curr_token.clone();
		let operator = current_token.literal.clone();

		self.next_token();

		let rhs = self.parse_expression(Precedence::Prefix)?;
		
		let prefix = Prefix { operator, rhs };
		Some(prefix)
	}

	fn parse_infix_expression(&mut self, lhs: Expression) -> Option<Infix> {
		let operator = self.curr_token.literal.clone();
		let precedence = self.get_current_precedence();

		self.next_token();
		let rhs = self.parse_expression(precedence)?;

		let infix = Infix { operator, lhs, rhs };
		Some(infix)
	}
	
	fn parse_grouped_expression(&mut self) -> Option<Expression> {
		self.next_token();
		let exp = self.parse_expression(Precedence::Lowest);

		if !(self.next_token.token_type == TokenType::Rparen) {
			self.peek_error(TokenType::Rparen);
			return None;
		}
		self.next_token();
		return exp;
	}

	fn parse_if_expression(&mut self) -> Option<IfExpression> {
		// TODO: Reconsider parenthesis for if statements
		if !(self.next_token.token_type == TokenType::Lparen) {
			self.peek_error(TokenType::Lparen);
			return None;
		}
		self.next_token();

		let condition = self.parse_expression(Precedence::Lowest).unwrap(); // TODO: REVISIT

		if !(self.curr_token.token_type == TokenType::Rparen) {
			self.peek_error(TokenType::Rparen);
			return None;
		}
		self.next_token();

		let consequence = self.parse_block_statement();
		let alternative = if self.next_token.token_type == TokenType::Else {
			self.next_token();
			// TODO: Replace with consume_on_match use (why isn't it plug-and-play?)
			if self.next_token.token_type != TokenType::Lbrace {
				self.peek_error(TokenType::Lbrace);
				return None;
			}
			Some(self.parse_block_statement())
		} else {
			None
		};
		let exp = IfExpression { condition, consequence, alternative };
		Some(exp)
	}

	fn parse_block_statement(&mut self) -> BlockStatement {
		self.next_token(); 
		let mut statements = Vec::new();
		while !(self.curr_token.token_type == TokenType::Rbrace) && !(self.curr_token.token_type == TokenType::Eof) {
			if let Some(stmt) = self.parse_statement() {
				statements.push(stmt);
			}
			self.next_token();
		}
		return BlockStatement { statements };
	}

	fn parse_identifier(&self) -> Identifier {
		Identifier { value: self.curr_token.literal.clone() }
	}

	fn parse_integer_literal(&self) -> IntegerLiteral {
		let value = self.curr_token.literal.parse::<i64>().unwrap();
		IntegerLiteral { value }
	}

	fn parse_boolean(&self) -> Boolean {
		Boolean { value: self.curr_token_is(TokenType::True) }
	}

	fn get_current_precedence(&self) -> Precedence {
		match self.curr_token.token_type {
			TokenType::Equals_Equals | TokenType::Not_Equals => Precedence::Equals,
			TokenType::Lt | TokenType::Gt     => Precedence::LessGreater,
			TokenType::Plus | TokenType::Minus => Precedence::Sum,
			TokenType::Slash | TokenType::Asterisk => Precedence::Product,
			_ => Precedence::Lowest, 
		}
	}

	fn peek_precedence(&self) -> Precedence {
		match self.next_token.token_type {
			TokenType::Equals_Equals | TokenType::Not_Equals => Precedence::Equals,
			TokenType::Lt | TokenType::Gt     => Precedence::LessGreater,
			TokenType::Plus | TokenType::Minus => Precedence::Sum,
			TokenType::Slash | TokenType::Asterisk => Precedence::Product,
			_ => Precedence::Lowest, 
		}
	}

	fn synchronize_statement(&mut self) {
		while self.curr_token.token_type != TokenType::Semicolon && self.curr_token.token_type != TokenType::Eof {
			self.next_token()
		}
	}

	fn curr_token_is(&self, token: TokenType) -> bool {
		return self.curr_token.token_type == token;
	}

	fn no_prefix_parser_error(&mut self, token_type: TokenType) {
		let message = format!("No prefix parse function for {:?} found", token_type);
		self.errors.push(message);
	}

	fn peek_error(&mut self, token_type: TokenType) {
		let message = format!(
			"Expected next token to be {:?}, got {:?} instead",
			token_type, self.next_token.token_type
		);
		self.errors.push(message);
	}

	fn peek_token_is(&mut self, expected_type: TokenType) -> bool {
		self.next_token.token_type == expected_type 
	}

	fn peek_and_consume_on_match(&mut self, expected_type: TokenType) -> bool {
		if self.next_token.token_type == expected_type {
			self.next_token();
			true
		} else {
			self.peek_error(expected_type);
			false
		}
	}
}

#[cfg(test)]
mod tests {
    use super::*;
	
    #[test]
    fn test_let_statement() {
        let source = r#"
           let x = 5;
        "#;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

		if let Statement::Let(let_statement) = &program.statements[0] {
			assert_eq!(let_statement.name.value, "x");
			
			if let Expression::IntegerLiteral(int_lit) = &let_statement.value {
				assert_eq!(int_lit.value, 5);
			} else {
				panic!("expected IntegerLiteral");
			}
		} else {
			panic!("expected let statement");
		}
		println!("Statements: {:#?}", program.statements);
		println!("Errors: {:#?}", parser.errors);
    }

    #[test]
    fn test_return_statement() {
        let source = r#"
            return 5;
        "#;
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
		
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

		if let Statement::Return(return_statement) = &program.statements[0] {
			if let Expression::IntegerLiteral(int_lit) = &return_statement.value {
				assert_eq!(int_lit.value, 5)
			} else {
				panic!("expected IntegerLiteral");
			}
		} else {
			panic!("expected return statement")
		}
		println!("Statement: {:#?}", program.statements);
		println!("Errors: {:#?}", parser.errors);
    }

    #[test]
    fn test_parser_errors() {
        let source = r#"
            let = 5;
            let x 828282;
            let 6767;
        "#;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();
		println!("Reported errors: {:#?}", parser.errors);
        assert_eq!(parser.errors.len(), 3);
    }

	#[test]
	fn test_expression() {
		let source = "foobar";

		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		if let Statement::Expression(exp_statement) = &program.statements[0] {
			if let Expression::Identifier(id) = &exp_statement.expression {
				assert_eq!(id.value, "foobar");
			} else {
				panic!("expected identifier");
			}
		} else {
			panic!("expected identifier expression")
		}
		println!("{:#?}", program.statements);
		assert_eq!(program.statements.len(), 1);
		assert_eq!(parser.errors.len(), 0);
	}

	#[test]
	fn test_integer_literal() {
		let source = "1267";
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		if let Statement::Expression(exp_statement) = &program.statements[0] {
			if let Expression::IntegerLiteral(int_lit) = &exp_statement.expression {
				assert_eq!(int_lit.value, 1267);
			} else {
				panic!("expected integer literal");
			}
		} else {
			panic!("expected integer literal")
		}
		println!("{:#?}", program.statements);
		assert_eq!(program.statements.len(), 1);
		assert_eq!(parser.errors.len(), 0);
	}

	#[test]
	fn test_prefix_expression() {
		let source = r#"
            let foo = 10;
            let bar = !foobar;
            let baz = -foo;
            ~foo;
        "#;
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		println!("{:#?}", program.statements);
		println!("{:?}", parser.errors);
		assert_eq!(program.statements.len(), 4);
		assert_eq!(parser.errors.len(), 0);
	}

	// TODO: Heavily flesh out with helper functions for readability. Split into operator precedence specific test cases
	#[test]
	fn test_infix_expression() {
		let source = r#"
            x + y;
            x == y;
            3 + 4 * 5 == 3 * 1 + 4 * 5;
            -1 * 2 + 3;
        "#;
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		println!("{:#?}", program.statements);
		println!("{:?}", parser.errors);
		assert_eq!(program.statements.len(), 4);
		assert_eq!(parser.errors.len(), 0);
	}

	#[test]
	fn test_boolean() {
		let source = r#"
            3 > 5 == false;
        "#;
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		println!("{:#?}", program.statements);
		println!("{:?}", parser.errors);
		assert_eq!(program.statements.len(), 1);
		assert_eq!(parser.errors.len(), 0);
	}

	#[test]
	fn test_grouped_expression() {
		let source = r#"
            1 + (2 + 3) + 4;
            (5 + 5) * 2;
            -(5 + 5);
            !(true == true);
            2 / (5 + 5);
        "#;
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		println!("{:#?}", program.statements);
		println!("{:?}", parser.errors);
		assert_eq!(parser.errors.len(), 0);
	}

	#[test]
	fn test_if_expression() {
		let source = r#"
            let x = if (true) {
                4 / 2;
            }
            let y = if (false) {
                4 / 0;
            } else {
                4 / 1;
            }
        "#;
		let lexer = Lexer::new(source);
		let mut parser = Parser::new(lexer);

		let program = parser.parse_program();
		println!("{:#?}", program.statements);
		println!("{:?}", parser.errors);
	}
}
