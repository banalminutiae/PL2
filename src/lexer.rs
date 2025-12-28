use crate::token::{Token, TokenType, lookup_identifier};

#[derive(Clone)]
pub struct Lexer<'a> {
	input: &'a str,
	cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
			input,
			cursor: 0,
        }
    }

    fn read_char(&mut self) -> Option<char> {
		let remaining_input = &self.input[self.cursor..];

		let mut chars = remaining_input.chars(); 
		
		if let Some(c) = chars.next() {
            self.cursor += c.len_utf8();
            Some(c)
        } else {
            None
        }
	} 

    fn peek_char(&mut self) -> Option<char> {
        self.input[self.cursor..].chars().next()
    }

    fn eat_whitespace_and_comments(&mut self) {
		while let Some(ch) = self.peek_char() {
			match Some(ch) {
				Some(ch) if (ch.is_whitespace()) => {
					self.read_char();
				}
				Some('/') => {
					self.read_char();
					self.read_char();
					self.eat_until_newline();
				}
				_ => break,
			}
		}
	}

	fn eat_until_newline(&mut self) {
		while let Some(ch) = self.peek_char() {
			if ch != '\n' {
				self.read_char().unwrap();
			} else {
				break;
			}
		}
	}

	fn read_identifier(&mut self, start_pos: usize) -> String {
		while let Some(ch) = self.peek_char() {
			if ch.is_alphabetic() {
				self.read_char();
			} else {
				break;
			}
		}
		self.input[start_pos..self.cursor].into()
	}

	fn read_number(&mut self, start_pos: usize) -> String {
		while let Some(ch) = self.peek_char() {
			if ch.is_ascii_digit() || ch == '_' || ch == ',' {
				self.read_char();
			} else {
				break;
			}
		}
		self.input[start_pos..self.cursor].into()
	}

	fn read_compound_token(
		&mut self,
		expected: char,
		compound_type: TokenType,
		compound_literal: String,
		single_type: TokenType,
		single_literal: String,
	) -> Token {
		if let Some(peeked_char) = self.peek_char() && peeked_char == expected {
			self.read_char();
			return Token::new(compound_type, compound_literal);
		}
		Token::new(single_type, single_literal)
	}

    pub fn next_token(&mut self) -> Token {
		self.eat_whitespace_and_comments();
		let start_pos = self.cursor;
        let current_char = self.read_char().unwrap_or(' ');

        let literal = current_char.into();

        match current_char {
            '{' => { Token::new(TokenType::Lbrace, literal) }
            '}' => { Token::new(TokenType::Rbrace, literal) }
            '(' => { Token::new(TokenType::Lparen, literal) }
            ')' => { Token::new(TokenType::Rparen, literal) }
			'[' => { Token::new(TokenType::Lsq_Bracket, literal) }
			']' => { Token::new(TokenType::Rsq_Bracket, literal) }
            ':' => { Token::new(TokenType::Colon, literal) }			
            ';' => { Token::new(TokenType::Semicolon, literal) }
            ',' => { Token::new(TokenType::Comma, literal) }
			'~' => { Token::new(TokenType::Tilde, literal) }
			'%' => { self.read_compound_token('=', TokenType::Percent_Equals, "%=".into(), TokenType::Percent, "%".into()) }
			'*' => { self.read_compound_token('=', TokenType::Multiply_Equals, "*=".into(), TokenType::Asterisk, "*".into()) }
			'+' => { self.read_compound_token('=', TokenType::Plus_Equals, "+=".into(), TokenType::Plus, "+".into()) }
			'-' => { self.read_compound_token('=', TokenType::Minus_Equals, "-=".into(), TokenType::Minus, "-".into()) }
			'/' => { self.read_compound_token('=', TokenType::Divide_Equals, "/=".into(), TokenType::Divide, "/".into()) }
			'=' => { self.read_compound_token('=', TokenType::Equals_Equals, "==".into(), TokenType::Equals, "=".into()) }
			'!' => { self.read_compound_token('=', TokenType::Not_Equals, "!=".into(), TokenType::Exclamation, "!".into()) }
			'^' => { self.read_compound_token('=', TokenType::Caret_Equals, "^=".into(), TokenType::Caret, "^".into()) }
			'|' => { self.read_compound_token('|', TokenType::Or, "||".into(), TokenType::Pipe, "|".into()) }
			'&' => { self.read_compound_token('&', TokenType::And, "&&".into(), TokenType::Amp, "&".into()) }
			'<' => { self.read_compound_token('=', TokenType::Lteq, "<=".into(), TokenType::Lt, "<".into()) }
			'>' => { self.read_compound_token('=', TokenType::Gteq, ">=".into(), TokenType::Gt, ">".into()) }
            ' ' => { Token::new(TokenType::EOF, " ".into()) }
			_ if current_char.is_alphabetic() => {
				let identifier = self.read_identifier(start_pos);
				Token::new(lookup_identifier(&identifier), identifier)
			}
			_ if current_char.is_ascii_digit() => {
				Token::new(TokenType::Integer, self.read_number(start_pos))
			}
            _ => { Token::new(TokenType::Illegal, literal) }
		}
	}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_declarations<'a>() {
        let source = r#"
            let x = 15;
            let y = 10;

            let add = fn (x, y) {
                x + y;
            };

            let result = add(x, y);
        "#;
        let lexer_cases = [
            Token::new(TokenType::Let, "let".into()),
            Token::new(TokenType::Identifier, "x".into()),
            Token::new(TokenType::Equals, "=".into()),
            Token::new(TokenType::Integer, "15".into()),
			Token::new(TokenType::Semicolon, ";".into()),
			
			Token::new(TokenType::Let, "let".into()),
            Token::new(TokenType::Identifier, "y".into()),
            Token::new(TokenType::Equals, "=".into()),
            Token::new(TokenType::Integer, "10".into()),
			Token::new(TokenType::Semicolon, ";".into()),
			
			Token::new(TokenType::Let, "let".into()),
			Token::new(TokenType::Identifier, "add".into()),
			Token::new(TokenType::Equals, "=".into()),
			Token::new(TokenType::Function, "fn".into()),
			Token::new(TokenType::Lparen, "(".into()),
            Token::new(TokenType::Identifier, "x".into()),
			Token::new(TokenType::Comma, ",".into()),
			Token::new(TokenType::Identifier, "y".into()),
			Token::new(TokenType::Rparen, ")".into()),
            Token::new(TokenType::Lbrace, "{".into()),
			Token::new(TokenType::Identifier, "x".into()),
			Token::new(TokenType::Plus, "+".into()),
			Token::new(TokenType::Identifier, "y".into()),
			Token::new(TokenType::Semicolon, ";".into()),
            Token::new(TokenType::Rbrace, "}".into()),
            Token::new(TokenType::Semicolon, ";".into()),

			Token::new(TokenType::Let, "let".into()),
			Token::new(TokenType::Identifier, "result".into()),
			Token::new(TokenType::Equals, "=".into()),
			Token::new(TokenType::Identifier, "add".into()),
			Token::new(TokenType::Lparen, "(".into()),
            Token::new(TokenType::Identifier, "x".into()),
			Token::new(TokenType::Comma, ",".into()),
			Token::new(TokenType::Identifier, "y".into()),
			Token::new(TokenType::Rparen, ")".into()),
            Token::new(TokenType::Semicolon, ";".into()),
			Token::new(TokenType::EOF, " ".into()),
        ];

        let mut lexer = Lexer::new(source);

        for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
        }
    }

	#[test]
	fn test_conditionals<'a>() {
		let source = r#"
            if (5 <= 10) {
                return true;
            } else {
                return !false;
            }
        "#;

		let lexer_cases = [
			Token::new(TokenType::If, "if".into()),
			Token::new(TokenType::Lparen, "(".into()),
			Token::new(TokenType::Integer, "5".into()),
			Token::new(TokenType::Lteq, "<=".into()),
			Token::new(TokenType::Integer, "10".into()),
			Token::new(TokenType::Rparen, ")".into()),
			Token::new(TokenType::Lbrace, "{".into()),
			Token::new(TokenType::Return, "return".into()),
			Token::new(TokenType::True, "true".into()),
			Token::new(TokenType::Semicolon, ";".into()),
			Token::new(TokenType::Rbrace, "}".into()),
			Token::new(TokenType::Else, "else".into()),
			Token::new(TokenType::Lbrace, "{".into()),
			Token::new(TokenType::Return, "return".into()),
			Token::new(TokenType::Exclamation, "!".into()),
			Token::new(TokenType::False, "false".into()),
			Token::new(TokenType::Semicolon, ";".into()),
			Token::new(TokenType::Rbrace, "}".into()),
			Token::new(TokenType::EOF, " ".into()),
		];
		

		let mut lexer = Lexer::new(source);

		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
	}

	#[test]
	fn test_comment<'a>() {
		let source = r#"
            // You should never read this
            let x = 0;
        "#;

		let lexer_cases = [
            Token::new(TokenType::Let, "let".into()),
            Token::new(TokenType::Identifier, "x".into()),
            Token::new(TokenType::Equals, "=".into()),
            Token::new(TokenType::Integer, "0".into()),
			Token::new(TokenType::Semicolon, ";".into()),
		];

		let mut lexer = Lexer::new(source);

		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
	}

	#[test]
	fn test_compounds<'a>() {
		let source = r#"
            < > = | & ! 
            <= >= || && != == 
        "#;

		
		let mut lexer = Lexer::new(source);

		let lexer_cases = [
			Token::new(TokenType::Lt, "<".into()),
			Token::new(TokenType::Gt, ">".into()),
			Token::new(TokenType::Equals, "=".into()),
			Token::new(TokenType::Pipe, "|".into()),
			Token::new(TokenType::Amp, "&".into()),
			Token::new(TokenType::Exclamation, "!".into()),
			Token::new(TokenType::Lteq, "<=".into()),
			Token::new(TokenType::Gteq, ">=".into()),
			Token::new(TokenType::Or, "||".into()),			
			Token::new(TokenType::And, "&&".into()),
			Token::new(TokenType::Not_Equals, "!=".into()),			
			Token::new(TokenType::Equals_Equals, "==".into()),			
		];
		
		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
	}

	#[test]
	fn test_number_formats<'a>() {
		let source = r#"
            100,000
            100_000
        "#;

		let lexer_cases = [
			Token::new(TokenType::Integer, "100,000".into()),
			Token::new(TokenType::Integer, "100_000".into()),
		];
		
		let mut lexer = Lexer::new(source);

		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
    }
}
