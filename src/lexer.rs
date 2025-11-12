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

    fn eat_whitespace(&mut self) {
		while let Some(ch) = self.peek_char() {
			if ch.is_whitespace() {
				self.read_char().unwrap();
			} else {
				break;
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
			if is_letter(ch) {
				self.read_char();
			} else {
				break;
			}
		}
		self.input[start_pos..self.cursor].into()
	}

	fn read_number(&mut self, start_pos: usize) -> String {
		while let Some(ch) = self.peek_char() {
			if is_digit(ch) || ch == '_' || ch == ',' {
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
		if let Some(peeked_char) = self.peek_char() {
			if peeked_char == expected {
				self.read_char();
				return Token::new(compound_type, compound_literal);
			}
		}
		Token::new(single_type, single_literal)
	}

    pub fn next_token(&mut self) -> Token {
		self.eat_whitespace();
		let start_pos = self.cursor;
        let current_char =  match self.read_char() {
            Some(token) => token,
            None => ' ',
        };

        let literal = current_char.into();

        match current_char {
            '{' => { Token::new(TokenType::LBRACE, literal) }
            '}' => { Token::new(TokenType::RBRACE, literal) }
            '(' => { Token::new(TokenType::LPAREN, literal) }
            ')' => { Token::new(TokenType::RPAREN, literal) }
			'[' => { Token::new(TokenType::LSQ_BRACKET, literal) }
			']' => { Token::new(TokenType::RSQ_BRACKET, literal) }
			'%' => { Token::new(TokenType::MOD, literal) }
            ':' => { Token::new(TokenType::COLON, literal) }			
            ';' => { Token::new(TokenType::SEMICOLON, literal) }
            ',' => { Token::new(TokenType::COMMA, literal) }
			'~' => { Token::new(TokenType::TILDE, literal) }
			'*' => { self.read_compound_token('=', TokenType::MULTIPLY_ASSIGN, "*=".into(), TokenType::ASTERISK, "*".into()) }
			'+' => { self.read_compound_token('=', TokenType::PLUS_ASSIGN, "+=".into(), TokenType::PLUS, "+".into()) }
			'-' => { self.read_compound_token('=', TokenType::MINUS_ASSIGN, "-=".into(), TokenType::MINUS, "-".into()) }			
			'=' => { self.read_compound_token('=', TokenType::EQUALS, "==".into(), TokenType::ASSIGN, "=".into()) }
			'!' => { self.read_compound_token('=', TokenType::NOT_EQUALS, "!=".into(), TokenType::EXCLAMATION, "!".into()) }
			'^' => { self.read_compound_token('=', TokenType::XOR, "^=".into(), TokenType::CARET, "^".into()) }
			'|' => { self.read_compound_token('|', TokenType::OR, "||".into(), TokenType::PIPE, "|".into()) }
			'&' => { self.read_compound_token('&', TokenType::AND, "&&".into(), TokenType::AMP, "&".into()) }
			'<' => { self.read_compound_token('=', TokenType::LTEQ, "<=".into(), TokenType::LT, "<".into()) }
			'>' => { self.read_compound_token('=', TokenType::GTEQ, ">=".into(), TokenType::GT, ">".into()) }
			'/' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '/' {
						self.read_char();
						self.eat_until_newline();
						return Token::new(TokenType::COMMENT, "//".into());
					}
					if peeked_char == '/' {
						self.read_char();
						return Token::new(TokenType::DIVIDE_ASSIGN, "/=".into());
					}
				}
				Token::new(TokenType::DIVIDE, literal)
			}
            ' ' => { Token::new(TokenType::EOF, " ".into()) }
			_ if is_letter(current_char) => {
				let identifier = self.read_identifier(start_pos);
				return Token::new(lookup_identifier(&identifier), identifier);
			}
			_ if is_digit(current_char) => {
				Token::new(TokenType::INTEGER, self.read_number(start_pos))
			}
            _ => { Token::new(TokenType::ILLEGAL, literal) }
		}
	}
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
}

fn is_digit(ch: char) -> bool {
	return '0' <= ch && ch <= '9';
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
            Token::new(TokenType::LET, "let".into()),
            Token::new(TokenType::IDENTIFIER, "x".into()),
            Token::new(TokenType::ASSIGN, "=".into()),
            Token::new(TokenType::INTEGER, "15".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
			
			Token::new(TokenType::LET, "let".into()),
            Token::new(TokenType::IDENTIFIER, "y".into()),
            Token::new(TokenType::ASSIGN, "=".into()),
            Token::new(TokenType::INTEGER, "10".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
			
			Token::new(TokenType::LET, "let".into()),
			Token::new(TokenType::IDENTIFIER, "add".into()),
			Token::new(TokenType::ASSIGN, "=".into()),
			Token::new(TokenType::FUNCTION, "fn".into()),
			Token::new(TokenType::LPAREN, "(".into()),
            Token::new(TokenType::IDENTIFIER, "x".into()),
			Token::new(TokenType::COMMA, ",".into()),
			Token::new(TokenType::IDENTIFIER, "y".into()),
			Token::new(TokenType::RPAREN, ")".into()),
            Token::new(TokenType::LBRACE, "{".into()),
			Token::new(TokenType::IDENTIFIER, "x".into()),
			Token::new(TokenType::PLUS, "+".into()),
			Token::new(TokenType::IDENTIFIER, "y".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
            Token::new(TokenType::RBRACE, "}".into()),
            Token::new(TokenType::SEMICOLON, ";".into()),

			Token::new(TokenType::LET, "let".into()),
			Token::new(TokenType::IDENTIFIER, "result".into()),
			Token::new(TokenType::ASSIGN, "=".into()),
			Token::new(TokenType::IDENTIFIER, "add".into()),
			Token::new(TokenType::LPAREN, "(".into()),
            Token::new(TokenType::IDENTIFIER, "x".into()),
			Token::new(TokenType::COMMA, ",".into()),
			Token::new(TokenType::IDENTIFIER, "y".into()),
			Token::new(TokenType::RPAREN, ")".into()),
            Token::new(TokenType::SEMICOLON, ";".into()),
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
			Token::new(TokenType::IF, "if".into()),
			Token::new(TokenType::LPAREN, "(".into()),
			Token::new(TokenType::INTEGER, "5".into()),
			Token::new(TokenType::LTEQ, "<=".into()),
			Token::new(TokenType::INTEGER, "10".into()),
			Token::new(TokenType::RPAREN, ")".into()),
			Token::new(TokenType::LBRACE, "{".into()),
			Token::new(TokenType::RETURN, "return".into()),
			Token::new(TokenType::TRUE, "true".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
			Token::new(TokenType::RBRACE, "}".into()),
			Token::new(TokenType::ELSE, "else".into()),
			Token::new(TokenType::LBRACE, "{".into()),
			Token::new(TokenType::RETURN, "return".into()),
			Token::new(TokenType::EXCLAMATION, "!".into()),
			Token::new(TokenType::FALSE, "false".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
			Token::new(TokenType::RBRACE, "}".into()),
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
			Token::new(TokenType::COMMENT, "//".into()),
            Token::new(TokenType::LET, "let".into()),
            Token::new(TokenType::IDENTIFIER, "x".into()),
            Token::new(TokenType::ASSIGN, "=".into()),
            Token::new(TokenType::INTEGER, "0".into()),
			Token::new(TokenType::SEMICOLON, ";".into()),
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
			Token::new(TokenType::LT, "<".into()),
			Token::new(TokenType::GT, ">".into()),
			Token::new(TokenType::ASSIGN, "=".into()),
			Token::new(TokenType::PIPE, "|".into()),
			Token::new(TokenType::AMP, "&".into()),
			Token::new(TokenType::EXCLAMATION, "!".into()),
			Token::new(TokenType::LTEQ, "<=".into()),
			Token::new(TokenType::GTEQ, ">=".into()),
			Token::new(TokenType::OR, "||".into()),			
			Token::new(TokenType::AND, "&&".into()),
			Token::new(TokenType::NOT_EQUALS, "!=".into()),			
			Token::new(TokenType::EQUALS, "==".into()),			
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
			Token::new(TokenType::INTEGER, "100,000".into()),
			Token::new(TokenType::INTEGER, "100_000".into()),
		];
		
		let mut lexer = Lexer::new(source);

		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
    }
}
