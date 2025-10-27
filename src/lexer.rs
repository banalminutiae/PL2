use crate::token::{Token, TokenType, lookup_identifier};
use std::iter::{Peekable};
use std::str::Chars;

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

	fn read_identifier(&mut self, current_char: char) -> String {
		let mut ident = String::new();
		ident.push(current_char);
		while let Some(ch) = self.peek_char() {
			if is_letter(ch) {
				ident.push(self.read_char().unwrap());
			} else {
				break;
			}
		}
		ident
	}

	fn read_number(&mut self, current_char: char) -> String {
		let mut ident = String::new();
		ident.push(current_char);
		while let Some(ch) = self.peek_char() {
			if is_digit(ch) {
				ident.push(self.read_char().unwrap());
			} else {
				break;
			}
		}
		ident
	}

    pub fn next_token(&mut self) -> Token {
		self.eat_whitespace();
        let current_char =  match self.read_char() {
            Some(token) => token,
            None => ' ',
        };

        let literal = current_char.to_string();

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
			'^' => { Token::new(TokenType::CARET, literal) }
			'~' => { Token::new(TokenType::TILDE, literal) }
            '+' => { Token::new(TokenType::PLUS, literal) }
            '-' => { Token::new(TokenType::MINUS, literal) }
			'*' => { Token::new(TokenType::ASTERISK, literal) }
			'=' => {
                if let Some(peeked_char) = self.peek_char() {
                    if peeked_char == '=' {
                        self.read_char();
                        return Token::new(TokenType::EQUALS, "==".to_string());
                    } 
                }
                Token::new(TokenType::ASSIGN, "=".to_string())
            }
			'!' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '=' {
						self.read_char();
						return Token::new(TokenType::NOT_EQUALS, "!=".to_string());
					}
				}
				Token::new(TokenType::EXCLAMATION, literal)
			}
			'/' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '/' {
						self.read_char();
						self.eat_until_newline();
						return Token::new(TokenType::COMMENT, "//".to_string());
					}
				}
				Token::new(TokenType::DIVIDE, literal) }
			'|' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '|' {
						self.read_char();
						return Token::new(TokenType::OR, "||".to_string());
					}
				}
				Token::new(TokenType::PIPE, literal)
			}
			'&' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '&' {
						self.read_char();
						return Token::new(TokenType::AND, "&&".to_string());
					}
				}
				Token::new(TokenType::AMP, literal)
			}
			'<' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '=' {
						self.read_char();
						return Token::new(TokenType::LTEQ, "<=".to_string());
					}
				}
				Token::new(TokenType::LT, literal)
			}
			'>' => {
				if let Some(peeked_char) = self.peek_char() {
					if peeked_char == '=' {
						self.read_char();
						return Token::new(TokenType::GTEQ, "<=".to_string());
					}
				}
				Token::new(TokenType::GT, literal) }
            ' ' => { Token::new(TokenType::EOF, " ".to_string()) }
            _ => {
				if is_letter(current_char) {
					let identifier = self.read_identifier(current_char).to_string();
					Token::new(lookup_identifier(&identifier), identifier)
				} else if is_digit(current_char) {
					Token::new(TokenType::INTEGER, self.read_number(current_char).to_string())
				} else {
					Token::new(TokenType::ILLEGAL, literal)
				}
			}
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
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENTIFIER, "x".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INTEGER, "15".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
			
			Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENTIFIER, "y".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INTEGER, "10".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
			
			Token::new(TokenType::LET, "let".to_string()),
			Token::new(TokenType::IDENTIFIER, "add".to_string()),
			Token::new(TokenType::ASSIGN, "=".to_string()),
			Token::new(TokenType::FUNCTION, "fn".to_string()),
			Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENTIFIER, "x".to_string()),
			Token::new(TokenType::COMMA, ",".to_string()),
			Token::new(TokenType::IDENTIFIER, "y".to_string()),
			Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
			Token::new(TokenType::IDENTIFIER, "x".to_string()),
			Token::new(TokenType::PLUS, "+".to_string()),
			Token::new(TokenType::IDENTIFIER, "y".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),

			Token::new(TokenType::LET, "let".to_string()),
			Token::new(TokenType::IDENTIFIER, "result".to_string()),
			Token::new(TokenType::ASSIGN, "=".to_string()),
			Token::new(TokenType::IDENTIFIER, "add".to_string()),
			Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENTIFIER, "x".to_string()),
			Token::new(TokenType::COMMA, ",".to_string()),
			Token::new(TokenType::IDENTIFIER, "y".to_string()),
			Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
			Token::new(TokenType::EOF, " ".to_string()),
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
                return false;
            }
        "#;

		let lexer_cases = [
			Token::new(TokenType::IF, "if".to_string()),
			Token::new(TokenType::LPAREN, "(".to_string()),
			Token::new(TokenType::INTEGER, "5".to_string()),
			Token::new(TokenType::LTEQ, "<=".to_string()),
			Token::new(TokenType::INTEGER, "10".to_string()),
			Token::new(TokenType::RPAREN, ")".to_string()),
			Token::new(TokenType::LBRACE, "{".to_string()),
			Token::new(TokenType::RETURN, "return".to_string()),
			Token::new(TokenType::TRUE, "true".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
			Token::new(TokenType::RBRACE, "}".to_string()),
			Token::new(TokenType::ELSE, "else".to_string()),
			Token::new(TokenType::LBRACE, "{".to_string()),
			Token::new(TokenType::RETURN, "return".to_string()),
			Token::new(TokenType::FALSE, "false".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
			Token::new(TokenType::RBRACE, "}".to_string()),
			Token::new(TokenType::EOF, " ".to_string()),
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
			Token::new(TokenType::COMMENT, "//".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENTIFIER, "x".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INTEGER, "0".to_string()),
			Token::new(TokenType::SEMICOLON, ";".to_string()),
		];

		let mut lexer = Lexer::new(source);

		for (_, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
		}
	}
}
