use crate::token::{Token, TokenType};
use std::iter::{ Peekable};
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer { 
            input: &input,
            chars: input.chars().peekable(),
         }
    }

    fn read_char(&mut self) -> Option<char> {
		let ch  = match self.chars.next() {
			Some(ch) => ch,
			None => ' ',
		};
		Some(ch)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn eat_whitespace(&mut self) {
		while let Some(&ch) = self.peek_char() {
			if ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n' {
				self.read_char().unwrap();
			} else {
				break;
			}
		}
	}

	fn read_identifier(&mut self, current_char: char) -> String {
		let mut ident = String::new();
		ident.push(current_char);
		while let Some(&ch) = self.peek_char() {
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
		while let Some(&ch) = self.peek_char() {
			if is_digit(ch) {
				ident.push(self.read_char().unwrap());
			} else {
				break;
			}
		}
		ident
	}

    fn next_token(&mut self) -> Token {
		self.eat_whitespace();
        let current_char =  match self.read_char() {
            Some(token) => token,
            None => ' ',
        };

        let literal = current_char.to_string();

        match current_char {
            '=' => {
                if let Some(&peeked_char) = self.peek_char() {
                    if peeked_char == '=' {
                        self.read_char();
                        return Token::new(TokenType::EQUALS, "==".to_string());
                    } 
                }
                Token::new(TokenType::ASSIGN, "=".to_string())
            }
            '{' => { Token::new(TokenType::LBRACE, literal) }
            '}' => { Token::new(TokenType::RBRACE, literal) }
            '(' => { Token::new(TokenType::LPAREN, literal) }
            ')' => { Token::new(TokenType::RPAREN, literal) }
            ';' => { Token::new(TokenType::SEMICOLON, literal) }
            ',' => { Token::new(TokenType::COMMA, literal) }
            '+' => { Token::new(TokenType::PLUS, literal) }
            '-' => { Token::new(TokenType::MINUS, literal) }
			'|' => { Token::new(TokenType::PIPE, literal) }
			'*' => { Token::new(TokenType::ASTERISK, literal) }
			'<' => { Token::new(TokenType::LT, literal) }
			'>' => { Token::new(TokenType::GT, literal) }
            ' ' => { Token::new(TokenType::EOF, literal) }
            _  => {
				if is_letter(current_char) {
					Token::new(TokenType::IDENTIFIER, self.read_identifier(current_char).to_string())
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
    fn test_next_token<'a>() {
        let source = "==+(){}  ,;let|965";

        let lexer_cases = [
            Token::new(TokenType::EQUALS, "==".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),       
            Token::new(TokenType::SEMICOLON, ";".to_string()),
			Token::new(TokenType::IDENTIFIER, "let".to_string()),
			Token::new(TokenType::PIPE, "|".to_string()),
			Token::new(TokenType::INTEGER, "965".to_string()),
            Token::new(TokenType::EOF, " ".to_string())     
        ];

        let mut lexer = Lexer::new(source);

        for (index, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
        }
    }
}


