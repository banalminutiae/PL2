use crate::token::{Token, TokenType};
use std::iter::{ Peekable};
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    read_position: i32,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let lexer =  Lexer { 
            input: &input,
            read_position: 1, 
            chars: input.chars().peekable(),
         };
         return lexer;
    }

    fn read_char(&mut self) -> Option<char> {
        self.read_position += 1;
        return self.chars.next();
    }

    fn peek_char(&mut self) -> Option<&char> {
        return self.chars.peek();
    }

    fn eat_whitespace(&mut self) {
		if let Some(&peeked_char) = self.peek_char() {
			if peeked_char == ' ' || peeked_char == '\r' || peeked_char == '\t' || peeked_char == '\n' {
				self.read_char().unwrap();
			}			
		}
    }

    fn next_token(&mut self) -> Token {
		self.eat_whitespace();
        let current_char =  match self.read_char() {
            Some(token) => token,
            None => '0'
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
            '0' => { Token::new(TokenType::EOF, literal) }
            _  => { Token::new(TokenType::ILLEGAL, literal) }
        }
    }
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch >= 'z' || 'A' <= ch && ch >= 'Z' || ch == '_';
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token<'a>() {
        let source = "==+(){} ,;|";

        let lexer_cases = [
            Token::new(TokenType::EQUALS, "==".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),       
            Token::new(TokenType::SEMICOLON, ";".to_string()),
			Token::new(TokenType::ILLEGAL, "|".to_string()),
            Token::new(TokenType::EOF, "0".to_string())     
        ];

        let mut lexer = Lexer::new(source);

        for (index, expected_token) in lexer_cases.iter().enumerate() {
            let actual_token = lexer.next_token();
            println!("Expected: {:?}, Got: {:?}", expected_token, actual_token);
            assert_eq!(&actual_token, expected_token);
        }
    }
}


