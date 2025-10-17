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
        if let Some(c) = self.chars.next() {
            self.read_position += 1;
            return Some(c);
        } else {
            return None;
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        return self.chars.peek();
    }

    fn next_token(&mut self) -> Token {
        let current_char = self.read_char().unwrap();
        let literal = current_char.to_string();

        match current_char {
            '=' => {
                if let Some(&peeked_char) = self.peek_char() {
                    if peeked_char == '=' {
                        self.read_char();
                        return Token { token_type: TokenType::EQUALS, literal }
                    } 
                } 
                Token { token_type: TokenType::ASSIGN, literal }
            }
            '{' => { Token { token_type: TokenType::LBRACE, literal }}
            '}' => { Token { token_type: TokenType::RBRACE, literal }}
            '(' => { Token { token_type: TokenType::LPAREN, literal }}
            ')' => { Token { token_type: TokenType::RPAREN, literal }}
            ';' => { Token { token_type: TokenType::SEMICOLON, literal }}
            ',' => { Token { token_type: TokenType::COMMA, literal }}
            '+' => { Token { token_type: TokenType::PLUS, literal }}
            '-' => { Token { token_type: TokenType::MINUS, literal }}
            _ => { Token { token_type: TokenType::EOF, literal: "".to_string()} }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token<'a>() {
        let source = "=+(){},;";

        let lexer_cases = [
            Token { token_type: TokenType::ASSIGN, literal: "=".to_string() },
            Token { token_type: TokenType::PLUS, literal: "+".to_string() },
            Token { token_type: TokenType::LPAREN, literal: "(".to_string() },
            Token { token_type: TokenType::RPAREN, literal: ")".to_string() },
            Token { token_type: TokenType::LBRACE, literal: "{".to_string() },
            Token { token_type: TokenType::RBRACE, literal: "}".to_string() },
            Token { token_type: TokenType::COMMA, literal: ",".to_string() },       
            Token { token_type: TokenType::SEMICOLON, literal: ";".to_string() },       
        ];

        let mut lexer = Lexer::new(source);

        for (index, _) in source.char_indices() {
            assert_eq!(lexer.next_token(), lexer_cases[index]);
        }
    }
}


