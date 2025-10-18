#[derive(PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token { token_type, literal }
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    IDENTIFIER,
    LITERAL,
    ILLEGAL,
    INT,
    ASSIGN,
    EQUALS,
    PLUS,
    MINUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    EOF,
}

