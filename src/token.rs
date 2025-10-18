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

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    IDENTIFIER,
    LITERAL,
    ILLEGAL,
    INTEGER,
    ASSIGN,
    EQUALS,
	NOT_EQUALS,
    PLUS,
    MINUS,
	EXCLAMATION,
	ASTERISK,
	LT,
	GT,
	FSLASH,
	COMMENT,
	PIPE,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
	TRUE,
	FALSE,
	IF,
	ELSE,
	RETURN,
    EOF,
}

static KEYWORDS: &'static [(&'static str, TokenType)] = &[
	("fn", TokenType::FUNCTION),
	("let", TokenType::LET),
	("true", TokenType::TRUE),
	("false", TokenType::FALSE),
	("if", TokenType::IF),
	("else", TokenType::ELSE),
	("return", TokenType::RETURN),
];

pub fn lookup_identifier(literal: &str) -> TokenType {
	if let Some((_, value)) = KEYWORDS.iter().find(|(k, _)| *k == literal) {
		value.clone()
	} else {
		TokenType::IDENTIFIER
	}
}
