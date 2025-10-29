#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token { token_type, literal }
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    IDENTIFIER,
    ILLEGAL,
    INTEGER,
    ASSIGN,
    EQUALS,
	NOT_EQUALS,
    PLUS,
	PLUS_ASSIGN,
    MINUS,
	MINUS_ASSIGN,
	EXCLAMATION,
	ASTERISK,
	MULTIPLY_ASSIGN,

	LT,
	LTEQ,
	GT,
	GTEQ,

	DIVIDE,
	COMMENT,
	DIVIDE_ASSIGN,
	
	PIPE,
	OR,
	AMP,
	AND,
    COMMA,
	CARET,
	XOR,
	TILDE,
	MOD,
	COLON,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
	LSQ_BRACKET,
	RSQ_BRACKET,

	// keywords
	FUNCTION,
    LET,
	TRUE,
	FALSE,
	IF,
	ELSE,
	WHILE,
	FOR,
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
	("while", TokenType::WHILE),
	("for", TokenType::FOR),
	("return", TokenType::RETURN),
];

pub fn lookup_identifier(literal: &str) -> TokenType {
	if let Some((_, value)) = KEYWORDS.iter().find(|(k, _)| *k == literal) {
		value.clone()
	} else {
		TokenType::IDENTIFIER
	}
}
