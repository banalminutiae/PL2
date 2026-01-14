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
    Identifier,
    Illegal,
    Integer,
    Equals,
    Equals_Equals,
	Not_Equals,
    Plus,
	Plus_Equals,
    Minus,
	Minus_Equals,
	Exclamation,
	Asterisk,
	Multiply_Equals,

	Lt,
	Lteq,
	Gt,
	Gteq,

	Slash,
	Slash_Equals,
	
	Pipe,
	Or,
	Amp,
	And,
    Comma,
	Caret,
	Caret_Equals,
	Tilde,
	Percent,
	Percent_Equals,
	Colon,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
	Lsq_Bracket,
	Rsq_Bracket,

	// keywords
	Function,
    Let,
	True,
	False,
	If,
	Else,
	While,
	For,
	Return,
    EOF,
}

static KEYWORDS: &[(&str, TokenType)] = &[
	("fn", TokenType::Function),
	("let", TokenType::Let),
	("true", TokenType::True),
	("false", TokenType::False),
	("if", TokenType::If),
	("else", TokenType::Else),
	("while", TokenType::While),
	("for", TokenType::For),
	("return", TokenType::Return),
];

pub fn lookup_identifier(literal: &str) -> TokenType {
	if let Some((_, value)) = KEYWORDS.iter().find(|(k, _)| *k == literal) {
		value.clone()
	} else {
		TokenType::Identifier
	}
}
