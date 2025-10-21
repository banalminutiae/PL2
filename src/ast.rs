use crate::token::{Token};

pub trait Node {
	fn token_literal(&self) -> &str;
}

#[derive(Debug)]
pub enum Statement {
	Let(LetStatement),
	Return(ReturnStatement),
	Expression(ExpressionStatement),
}

impl Node for Statement {
	fn token_literal(&self) -> &str {
		match self {
			Statement::Let(ls) => ls.token_literal(),
			Statement::Return(rt) => rt.token_literal(),
			Statement::Expression(exp) => exp.token_literal(),
		}
	}
}

#[derive(Debug)]
pub enum Expression {
	// Anything that resolves to a value. Bools, len(), string literals etc.
	Identifier(Identifier),
	IntegerLiteral(Identifier),
}

impl Node for Expression {
	fn token_literal(&self) -> &str {
		match self {
			Expression::Identifier(ident) => &ident.value,
			Expression::IntegerLiteral(_) => "int",
		}
	}
}

#[derive(Debug)]
// variable name of let x = 5, x is represented by the token.
pub struct Identifier {
	pub token: Token,
	pub value: String,
}

#[derive(Debug)]
pub struct IntegerLiteral {
	pub value: i64,
}

#[derive(Debug)]
// Whole statement e.g. let x = 5;
pub struct LetStatement {
	pub token: Token,
	pub name: Identifier,
	// pub value: Expression,
}

impl LetStatement {
	pub fn token_literal(&self) -> &str {
		"let"
	}
}	

#[derive(Debug)]
pub struct ReturnStatement {
	pub token: Token,
	// pub value: Expression,
}

impl ReturnStatement {
	pub fn token_literal(&self) -> &str {
		"return"
	}
}

#[derive(Debug)]
struct ExpressionStatement {
	pub expression: Expression,
}

impl ExpressionStatement {
    pub fn token_literal(&self) -> &str {
		self.expression.token_literal()
	}
}

#[derive(Debug)]
pub struct Program {
	pub statements: Vec<Statement>,
}

impl Node for Program {
	fn token_literal(&self) -> &str {
		if self.statements.len() > 0 {
			self.statements[0].token_literal()
		} else {
			""
		}
	}
}
