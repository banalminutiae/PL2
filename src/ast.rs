use crate::token::{Token};

pub trait Node {
	fn token_literal(&self) -> &str;
}

pub enum Statement {
	Let(LetStatement),
	Return(ReturnStatement),
	Expression(ExpressionStatement),
}

pub enum Expression {
	Identifier(Identifier),
	IntegerLiteral(Identifier),
}

pub struct LetStatement {
	pub token: Token,
	pub name: Identifier,
	pub value: Expression,
}
	

struct Program {
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
