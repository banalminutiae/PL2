use crate::token::Token;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> &str;
}

// Statement ------------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
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

impl fmt::Display for Statement {
	fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Statement::Let(lt) => write!(f, "{}", lt),
			Statement::Return(rt) => write!(f, "{}", rt),
			Statement::Expression(exp) => write!(f, "{}", exp),
		}
	}
}

#[derive(Debug, PartialEq)]
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

impl fmt::Display for LetStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "let {} = {}", self.name, self.name)
	}
}
	
#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    // pub value: Expression,
}

impl ReturnStatement {
    pub fn token_literal(&self) -> &str {
        "return"
    }
}

impl fmt::Display for ReturnStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "return {}", "return")
	}
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
	pub token: Token,
	pub expression: Expression
}

impl ExpressionStatement {
	pub fn token_literal(&self) -> &str {
		&self.token.literal
	}
}

impl fmt::Display for ExpressionStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.expression.token_literal())
	}
}

// Expression -----------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Anything that resolves to a value. Bools, len(), string literals etc.
    // Ifs and loops can go here because that'd be coo
    // function calls, prefix and postifx operators, and comparisons are also expressions
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(ident) => &ident.value,
            Expression::IntegerLiteral(_) => "int",
        }
    }
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::Identifier(ident) => write!(f, "{}", ident),
			Expression::IntegerLiteral(il) => write!(f, "{}", il), 
		}
	}
}

#[derive(Debug, PartialEq)]
// variable name of let x = 5, x is represented by the token.
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Identifier {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
	pub token: Token,
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

// Program -----------------------------------------------------------------------------

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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
