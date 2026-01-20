use std::fmt;

// Statement ------------------------------------------------------------------------------
#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
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
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "let {} = {}", self.name, self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "return {}", self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
	pub expression: Expression
}

impl fmt::Display for ExpressionStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.expression)
	}
}

// Expression -----------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum Expression {
	IfExpression(Box<IfExpression>),
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
	Boolean(Boolean),
	Prefix(Box<Prefix>),
	Infix(Box<Infix>),
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::IfExpression(ifx) => write!(f, "{}", ifx),
			Expression::Identifier(ident) => write!(f, "{}", ident),
			Expression::IntegerLiteral(il) => write!(f, "{}", il),
			Expression::Boolean(b) => write!(f, "{}", b),
			Expression::Prefix(pe) => write!(f, "{}", pe),
			Expression::Infix(inf) => write!(f, "{}", inf),
		}
	}
}

#[derive(Debug, PartialEq)]
// variable name of let x = 5, x is represented by the token.
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct Boolean {
	pub value: bool,
}

impl fmt::Display for Boolean {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
	pub condition: Expression,
	pub consequence: BlockStatement,
	pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}{:?}", self.condition, self.consequence, self.alternative)
	}
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
	pub statements: Vec<Statement>,
}
		
impl fmt::Display for BlockStatement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self.statements)
	}
}

#[derive(Debug, PartialEq)]
pub struct Prefix {
	pub operator: String,
	pub rhs: Expression,
}

impl fmt::Display for Prefix {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.operator, self.rhs)
	}
}

#[derive(Debug, PartialEq)]
pub struct Infix {
	pub lhs: Expression,
	pub operator: String,
	pub rhs: Expression,
}

impl fmt::Display for Infix {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}{}", self.lhs, self.operator, self.rhs)
	}
}

// Program -----------------------------------------------------------------------------

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
